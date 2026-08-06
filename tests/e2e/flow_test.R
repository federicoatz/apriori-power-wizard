## tests/e2e/flow_test.R
## -----------------------------------------------------------------------
## End-to-end flow test: drives the running Shiny app through the full
## four-step wizard in a real headless Chrome (chromote) and asserts that
## what the RESULTS STEP DISPLAYS matches what the solvers COMPUTE.
##
## Why this exists when 1,900+ unit tests already pass: the unit suite
## exercises R/ (the math) and the pure parts of modules/, but nothing in
## it renders a page -- so a refactor of common_results.R could break the
## multiplicity banner, the value boxes, or the report text while every
## unit test stays green (this is exactly how the v1.1.0 feature had to be
## verified by hand before release). The deploy workflow's smoke test only
## checks that the app BOOTS. This file closes the gap in between.
##
## What it asserts, against a plain `shiny::runApp()` server it spawns
## itself (the wasm build's boot path is the smoke test's job, and the
## server-side reactive wiring under test here is identical in both):
##
##   Scenario 1 (two_means): with 3 planned comparisons, the multiplicity
##     banner shows exactly the uncorrected and corrected N the family's
##     own solver produces; switching Bonferroni -> Sidak updates the
##     banner reactively (no re-Compute) to the Sidak figures; the report
##     text discloses the method by name.
##   Scenario 2 (wilcoxon): entering the safeguard branch via a published
##     z statistic produces the N implied by z -> P(X<Y) ->
##     safeguard_ci_auc() -> power_wilcoxon_n(), and the report text
##     records that the interval was built on the P(X<Y) scale.
##
## Usage:  Rscript tests/e2e/flow_test.R [port]
## Exits nonzero on the first failed assertion, printing what differed.
## Requires: chromote (+ a Chrome the CHROMOTE_CHROME env var can point
## to), processx; the app's own dependencies for the spawned server.
## -----------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1) as.integer(args[[1]]) else 7801L

## Expected values, computed from the app's own solvers. This is the right
## oracle for a FLOW test: the math itself is pinned by the unit suite
## (against pwr and published references), so what is being asserted here
## is purely that the UI displays what the solver returns, with no drift.
source("global.R")

expect <- new.env()
expect$k <- 3
expect$n_uncorrected <- power_two_means_n(0.3, 0.05, 0.80)$n_total
expect$n_bonferroni <- power_two_means_n(0.3, 0.05 / 3, 0.80)$n_total
expect$alpha_sidak <- 1 - (1 - 0.05)^(1 / 3)
expect$n_sidak <- power_two_means_n(0.3, expect$alpha_sidak, 0.80)$n_total

expect$wmw_z <- 2.34
expect$wmw_n_pub <- 80
p_pub <- z_to_p_superiority(expect$wmw_z, 40, 40)
sg <- safeguard_ci_auc(p_pub, 40, 40, conf_level = 0.80, one_sided = TRUE)
expect$wmw_p_safe <- if (sg$p_safeguard < 0.5) 1 - sg$p_safeguard else sg$p_safeguard
expect$wmw_n <- power_wilcoxon_n(expect$wmw_p_safe, 0.05, 0.80)$n_total

## ---- Spawn the app server and wait for it ------------------------------
message("Starting Shiny app on port ", port, " ...")
server <- processx::process$new(
  "Rscript", c("-e", sprintf("shiny::runApp('.', port = %d, host = '127.0.0.1')", port)),
  stdout = "|", stderr = "2>&1"
)
on.exit(try(server$kill(), silent = TRUE), add = TRUE)

ready <- FALSE
for (i in 1:120) {
  Sys.sleep(0.5)
  if (!server$is_alive()) break
  html <- tryCatch(suppressWarnings(paste(readLines(
    sprintf("http://127.0.0.1:%d/", port), warn = FALSE), collapse = "")),
    error = function(e) "")
  if (grepl("analysis_choice", html, fixed = TRUE)) { ready <- TRUE; break }
}
if (!ready) {
  cat(server$read_output())
  stop("App server did not become ready within 60s")
}
message("Server ready.")

## ---- Browser helpers ----------------------------------------------------
library(chromote)
sess <- ChromoteSession$new()
on.exit(try(sess$close(), silent = TRUE), add = TRUE)

ev <- function(js) tryCatch(sess$Runtime$evaluate(js)$result$value, error = function(e) NA)

## Set a numeric input the way a user would, so Shiny sees the change.
setnum <- function(id, val) ev(sprintf(
  "(function(){var e=document.getElementById('%s'); if(!e)return false;
    var s=Object.getOwnPropertyDescriptor(window.HTMLInputElement.prototype,'value').set;
    s.call(e,'%s');
    e.dispatchEvent(new Event('input',{bubbles:true}));
    e.dispatchEvent(new Event('change',{bubbles:true})); return true;})()", id, val))

## Click a radio option by its value inside a Shiny radio group.
setradio <- function(group_id, value) ev(sprintf(
  "(function(){var r=document.querySelectorAll('#%s input[type=radio]');
    for(var i=0;i<r.length;i++){if(r[i].value=='%s'){r[i].click();return true;}}
    return false;})()", group_id, value))

## Click the LAST visible button/link whose trimmed text equals `t`
## (each wizard step has its own Back/Next pair; the last one in DOM order
## is the one on the most recently shown step).
clickTxt <- function(t) ev(sprintf(
  "(function(){var b=[].slice.call(document.querySelectorAll('button,a'))
     .filter(function(x){return x.innerText.trim()=='%s' && x.offsetParent!==null;});
   if(b.length){b[b.length-1].click();return true;} return false;})()", t))

## innerText by EXACT element id. Family modules stay mounted in the DOM
## after a visit, so suffix-matching ('[id$=...]') would silently read the
## FIRST family's output while asserting about the second -- ids here are
## always written out in full, namespace included.
txt <- function(id) {
  v <- ev(sprintf(
    "(function(){var e=document.getElementById('%s');
      return e?e.innerText.trim():'';})()", id))
  if (is.null(v) || length(v) == 0 || is.na(v)) "" else gsub("\\s+", " ", v)
}

failures <- 0L
check <- function(label, ok, detail = "") {
  status <- if (isTRUE(ok)) "ok " else { failures <<- failures + 1L; "FAIL" }
  cat(sprintf("[%s] %s%s\n", status, label,
              if (!isTRUE(ok) && nzchar(detail)) paste0(" -- ", detail) else ""))
}
## Both banner figures are asserted VERBATIM (comma-grouped, as rendered).
fmt <- function(n) format(n, big.mark = ",")

## ---- Scenario 1: two_means, multiplicity banner, Bonferroni -> Sidak ----
invisible(sess$Page$navigate(sprintf("http://127.0.0.1:%d/", port), wait_ = TRUE))
Sys.sleep(4)

## the Step-1 radios appear once Shiny has connected; retry briefly
for (i in 1:20) {
  if (isTRUE(setradio("analysis_choice", "two_means"))) break
  Sys.sleep(0.5)
}
Sys.sleep(1)
clickTxt("Start"); Sys.sleep(2.5)              # -> design step
clickTxt("Next"); Sys.sleep(1.5)               # -> parameters step
setnum("two_means-n_comparisons", "3"); Sys.sleep(1.5)
clickTxt("Next"); Sys.sleep(1.5)               # -> effect size (SESOI d = 0.3 default)
clickTxt("Compute"); Sys.sleep(5)              # -> results

banner <- txt("two_means-multiplicity_note")
check("banner is present with k = 3",
      grepl("3 comparisons", banner), substr(banner, 1, 120))
check(sprintf("banner shows the solver's uncorrected N (%s)", fmt(expect$n_uncorrected)),
      grepl(sprintf("from %s to", fmt(expect$n_uncorrected)), banner), banner)
check(sprintf("banner shows the solver's Bonferroni N (%s)", fmt(expect$n_bonferroni)),
      grepl(sprintf("to %s", fmt(expect$n_bonferroni)), banner), banner)
check("banner names Bonferroni", grepl("Bonferroni", banner), banner)

report <- txt("two_means-report_text")
check("report discloses Bonferroni by name", grepl("Bonferroni-corrected", report),
      substr(report, 1, 160))
check("report states what the correction cost",
      grepl(sprintf("would have required N = %s", fmt(expect$n_uncorrected)), report),
      substr(report, 1, 300))

## Switch to Sidak; every figure must update reactively, with no re-Compute.
setradio("two_means-mc_method", "sidak"); Sys.sleep(3)
banner2 <- txt("two_means-multiplicity_note")
check("after switching, banner names Sidak", grepl("Šidák", banner2), banner2)
check(sprintf("banner shows the Sidak per-test alpha (%s)", format_stat(expect$alpha_sidak, 4)),
      grepl(format_stat(expect$alpha_sidak, 4), banner2, fixed = TRUE), banner2)
check(sprintf("banner shows the Sidak-corrected N (%s)", fmt(expect$n_sidak)),
      grepl(sprintf("to %s", fmt(expect$n_sidak)), banner2), banner2)
report2 <- txt("two_means-report_text")
check("report now discloses Sidak", grepl("Sidak-corrected", report2),
      substr(report2, 1, 160))

## ---- Scenario 2: wilcoxon, safeguard from a published z, AUC scale -----
clickTxt("Change analysis type"); Sys.sleep(1.5)
setradio("analysis_choice", "wilcoxon"); Sys.sleep(1)
clickTxt("Start"); Sys.sleep(2.5)
clickTxt("Next"); Sys.sleep(1.5)               # design -> parameters
clickTxt("Next"); Sys.sleep(1.5)               # parameters -> effect size
setradio("wilcoxon-es_branch", "safeguard"); Sys.sleep(1.5)
setradio("wilcoxon-sg_metric", "z"); Sys.sleep(1)
setnum("wilcoxon-sg_published_value", as.character(expect$wmw_z)); Sys.sleep(1)
setnum("wilcoxon-sg_published_n", as.character(expect$wmw_n_pub)); Sys.sleep(2)

## read the effect-size summary BEFORE Compute: the wizard's steps are a
## hidden-type tabsetPanel, and innerText of the (hidden) effect-size tab
## collapses to "" once the results tab is showing
psum <- txt("wilcoxon-psup_summary")
check("effect summary is present and shows the planned P(X < Y)",
      grepl("Planning for P\\(X < Y\\)", psum), psum)
check("no normality warning when arriving via z (distribution-free route)",
      !grepl("assumes the outcome is normally", psum), psum)

clickTxt("Compute"); Sys.sleep(5)

wreport <- txt("wilcoxon-report_text")
check(sprintf("wilcoxon report shows the AUC-route N (N = %d)", expect$wmw_n),
      grepl(sprintf("N = %d", expect$wmw_n), wreport), substr(wreport, 1, 300))
check("wilcoxon report records the interval was built on the P(X<Y) scale",
      grepl("Hanley & McNeil", wreport), substr(wreport, 1, 300))
check("wilcoxon report shows the z it started from",
      grepl(sprintf("z = %.2f", expect$wmw_z), wreport), substr(wreport, 1, 300))

## ---- Verdict ------------------------------------------------------------
if (failures > 0L) {
  cat(sprintf("\n%d flow assertion(s) FAILED\n", failures))
  quit(status = 1)
}
cat("\nAll flow assertions passed.\n")
