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
##   Scenario 3 (clustered continuous + clustered categorical): the study
##     plan. This is the only feature in the app that spans two family
##     modules, so it is the only one a unit test structurally cannot
##     reach -- R/study_plan.R pins the arithmetic, but the wiring that
##     carries an entry from one module's session into another's panel
##     exists only at runtime. Asserts that nothing appears before the
##     user opts in, that the combined requirement is the LARGEST of two
##     analyses on one sample rather than their sum, that the non-binding
##     analysis is told what powering on it alone would miss, that the
##     figure reaches the report text, and that unticking removes the row.
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

## ---- Scenario 3: the cross-family study plan ----------------------------
## The one feature in this app whose whole point is that it spans two
## family modules, so a unit test on R/study_plan.R cannot reach it: the
## aggregation is pure and pinned there, but the wiring that carries an
## entry from one module's session into another's panel is only exercised
## here. Specifically at risk and only visible in a browser -- that
## session$userData is genuinely shared between two module sessions, that
## a second family's tick re-renders the FIRST family's panel, and that
## unticking removes the row rather than leaving a stale number behind.
##
## The two families used are the ones from the paper's walkthrough, which
## is also the case that motivated the feature: a continuous primary
## outcome and a categorical secondary one on the same participants, where
## the secondary measure binds and powering on the primary alone
## under-recruits.
expect$sp_cont <- power_clustered_n(d = 0.5, icc = 0.05, cluster_size = 4)$n_total
expect$sp_cat <- power_clustered_cat_n(w = 0.185, df = 3, icc = 0.05, cluster_size = 4)$n_total
expect$sp_total <- max(expect$sp_cont, expect$sp_cat)
expect$sp_short <- expect$sp_total - min(expect$sp_cont, expect$sp_cat)

## Family 1: clustered continuous. Defaults for d/ICC/cluster size are set
## explicitly so the expectations above are not hostage to a default change.
clickTxt("Change analysis type"); Sys.sleep(1.5)
setradio("analysis_choice", "clustered_rct"); Sys.sleep(1)
clickTxt("Start"); Sys.sleep(2.5)
setnum("clustered_rct-cluster_size", "4"); Sys.sleep(1)
setnum("clustered_rct-icc", "0.05"); Sys.sleep(1)
clickTxt("Next"); Sys.sleep(1.5)               # design -> parameters
clickTxt("Next"); Sys.sleep(1.5)               # parameters -> effect size
setnum("clustered_rct-sesoi_d", "0.5"); Sys.sleep(1.5)
clickTxt("Compute"); Sys.sleep(5)

## Nothing may appear before the box is ticked: someone pricing unrelated
## studies must never be shown a combined total.
check("study plan panel is absent until opted in",
      !nzchar(txt("clustered_rct-study_plan_panel")),
      txt("clustered_rct-study_plan_panel"))

ev("(function(){var e=document.getElementById('clustered_rct-in_study_plan');
     if(!e)return false; e.click(); return true;})()"); Sys.sleep(3)
panel1 <- txt("clustered_rct-study_plan_panel")
check("with one analysis, the panel asks for a second rather than showing a total",
      grepl("at least two", panel1), panel1)

## Family 2: clustered categorical, same sample -- reached through the
## study-plan card's own button rather than the header link, since that is
## the path a user building a plan actually takes. It also exercises two
## things nothing else does: that the button is wired at all, and that the
## plan survives leaving the family (it lives in session$userData, not in
## the module).
check("study-plan card offers a way to add the next analysis",
      isTRUE(ev("(function(){var b=document.getElementById('clustered_rct-plan_next_analysis'); return !!b && b.offsetParent!==null;})()")),
      "button missing or hidden")
ev("document.getElementById('clustered_rct-plan_next_analysis').click()"); Sys.sleep(2.5)

## Landing on Step 1 with no sign the plan survived would be worse than
## the extra scrolling this button removes.
banner <- txt("plan_in_progress")
check("Step 1 says a plan is in progress after using that button",
      grepl("Study plan in progress: 1 analysis", banner), banner)
check("Step 1 banner names the family rather than its key",
      grepl("Clustered: sessions / matching groups", banner, fixed = TRUE) &&
        !grepl("clustered_rct", banner), banner)

setradio("analysis_choice", "clustered_cat"); Sys.sleep(1)
clickTxt("Start"); Sys.sleep(2.5)
setradio("clustered_cat-outcome_type", "categorical"); Sys.sleep(1.5)
## df is derived, not entered: (categories - 1) * (arms - 1), so 4 and 2
## give the df = 3 the expectation above was computed at.
setnum("clustered_cat-n_categories", "4"); Sys.sleep(1)
setnum("clustered_cat-n_arms", "2"); Sys.sleep(1)
setnum("clustered_cat-cluster_size", "4"); Sys.sleep(1)
setnum("clustered_cat-icc", "0.05"); Sys.sleep(1)
clickTxt("Next"); Sys.sleep(1.5)
clickTxt("Next"); Sys.sleep(1.5)
setnum("clustered_cat-sesoi_w", "0.185"); Sys.sleep(1.5)
clickTxt("Compute"); Sys.sleep(5)

ev("(function(){var e=document.getElementById('clustered_cat-in_study_plan');
     if(!e)return false; e.click(); return true;})()"); Sys.sleep(3)

panel2 <- txt("clustered_cat-study_plan_panel")
check(sprintf("second family's panel shows the combined requirement (%s)", fmt(expect$sp_total)),
      grepl(sprintf("Recruit %s participants", fmt(expect$sp_total)), panel2), panel2)

## Families are named, not keyed. This is asserted because it silently
## regressed once: `family_titles` lived at the top level of app.R, which
## runApp() evaluates INWARD of the global environment, while
## modules/common_results.R is sourced straight INTO it -- so the lookup
## could never resolve and quietly fell back to the raw key. The panel,
## the Step-1 banner and the generated Method text all read "two_means".
## Nothing caught it because every existing assertion checked numbers.
for (nm in c("Clustered: sessions / matching groups",
             "Clustered: binary or categorical outcome")) {
  check(sprintf("plan names the family in prose (%s)", nm),
        grepl(nm, panel2, fixed = TRUE), panel2)
}
check("plan never shows a raw family key",
      !grepl("clustered_rct|clustered_cat", panel2), panel2)
check("panel states the max rule rather than summing",
      grepl("largest requirement among them, not their sum", panel2), panel2)

## The binding analysis must NOT be warned about a shortfall it does not
## have -- here the categorical secondary measure is the one that binds,
## which is the walkthrough's point: the primary hypothesis is not what
## sizes the study.
check("the binding analysis shows no shortfall warning",
      !grepl("too few", panel2), panel2)

## The plan reaches the manuscript text, not just the screen.
sreport <- txt("clustered_cat-report_text")
check("report text carries the combined requirement",
      grepl("largest of them rather than their sum", sreport), substr(sreport, 1, 400))
## The Method text is the one output that ends up in somebody's manuscript,
## so a raw key leaking here is worse than on screen.
check("report text names the binding family rather than its key",
      grepl("set by Clustered: binary or categorical outcome", sreport, fixed = TRUE) &&
        !grepl("clustered_cat", sreport), substr(sreport, 1, 500))

## The cross-module assertion this scenario exists for. Note the revisit:
## a family's tab stays mounted but HIDDEN once you move to another, and
## Shiny suspends outputs on hidden tabs (suspendWhenHidden defaults to
## TRUE), so the first family's panel is not re-rendered while the second
## is on screen. That is the right behaviour -- there is nothing to see --
## and it resumes on return, which is what a user actually experiences and
## what is asserted here. Reading the hidden output directly would test
## Shiny's suspension policy, not this feature.
clickTxt("Change analysis type"); Sys.sleep(1.5)
setradio("analysis_choice", "clustered_rct"); Sys.sleep(1)
clickTxt("Start"); Sys.sleep(3)

panel1b <- txt("clustered_rct-study_plan_panel")
check("returning to the first family shows the plan built in the second",
      grepl(sprintf("Recruit %s participants", fmt(expect$sp_total)), panel1b), panel1b)
check(sprintf("first family is told what powering on it alone would miss (%s)", fmt(expect$sp_short)),
      grepl(sprintf("recruit %s too few", fmt(expect$sp_short)), panel1b), panel1b)

## Unticking must remove the row, not leave a stale number behind in the
## other family's panel.
clickTxt("Change analysis type"); Sys.sleep(1.5)
setradio("analysis_choice", "clustered_cat"); Sys.sleep(1)
clickTxt("Start"); Sys.sleep(3)
ev("(function(){var e=document.getElementById('clustered_cat-in_study_plan');
     if(!e)return false; e.click(); return true;})()"); Sys.sleep(3)

clickTxt("Change analysis type"); Sys.sleep(1.5)
setradio("analysis_choice", "clustered_rct"); Sys.sleep(1)
clickTxt("Start"); Sys.sleep(3)
panel1c <- txt("clustered_rct-study_plan_panel")
check("unticking the second family removes its row from the first family's panel",
      grepl("at least two", panel1c), panel1c)

## ---- Scenario 4: colour contrast, both themes ---------------------------
## Added after a class of bug that no existing check could see. The app
## themes itself with its own tokens under [data-theme] and does NOT use
## Bootstrap's data-bs-theme, so every Bootstrap component the stylesheet
## does not explicitly restyle kept reading `--bs-*` at its LIGHT values
## once the app went dark: an inline <code> came out pure black on
## #0B0F1A (contrast 1.10, invisible) and every helpText() at 1.04. The
## unit suite cannot see this -- it renders no pages -- and the axe-core
## audit runs only against the deployed shinylive build, on the landing
## page, outside this suite.
##
## Two details matter for this to mean anything. Contrast is measured on
## the ACTUAL rendered foreground, compositing any inherited `opacity`
## into it: reading `color` alone reported the value-box labels as a pass
## at 3.93:1 when white at opacity .75 over the tile is really 2.87:1.
## And the background is resolved by walking up to the first non-
## transparent ancestor, since most text sits on a transparent element.
##
## Thresholds are WCAG AA: 4.5:1 for normal text, 3:1 for large (>=24px,
## or >=18.66px bold).
contrast_scan <- '
(function(){
  function lum(c){var m=c.match(/[\\d.]+/g); if(!m) return null;
    if (m.length>3 && parseFloat(m[3])===0) return null;
    var f=[m[0],m[1],m[2]].map(function(v){v=v/255;
      return v<=0.03928 ? v/12.92 : Math.pow((v+0.055)/1.055,2.4);});
    return 0.2126*f[0]+0.7152*f[1]+0.0722*f[2];}
  function bgOf(el){while(el&&el!==document.documentElement){
      var b=getComputedStyle(el).backgroundColor, m=b.match(/[\\d.]+/g);
      if(m&&(m.length<4||parseFloat(m[3])>0.5)) return b; el=el.parentElement;}
    return getComputedStyle(document.documentElement).backgroundColor;}
  var out=[];
  document.querySelectorAll("*").forEach(function(el){
    if(!el.offsetParent && el.tagName!=="BODY") return;
    var t=Array.from(el.childNodes).filter(function(n){
        return n.nodeType===3 && n.textContent.trim();})
      .map(function(n){return n.textContent.trim();}).join(" ");
    if(!t) return;
    var cs=getComputedStyle(el);
    var op=1, a=el;
    while(a && a!==document.documentElement){
      op*=parseFloat(getComputedStyle(a).opacity||1); a=a.parentElement;}
    var bgc=bgOf(el);
    var fm=(cs.color.match(/[\\d.]+/g)||[]).map(Number);
    var bm=(bgc.match(/[\\d.]+/g)||[]).map(Number);
    if(fm.length<3||bm.length<3) return;
    var eff="rgb("+[0,1,2].map(function(i){return op*fm[i]+(1-op)*bm[i];}).join(",")+")";
    var lf=lum(eff), lb=lum(bgc); if(lf===null||lb===null) return;
    var r=(Math.max(lf,lb)+0.05)/(Math.min(lf,lb)+0.05);
    var size=parseFloat(cs.fontSize), bold=parseInt(cs.fontWeight)>=700;
    var need=((size>=24)||(size>=18.66&&bold))?3:4.5;
    if(r<need) out.push(el.tagName+"."+(el.className||"").toString().split(" ")[0]+
                        " ratio="+r.toFixed(2)+" need="+need+" text=\\""+t.substr(0,40)+"\\"");
  });
  return out.join(" | ");})()'

theme_of <- function() ev("String(document.documentElement.getAttribute('data-theme'))")
set_theme <- function(mode) {
  ev(sprintf("document.getElementById('pw-theme-%s').click()", mode)); Sys.sleep(2)
}

for (mode in c("dark", "light")) {
  set_theme(mode)
  check(sprintf("theme switched to %s", mode), identical(theme_of(), mode), theme_of())
  fails <- ev(contrast_scan)
  if (is.null(fails) || is.na(fails)) fails <- "SCAN FAILED"
  check(sprintf("%s theme: every text node meets WCAG AA contrast", mode),
        identical(fails, ""), fails)
}
set_theme("dark")

## ---- Verdict ------------------------------------------------------------
if (failures > 0L) {
  cat(sprintf("\n%d flow assertion(s) FAILED\n", failures))
  quit(status = 1)
}
cat("\nAll flow assertions passed.\n")
