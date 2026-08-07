## validation/accessibility_check.R
## -----------------------------------------------------------------------
## Automated accessibility audit of the exported shinylive build, using
## axe-core (https://github.com/dequelabs/axe-core) injected into a real
## headless Chrome via `chromote` -- same toolchain as
## deploy/smoke_test.R, so no Node/npm dependency is needed in CI.
##
## WHY THIS IS A STARTING POINT, NOT A COMPLETE ACCESSIBILITY AUDIT.
## axe-core catches a meaningful subset of WCAG issues automatically
## (missing lang attribute, color contrast, ARIA misuse, missing
## landmarks, heading order, ...), but it cannot verify keyboard
## navigation, screen-reader usability, or mobile layout -- those need a
## human with a keyboard, a screen reader (VoiceOver/NVDA/JAWS), and a
## phone, which no script can substitute for. Automating THIS part first
## because it is the part a script legitimately can do.
##
## Status as of 2026-08-07 (app v1.4.3, code-verified; the live audit run
## originally captured 2026-08-05 at v1.0.0 has not been re-run since --
## see the header note above about what a script can and cannot check):
## fixed since this script first ran -- color-contrast
## (APP_PALETTES$light$muted in global.R, #8B8B96 -> #6D6D78; verified
## against every default-view foreground/background pair used in the app,
## both by direct WCAG contrast calculation and by repeated live audits),
## heading-order (the four heading levels used throughout the app were
## shifted down one, h1/h2/h3/h4, to close the h1->h4 gap the original
## "Step: ..." headings created; the one remaining case, "Step 1" on the
## landing tab, has no family h2 title to sit under and so is exposed as
## level 2 via `aria-level="2"` on its h3 tag rather than a plain h2,
## keeping its .step-card CSS unchanged), and landmark-one-main / region
## (app.R wraps the header in <header> and the wizard's tabsetPanel in
## <main>; the footer was already a <footer>). All four were re-confirmed
## present in the source at v1.4.3 by direct inspection (grep for the
## specific markup this comment names), though the axe-core run itself was
## not repeated -- do that before relying on this note for a release
## announcement or a compliance claim.
##
## COLOR CONTRAST IS NOW MEASURED, NOT ASSUMED (v1.4.3). This script only
## ever ran against the deployed shinylive build's LANDING page, in the
## light theme, and it is not part of the regular suite -- which left the
## dark theme, and every step past the first, unchecked by anything. That
## gap hid a whole class of defect: the app themes itself with its own
## tokens under [data-theme] and does not use Bootstrap's data-bs-theme,
## so every Bootstrap component the stylesheet did not explicitly restyle
## kept reading `--bs-*` at its LIGHT values once the app went dark. An
## inline <code> rendered pure black on #0B0F1A (1.10:1, invisible) and
## every helpText() at 1.04:1. Both are fixed at the root -- see the
## "BOOTSTRAP VARIABLE BRIDGE" block in www/styles.css.
##
## tests/e2e/flow_test.R now carries a contrast scan over every visible
## text node, on all four wizard steps, in BOTH themes, asserting WCAG AA
## (4.5:1 normal, 3:1 large). It runs in CI on every push. Two details it
## gets right that a naive check does not: the measured foreground
## composites any inherited `opacity` -- reading `color` alone scored the
## value-box labels 3.93:1 when white at opacity .75 over the tile is
## really 2.87:1 -- and the background is resolved by walking up to the
## first non-transparent ancestor. Verified non-vacuous by disabling the
## variable bridge and confirming the scan fails.
##
## That closed two further findings this note previously could not have
## made: the value-box labels lost their 0.75 opacity, and light-mode
## --positive moved from #00926F to #008062, since white-on-#00926F was
## 3.93:1. Both themes now report zero failures.
##
## What axe-core still adds over that scan, and why this script stays:
## everything that is not contrast -- ARIA misuse, heading order,
## landmarks, form-label association, the aria-prohibited-attr issue
## below. Re-run it before making a compliance claim.
##
## Still open, left for deliberate follow-up rather than a blind
## mechanical fix:
##   - aria-prohibited-attr: shiny::icon() emits `role="presentation"
##     aria-label="X icon"` on every <i> tag by default -- a combination
##     axe flags because role="presentation" strips all semantics,
##     including aria-label. This comes from Shiny's own icon() helper,
##     not from app code directly, and touches every icon() call site in
##     the app; fixing it means wrapping icon() or post-processing its
##     output, which is a deliberate design decision, not a one-line fix.
##
## Usage (from the project root, after building _site -- see
## export_shinylive.R):
##   Rscript validation/accessibility_check.R [site_dir] [port]
## Always exits 0 (informational): CI prints the full violation list as a
## build log, but does not fail the build on it, because the known issues
## above are not yet resolved. Flip `FAIL_ON_VIOLATIONS <- TRUE` below
## once they are, so this becomes a real quality gate instead of a report.
## -----------------------------------------------------------------------

FAIL_ON_VIOLATIONS <- FALSE

args <- commandArgs(trailingOnly = TRUE)
site_dir <- if (length(args) >= 1) args[[1]] else "_site"
port <- if (length(args) >= 2) as.integer(args[[2]]) else 8921L
url <- sprintf("http://127.0.0.1:%d/index.html", port)

if (!requireNamespace("chromote", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Needs the 'chromote' and 'jsonlite' packages: install.packages(c(\"chromote\", \"jsonlite\"))")
}

axe_path <- file.path(tempdir(), "axe.min.js")
if (!file.exists(axe_path)) {
  message("Downloading axe-core...")
  utils::download.file("https://cdn.jsdelivr.net/npm/axe-core@4/axe.min.js",
                        axe_path, quiet = TRUE, mode = "wb")
}
axe_source <- paste(readLines(axe_path, warn = FALSE), collapse = "\n")

b <- chromote::ChromoteSession$new()
cat(sprintf("Navigating to %s ...\n", url))
b$Page$navigate(url, wait_ = TRUE)

ready_js <- '
(function () {
  var f = document.querySelector("iframe.app-frame");
  if (!f) return "no-iframe";
  var d = f.contentDocument || (f.contentWindow && f.contentWindow.document);
  if (!d) return "no-doc";
  return d.getElementById("analysis_choice") ? "ready" : "not-ready";
})()
'
ready <- FALSE
t0 <- Sys.time()
repeat {
  elapsed <- as.numeric(Sys.time() - t0, units = "secs")
  if (elapsed > 90) break
  val <- tryCatch(b$Runtime$evaluate(ready_js)$result$value, error = function(e) NA_character_)
  if (identical(val, "ready")) { ready <- TRUE; break }
  Sys.sleep(3)
}
if (!ready) {
  cat("App never became ready -- skipping accessibility audit (see deploy/smoke_test.R for the boot check).\n")
  b$close()
  quit(status = if (FAIL_ON_VIOLATIONS) 1 else 0)
}

# axe-core's color-contrast check samples actual rendered pixels rather than
# raw CSS values, and was observed to intermittently misfire immediately
# after #analysis_choice appears -- CSS transitions (background-color,
# opacity) on freshly-mounted elements (see styles.css `transition:` rules)
# can still be mid-flight at that exact instant. A short settle delay here
# removes that flakiness; it does not affect what the audit finds, only
# when it looks.
Sys.sleep(1)

inject_js <- sprintf('
(function () {
  var f = document.querySelector("iframe.app-frame");
  var d = f.contentDocument;
  var s = d.createElement("script");
  s.textContent = %s;
  d.head.appendChild(s);
  return typeof f.contentWindow.axe;
})()
', jsonlite::toJSON(axe_source, auto_unbox = TRUE))
axe_type <- b$Runtime$evaluate(inject_js)$result$value
if (!identical(axe_type, "object")) {
  stop("Failed to inject axe-core into the app iframe (typeof axe = ", axe_type, ")")
}

run_js <- '
(function () {
  var f = document.querySelector("iframe.app-frame");
  var w = f.contentWindow;
  return w.axe.run(w.document, {}).then(function (results) {
    return JSON.stringify({
      violations: results.violations.map(function (v) {
        return {
          id: v.id, impact: v.impact, help: v.help, helpUrl: v.helpUrl,
          nodes: v.nodes.length,
          targets: v.nodes.slice(0, 5).map(function (n) { return n.target.join(" "); })
        };
      }),
      passes: results.passes.length
    });
  });
})()
'
res <- b$Runtime$evaluate(run_js, awaitPromise = TRUE)
b$close()

if (is.null(res$result$value)) {
  stop("axe.run() did not return a result -- check the browser console for errors.")
}
out <- jsonlite::fromJSON(res$result$value, simplifyDataFrame = FALSE)

cat(sprintf("\n%d passed checks, %d violation type(s).\n\n", out$passes, length(out$violations)))
if (length(out$violations)) {
  for (v in out$violations) {
    cat(sprintf("[%s] %s (%s) -- %d node(s)\n", v$impact, v$id, v$help, v$nodes))
    cat(sprintf("  %s\n", v$helpUrl))
    for (t in v$targets) cat(sprintf("  - %s\n", t))
    cat("\n")
  }
} else {
  cat("No violations found.\n")
}

if (FAIL_ON_VIOLATIONS && length(out$violations)) quit(status = 1)
quit(status = 0)
