## deploy/smoke_test.R
## -----------------------------------------------------------------------
## Browser smoke test for the shinylive static build: opens the exported
## site in a real (headless) Chrome via the `chromote` package and waits
## for the app to actually finish booting inside its iframe, rather than
## just checking that `shinylive::export()` (in export_shinylive.R)
## exited without error -- a broken webR package fetch, a JS error before
## Shiny connects, or a wasm ABI mismatch (see export_shinylive.R's
## "IMPORTANT CAVEAT" section for a real past example of this) would all
## leave index.html present and well-formed while the app itself never
## becomes usable.
##
## Readiness signal: this polls for the exact same DOM marker the app's
## own boot splash (injected in export_shinylive.R) already uses --
## `#analysis_choice` (Step 1's real content) inside `iframe.app-frame` --
## so "the smoke test passes" and "a visitor's splash screen disappears"
## mean the same thing by construction, not by coincidence.
##
## Usage: first start deploy/serve_static.R in the background (see the
## "smoke-test" job in ../.github/workflows/deploy-shinylive.yml for the
## exact sequence), then:
##   Rscript deploy/smoke_test.R [url] [timeout_seconds]
## Exits with status 1 (and prints any browser console errors captured
## along the way) if the app never becomes ready in time.
## -----------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
url <- if (length(args) >= 1) args[[1]] else "http://127.0.0.1:8917/index.html"
timeout_s <- if (length(args) >= 2) as.numeric(args[[2]]) else 90

library(chromote)

b <- ChromoteSession$new()

console_msgs <- character(0)
b$Runtime$consoleAPICalled(function(msg) {
  if (identical(msg$type, "error")) {
    text <- tryCatch(
      paste(vapply(msg$args, function(a) {
        v <- a$value
        if (is.null(v)) a$description %||% "<unprintable>" else as.character(v)
      }, character(1)), collapse = " "),
      error = function(e) "<console message could not be decoded>"
    )
    console_msgs <<- c(console_msgs, text)
  }
})
`%||%` <- function(a, z) if (is.null(a)) z else a

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
  if (elapsed > timeout_s) break

  val <- tryCatch(b$Runtime$evaluate(ready_js)$result$value,
                   error = function(e) paste("eval-error:", conditionMessage(e)))
  cat(sprintf("  [%.0fs] %s\n", elapsed, val))
  if (identical(val, "ready")) { ready <- TRUE; break }
  Sys.sleep(3)
}

b$close()

if (ready) {
  cat(sprintf("\nSmoke test PASSED: app became ready in %.0fs.\n", elapsed))
  quit(status = 0)
} else {
  cat(sprintf("\nSmoke test FAILED: app did not become ready within %ds.\n", timeout_s))
  if (length(console_msgs)) {
    cat("\nBrowser console errors captured during the wait:\n")
    cat(paste(" -", console_msgs), sep = "\n")
  } else {
    cat("\n(No browser console errors were captured -- the app may simply be slow, or the readiness selector may be stale.)\n")
  }
  quit(status = 1)
}
