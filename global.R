## global.R
## -----------------------------------------------------------------------
## Loaded once per R process before ui.R/server.R (or app.R) run. Attaches
## packages and sources every file in R/ (calculation logic) and
## modules/ (Shiny modules + shared UI helpers).
## -----------------------------------------------------------------------

library(shiny)
library(bslib)
library(htmltools)

# Single source of truth for the app version shown in the UI footer, baked
# into generated report text (R/report_text.R), and saved into every
# project-state JSON (R/project_state.R) so a result can always be traced
# back to the exact app version that produced it. Keep in sync with the
# `version:` field in CITATION.cff -- tests/testthat/test-misc.R asserts
# the two match, so a mismatch fails the test suite instead of drifting
# silently.
APP_VERSION <- "0.17.0"
## NOTE: neither the logistic-regression nor the factorial-ANOVA family
## depends on an external power-analysis package, and this is deliberate.
## Logistic regression used to depend on WebPower::wp.logistic(), whose
## heavy compiled dependency chain (lme4/Matrix/lavaan/PearsonDS) crashed
## the browser-only (shinylive) build with a WebAssembly ABI error -- see
## README.md, "Browser-only deployment" -> Troubleshooting. Factorial
## ANOVA used to depend on Superpower::ANOVA_design()/ANOVA_exact(), which
## worked correctly but pulled in ~98MB of transitive wasm downloads
## (afex, lme4, car, emmeans, Matrix, doBy, survival, forecast, and more)
## on EVERY page load in the browser-only build, regardless of whether
## the visitor ever opened the ANOVA family -- a real network trace
## showed this was ~75% of total bytes transferred at startup. Both
## families now reimplement their published closed-form formula directly
## in base R (R/power_logistic.R: Demidenko 2007, validated against
## WebPower's own reference values; R/power_anova_factorial.R: the
## standard noncentral-F relationship, Cohen 1988, validated against
## Superpower's and pwr::pwr.anova.test()'s output before the dependency
## was removed). Neither package is a dependency of this app.
##
## pwr and ggplot2 are still used everywhere via an explicit `::` prefix
## (never library()-attached). Whether this actually defers their wasm
## fetch in the browser-only build is unconfirmed -- see the ANOVA/
## Superpower case above, where removing library() calls alone did NOT
## defer the fetch, so don't assume `::`-only usage saves load time by
## itself. It's kept mainly so nothing here silently starts requiring
## library(pwr)/library(ggplot2) again. Do NOT add unqualified calls to
## pwr:: or ggplot2:: functions anywhere in this codebase without the
## namespace prefix, or they will fail here.

# App-wide color palette, reused by both the bslib theme (app.R) and the
# ggplot2 power-curve theme (modules/common_results.R) so plots and UI
# chrome feel like one designed product rather than default-Shiny grey.
# Two palettes, one per UI theme, mirroring the CSS custom properties in
# www/styles.css. The power curve is drawn by R (server-side, or by webR in
# the browser-only build) and therefore cannot read CSS variables -- so the
# active theme is passed from the browser to R via the `pw_theme` input
# (see the theme-toggle script at the bottom of app.R) and resolved here by
# app_palette(). Keep these in sync with the :root blocks in styles.css.
# light.muted is #6D6D78 rather than the more common #8B8B96 lighter grey
# specifically because an axe-core accessibility audit (see
# validation/accessibility_check.R) flagged the lighter value at 3.17:1
# contrast against the light theme's surface color -- below WCAG AA's
# 4.5:1 minimum for body text. #6D6D78 keeps the same hue/saturation,
# darkened to 4.5:1+ against both bg and surface. Dark theme's muted
# already passed (4.6:1+) and was left unchanged.
APP_PALETTES <- list(
  light = list(
    accent   = "#5B5BD6",
    positive = "#00926F",
    warning  = "#B45309",
    bg       = "#FFFFFF",
    surface  = "#FBFBFD",
    ink      = "#16161D",
    muted    = "#6D6D78",
    rule     = "#E8E8ED",
    fill     = "rgba(91, 91, 214, 0.13)"
  ),
  dark = list(
    accent   = "#22D3EE",
    positive = "#34D399",
    warning  = "#FBBF24",
    bg       = "#151B2B",
    surface  = "#1A2133",
    ink      = "#E8ECF4",
    muted    = "#7C8AA5",
    rule     = "#232C42",
    fill     = "rgba(34, 211, 238, 0.15)"
  )
)

#' Resolve the plotting palette for the currently active UI theme
#'
#' @param mode character, "light" or "dark" (anything else falls back to
#'   "light" -- this is deliberately forgiving because the value arrives
#'   from the browser and may be NULL on the very first render, before the
#'   client has reported in)
#' @export
app_palette <- function(mode = "light") {
  if (is.null(mode) || !identical(mode, "dark")) APP_PALETTES$light else APP_PALETTES$dark
}

# Backwards-compatible alias: some code still refers to a single palette.
APP_PALETTE <- APP_PALETTES$light

# TRUE when plotly is installed, enabling the interactive (hover + zoom)
# power curve. When FALSE the app falls back to the static ggplot2 version,
# which is always available. This is checked once at startup rather than
# per-render so the UI can pick the right output binding.
#
# The fallback exists because the browser-only (shinylive/webR) build
# depends on webR's WebAssembly package repository, and a package that
# isn't available -- or is built against a mismatched ABI -- will otherwise
# take the whole app down at startup. Making the richer chart strictly
# optional means the worst case is a less shiny plot, not a blank page.
HAS_PLOTLY <- requireNamespace("plotly", quietly = TRUE)

# TRUE when the app is running inside webR (i.e., compiled to WebAssembly
# via the `shinylive` R package and served as static files with no R
# server behind it -- see deploy/export_shinylive.R and README.md,
# section "Browser-only deployment"). webR reports its OS as "emscripten";
# this is the documented, standard way to detect that runtime from R code.
# The app uses this flag in exactly one place (modules/common_results.R)
# to hide the PDF export button, because PDF rendering needs a LaTeX/
# pandoc toolchain that does not exist inside the browser sandbox. The
# HTML export has no such dependency and works identically either way.
IS_SHINYLIVE <- identical(R.version$os, "emscripten")

# rmarkdown is used only for the optional PDF export button; the app must
# still run (with a graceful fallback message) if it or a LaTeX engine is
# unavailable in the deployment environment. Under webR this branch is
# skipped entirely via IS_SHINYLIVE rather than relying on
# requireNamespace(), because the rmarkdown *package* can still load in
# webR even though pandoc/LaTeX -- external binaries it shells out to --
# do not exist there.
if (!IS_SHINYLIVE && requireNamespace("rmarkdown", quietly = TRUE)) {
  library(rmarkdown)
}

source_dir <- function(path) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(sort(files), source))
}

source_dir("R")
source_dir("modules")

# -----------------------------------------------------------------------
# Extension point for a future simulation-based module (e.g., mixed models
# via `simr`). Nothing in this version depends on Monte Carlo simulation;
# adding such a module means: (1) drop new pure functions in R/ following
# the same power_*_n() / power_*_at_n() / power_*_min_*() naming
# convention used by every other family, (2) add a new
## modules/mod_mixed_models.R following the mod_*.R pattern above, wired
# through wire_results_server() with family = "mixed_models" after adding
# a matching branch to generate_power_curve() and sensitivity_min_effect().
# No existing file needs to change.
# -----------------------------------------------------------------------
