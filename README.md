# A Priori Power Analysis Wizard

<!-- Zenodo DOI badge removed for now: the project's earlier Zenodo records were
     withdrawn in August 2026 when the repository history was rebuilt, so every
     previous DOI (concept DOI included) resolves to a "record deleted"
     tombstone. Restore the badge once the GitHub-Zenodo integration is
     re-established and a new DOI is minted. -->

A step-by-step Shiny application that guides researchers in experimental
economics and behavioral science through an a priori (before data
collection) power analysis to determine required sample size.

## What it does

The app is a wizard with four conceptual steps, implemented per test
family:

1. **Main analysis** -- comparison of two independent means, factorial
   between-subjects ANOVA, multiple linear regression, logistic
   regression, comparison of two proportions, a paired/repeated-measures
   comparison, a two-arm clustered design (participants interacting in
   matching groups / lab sessions, or a cluster-randomized field trial), a
   chi-square test (goodness-of-fit or independence) for categorical
   outcomes across two or more groups, a bivariate correlation test,
   McNemar's test for paired binary outcomes, a TOST equivalence test
   for two independent means, a one-way ANCOVA (group comparison
   adjusting for one covariate), or a two-group log-rank test for a
   time-to-event outcome (a niche tool for this app's audience -- e.g.,
   time-to-decision in a strategic game, or attrition/dropout timing in
   a longitudinal study), or a Wilcoxon-Mann-Whitney rank-sum test for
   two independent groups with a skewed, bounded, or ordinal outcome, or a
   repeated-measures ANOVA (the same participants measured three or more
   times, including the within x between interaction of a mixed design).
2. **Design structure** -- for factorial ANOVA, the number of factors and
   levels, and (critically) which **focal contrast** power should be
   computed for: a specific main effect or the interaction. The app
   computes power for that contrast only, never a generic omnibus test,
   and displays a persistent warning that interaction effects typically
   require far larger samples than the corresponding main effect
   (~4x the per-cell N for a "halved effect" interaction).
3. **Statistical parameters** -- alpha (default 0.05), power (default
   0.80, with a one-click switch to 0.90), one- vs. two-tailed test,
   balanced or custom allocation ratio, and an optional **Bonferroni
   correction for multiple planned comparisons**: set the number of
   planned comparisons and the alpha entered is treated as the
   family-wise rate, with the per-test alpha actually used (alpha /
   n_comparisons) disclosed automatically in the generated report text.
   This one shared input applies to every analysis family with no
   family-specific wiring. The same panel also takes an **expected
   attrition / exclusion rate**: the app then reports both the N your
   analysis needs and the larger N you should actually recruit
   (`analysis N / (1 - rate)`) so enough participants survive dropout and
   comprehension-check exclusions, and discloses it in the report text.
4. **Effect size** -- three explicit branches for most families (two
   families, McNemar's test and the TOST equivalence test, use a
   simpler, direct-entry effect-size step instead -- see below):
   - **Cohen's conventions** (small/medium/large), flagged as arbitrary,
     field-agnostic thresholds.
   - **Previous study + safeguard power** (Perugini, Gallucci &
     Costantini, 2014): enter the published effect size and the original
     study's N; the app computes a one-sided confidence interval (default
     80%) around the published estimate and uses its lower bound as a
     conservative, publication-bias-corrected input. The naive N (from
     the raw published estimate) and the safeguard-corrected N are shown
     side by side.
   - **SESOI** (smallest effect size of interest), in raw units (with an
     expected SD) or already-standardized units.

   McNemar's test has a genuinely two-dimensional effect (the two
   discordant-pair probabilities p10/p01), and the TOST equivalence
   test's key parameter (the equivalence margin) is inherently a directly-
   stated quantity -- neither fits the Cohen's-convention/safeguard/SESOI
   framework, so both use a simpler, direct-entry effect-size step
   instead of the three branches above.

The results step shows total N and per-group/per-cell N (always rounded
UP), a power-vs-N curve with the solution highlighted, an inverse
sensitivity analysis (minimum detectable effect for a budget-constrained
maximum N), an optional **budget** panel, and a paste-ready English
report/pre-registration text block with full method citations. Reports
can be exported as HTML or PDF.

The budget panel exists because experiments -- especially in experimental
economics -- pay their participants, so a sample size is also a line in a
grant application. Enter the cost of one participant (show-up fee plus
expected average earnings) and any fixed costs to turn N into a total,
and optionally a fixed budget to see how many participants it covers and,
reusing the same sensitivity machinery, the smallest effect that sample
could still detect. Amounts are labelled in euro, pound, or US dollar, selectable in the panel (the choice is cosmetic and affects no calculation).

## Saving, sharing, and resuming a project

Because the browser-only build has no server-side storage, whatever you
enter would normally be lost on a page reload. Every family page has
**Save** and **Share link** buttons (top-right, next to the family
title):

- **Save** downloads the current family's inputs as a small `.json`
  file. **Load a saved project**, next to the "Start" button on Step 1,
  reopens one later -- exactly where you left off, including which
  sub-step you were on.
- **Share link** rewrites the page's own URL to encode the same state as
  a query parameter and copies it to the clipboard; anyone who opens that
  link lands directly on the same family with the same inputs already
  filled in, no file needed.

Both funnel through the same pure serialization logic
(`R/project_state.R`, unit-tested in `tests/testthat/test-project_state.R`):
capture every input belonging to the active family, JSON-encode it, and
(for the link) base64url-encode that JSON for the URL. Restoring waits
for the target family's UI to actually be inserted into the page (lazy
loading, see below) before sending the input values, via
`session$onFlushed()`.

## Designed for first-time users

The app assumes no prior power-analysis experience:

- **Step 1** includes a "Not sure which analysis to choose?" helper: three
  plain-language questions (continuous or binary outcome? how many
  groups/predictors? interaction or not?) recommend a test family and can
  jump straight into its wizard.
- Every input that uses statistical vocabulary (alpha, power, Cohen's d/f/h,
  allocation ratio, safeguard confidence level, ...) carries an inline "?"
  tooltip with a plain-language explanation, reachable on hover without
  leaving the page.
- Each design step includes a short worked example ("does a reminder email
  predict task completion?") so users can map their own study onto the
  right inputs.
- The effect-size step opens with a one-line prompt -- "what is the
  smallest effect that would still be worth acting on?" -- to anchor the
  SESOI branch, which is the recommended default over Cohen's generic
  benchmarks.
- **Guided mode** (toggle in the top-right of the header, off by
  default): adds a longer "In plain language" explanation box to every
  step of every family, written for someone who has never run a power
  analysis before. Every box is always present in the page but hidden by
  CSS (`.guided-explain` in `www/styles.css`) unless the toggle is on, so
  flipping it takes effect instantly across every step and family --
  including ones already visited earlier in the session -- with no
  server round trip, the same way the light/dark theme toggle works.

## Look and feel

Modern product UI in **two themes**, light and dark, with a toggle in the
top-right of the header. Soft raised cards, hairline borders, large
display numerals for the headline results, one strong accent per theme
(indigo in light, cyan in dark), and motion used only to explain state
changes.

Structurally it's [`bslib`](https://rstudio.github.io/bslib/) (Bootstrap 5)
with icon-labeled sections (via `shiny::icon()` / Font Awesome 6, bundled
with recent Shiny) and `bslib::value_box()` summaries on the results step.

### Theming

Every colour is a CSS custom property, and the two themes are two blocks
of the same variable names selected by a `data-theme` attribute on
`<html>`. Switching is instant and entirely client-side -- no server round
trip, no re-render.

The visitor's choice is remembered in `localStorage`. On a first visit
with no saved choice, the app follows the OS-level
`prefers-color-scheme` (and keeps following it, live, until the visitor
picks a theme explicitly). A small inline script in `<head>` applies the
theme *before* first paint, so a dark-mode user never sees a white flash.

**Where the styling lives -- and why.** `app.R` uses a plain, uncustomized
`bs_theme(version = 5)`. Passing colors or fonts to `bs_theme()` makes
bslib recompile Bootstrap's Sass at runtime through compiled libsass
bindings, which is unreliable inside webR/shinylive. So *all* of the app's
branding lives in plain CSS custom properties at the top of
`www/styles.css` (the two `:root` / `[data-theme="dark"]` blocks). **To
re-skin the app, edit those two blocks -- nothing else should need to
change**, and you should not need to touch `bs_theme()`.

Because the power curve is drawn by R (which can't read CSS variables),
the active theme is reported back to the server as a root-level
`pw_theme` input and resolved by `app_palette()` in `global.R`. Module
sessions can't read root-level inputs directly, so the top-level server
parks the reactive in `session$userData`, which Shiny shares across
module sessions while keeping it isolated per user session. Keep
`APP_PALETTES` in `global.R` in sync with the `:root` blocks in
`styles.css`.

### Interactivity

- **Interactive power curve.** If [`plotly`](https://plotly.com/r/) is
  installed, the curve supports hover readout (N and power at any point),
  drag-to-zoom, and PNG/SVG export (both via plotly's own client-side
  modebar, no server round trip). If it isn't, the app silently falls
  back to the static `ggplot2` version -- see `HAS_PLOTLY` in
  `global.R` -- with its own "Download plot as SVG" button. The fallback
  is deliberate: the browser-only build depends on webR's WebAssembly
  package repository, and a missing or ABI-mismatched package would
  otherwise take the whole app down at startup. Worst case here is a
  less fancy chart, not a blank page.
- **Compare across alpha, power, AND effect size.** Every family's
  Results step has an optional "Compare across alpha / power / effect
  size" panel: tick extra significance levels, power targets, or effect
  sizes (framed as "X% weaker/stronger" than what you entered, since most
  families here don't have a Cohen's-style small/medium/large convention
  that applies) to see each scenario's required N plotted together on the
  same curve. Not offered for McNemar's test or logistic regression,
  whose effect isn't a single freely-rescalable number.
- **Animated result counters.** The value-box numbers count up when they
  change. This is decorative only: the DOM holds the final, correct string
  before the animation starts and the animation always terminates on
  exactly that string, so it cannot alter a displayed result.
- **Step transitions**, hover lift on the analysis cards, and an animated
  progress indicator.
- All motion respects `prefers-reduced-motion: reduce`.

### Fonts

Inter is loaded from Google Fonts in `app.R`'s `tags$head`, with
`display=swap` and a full system fallback stack declared in `styles.css`.
If it can't load -- offline use, a blocked CDN, a restrictive network --
the app renders immediately in the native UI sans and stays perfectly
legible. To make the app fully self-contained, delete the three
`fonts.googleapis.com` / `fonts.gstatic.com` `tags$link()` calls in
`app.R` and the fallback takes over permanently.

## What it deliberately does NOT do

- No Monte Carlo simulation-based power analysis (no `simr`, no
  bootstrap/simulation loops). Every calculation in this version is
  closed-form and returns in well under a second.
- No generic "omnibus ANOVA" power number -- factorial power is always
  computed for the specific focal contrast the user names.
- No silent defaults: every number the app produces is shown together
  with the assumptions (alpha, power, tails, allocation, effect-size
  branch and its justification) that generated it.

## Project structure

```
power-analysis-app/
├── app.R                     # Wizard shell: Step 1, decision helper, routing, bslib theme
├── global.R                  # Package loading + sources R/ and modules/ + shared color palette
├── R/                         # Pure, testable calculation functions (no Shiny)
│   ├── utils.R
│   ├── effect_size_conventions.R   # Cohen's (1988) benchmarks
│   ├── safeguard_power.R           # Perugini et al. (2014) safeguard power
│   ├── sesoi.R                     # Raw-unit <-> standardized-unit conversions
│   ├── power_two_means.R           # pwr::pwr.t.test / pwr.t2n.test
│   ├── power_proportions.R         # pwr::pwr.2p.test / pwr.2p2n.test
│   ├── power_regression.R          # pwr::pwr.f2.test
│   ├── power_logistic.R            # Demidenko (2007), base R only (no external dependency)
│   ├── power_anova_factorial.R     # Noncentral-F (Cohen 1988), base R only (no external dependency)
│   ├── power_paired_t.R            # pwr::pwr.t.test(type = "paired")
│   ├── power_clustered.R           # Design-effect-inflated pwr::pwr.t.test (Donner & Klar, 2000)
│   ├── power_chisq.R               # pwr::pwr.chisq.test (Cohen 1988, ch. 7)
│   ├── power_correlation.R         # pwr::pwr.r.test (Cohen 1988, ch. 3)
│   ├── power_mcnemar.R             # Connor (1987), base R only (no external dependency)
│   ├── power_tost.R                # Exact noncentral-t TOST (Phillips 1990), base R only
│   ├── power_ancova.R              # pwr::pwr.f2.test with covariate-adjusted f2 (Cohen 1988; Borm et al. 2007)
│   ├── power_survival.R            # Schoenfeld (1983) log-rank events formula, base R only
│   ├── power_wilcoxon.R            # Noether (1987) Mann-Whitney rank-sum formula, base R only
│   ├── power_rm_anova.R            # Repeated-measures noncentral F with rho and sphericity correction, base R only
│   ├── project_state.R             # Save/load/share-link JSON (de)serialization
│   ├── power_curve.R               # Generic power-vs-N curve generator
│   ├── sensitivity_analysis.R      # Generic inverse/minimum-detectable-effect
│   └── report_text.R               # Paste-ready report text builder
├── modules/                   # Shiny modules -- one per test family
│   ├── common_ui.R                 # Shared UI fragments (params step, effect-size step, nav)
│   ├── common_results.R            # Shared results step (plot, sensitivity, export)
│   ├── mod_two_means.R
│   ├── mod_proportions.R
│   ├── mod_regression.R
│   ├── mod_logistic.R
│   ├── mod_anova_factorial.R
│   ├── mod_paired_t.R
│   ├── mod_clustered.R
│   ├── mod_chisq.R
│   ├── mod_correlation.R
│   ├── mod_mcnemar.R
│   ├── mod_tost.R
│   ├── mod_ancova.R
│   ├── mod_survival.R
│   ├── mod_wilcoxon.R
│   └── mod_rm_anova.R
├── www/
│   └── styles.css
├── tests/
│   ├── testthat.R
│   └── testthat/              # One test file per R/ calculation module
├── deploy/
│   └── export_shinylive.R     # Local build script for the browser-only (webR) deployment
├── .github/workflows/
│   └── deploy-shinylive.yml   # CI: auto-build + publish the shinylive site to GitHub Pages
├── renv.lock
└── README.md
```

Every `R/power_*.R` file exposes three kinds of pure functions, all
directly unit-testable without starting Shiny:

- `power_<family>_n(...)` -- solves for required sample size
- `power_<family>_at_n(...)` -- power achieved at a given N (used by the
  power curve)
- `power_<family>_min_*(...)` -- minimum detectable effect at a given N
  (used by the sensitivity analysis)

## Running locally

```r
install.packages("renv")
renv::restore()   # installs the exact package versions in renv.lock
shiny::runApp()
```

> **Note on `renv.lock`:** this lockfile was authored by hand (the
> development environment used to build this app had no local R
> installation available to run `renv::snapshot()`). Package version
> numbers are current-as-of-writing best estimates, and the lockfile does
> not include transitive-dependency hashes. **Before relying on this
> project, run `renv::init()` followed by `renv::snapshot()` in a real R
> session to regenerate an accurate, fully-resolved lockfile.**

## Running the tests

```r
testthat::test_dir("tests/testthat")
```

Reference values for the two-means, two-proportions, multiple regression,
and logistic-regression tests are checked against textbook / G*Power /
published-documentation numbers (see comments in each test file for the
exact source; the logistic-regression tests are pinned against the
reference values published in WebPower's own documentation, since
`R/power_logistic.R` reimplements the same Demidenko 2007 formula without
depending on that package). The factorial-ANOVA tests are similarly
pinned: the one-way case is cross-checked directly against
`pwr::pwr.anova.test()` (an independent, widely-used implementation of
the same closed-form relationship), and the two-factor case was
validated against `Superpower::ANOVA_exact()`'s output across 12 designs
(0 numerical difference) before that dependency was removed -- see the
header comment in `R/power_anova_factorial.R` for the full derivation.

## Deployment

### shinyapps.io

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "<your-account>", token = "<token>", secret = "<secret>")
rsconnect::deployApp(appDir = ".", appName = "power-analysis-wizard")
```

### Posit Connect / Shiny Server

Copy the project directory (including `renv.lock`) to the server and
point Shiny Server / Connect at it; both will call `renv::restore()`
automatically if `renv` integration is enabled, or run it manually first.

### Browser-only deployment (shinylive + GitHub Pages, no server at all)

This app does no simulation and every calculation is closed-form and
sub-second, which makes it a good candidate for
[shinylive](https://posit-dev.github.io/r-shinylive/): a build of the app
compiled to WebAssembly (via webR) that runs entirely inside the visitor's
browser tab. There is no R process on a server at all -- you host a folder
of static files (HTML/JS/wasm) on GitHub Pages, Netlify, or any static
file host, for free, with no usage caps and no "app asleep" cold starts.

**Step 1 -- test the export locally, before deploying anywhere.**

```r
# from the project root (the folder containing app.R)
source("deploy/export_shinylive.R")
```

This installs the `shinylive` package if needed and writes a static
build to `_site/`. The first run downloads webR and the WebAssembly
builds of every package this app uses, so it can take a few minutes.

**Step 2 -- preview it in a real browser before trusting it.**

```r
install.packages("httpuv")  # if not already installed
httpuv::runStaticServer("_site")
```

Open the printed local URL, open the browser's developer console (F12 /
Cmd+Opt+I), and click through the *entire* wizard once -- including the
Results step, which is where every package actually gets exercised.
**Watch the console for red package-loading errors.** This is the one
step that genuinely needs to happen on your machine: nothing here was
verified against a live webR runtime while building this app, because no
R installation was available in that environment.

**Resolved issue (kept here for the record):** an earlier version of this
app used `WebPower::wp.logistic()` for the logistic-regression family.
`WebPower` pulls in `lme4`, `Matrix`, `lavaan`, and `PearsonDS` as
dependencies -- and one of those compiled packages turned out to be
ABI-incompatible with the webR/shinylive WebAssembly build, causing a
`call_indirect to a signature that does not match` crash at startup in
the browser-only build (confirmed via a diagnostic build with the
logistic module disabled: the crash disappeared). The fix was to
reimplement the same Demidenko (2007) formula directly in
`R/power_logistic.R` using only base R (`integrate()`, `uniroot()`,
`pnorm()`) -- validated against WebPower's own published reference
values (see `tests/testthat/test-power_logistic.R`) -- and drop
`WebPower` as a dependency entirely. If you still see a
`call_indirect`-style crash after this fix, it means a *different*
package's WebAssembly build is at fault; isolate it the same way: comment
out one family module at a time in `app.R`/`global.R`, re-export, and see
which one makes the crash disappear.

**Step 3 -- deploy to GitHub Pages.**

The included `.github/workflows/deploy-shinylive.yml` automates steps 1
onward on every push to `main`:

1. Push this project to a GitHub repository.
2. In the repo, go to **Settings → Pages → Build and deployment → Source**
   and select **GitHub Actions**.
3. Push to `main` (or trigger the workflow manually from the **Actions**
   tab). The workflow installs `shinylive`, runs the same export as
   above, and publishes the result.
4. Once the run finishes, your app is live at
   `https://<your-github-username>.github.io/<repo-name>/`.

Because this is a static site, there's no `renv::restore()` step at
runtime -- the exact package versions get baked into the exported
`_site/` folder at build time, so pin them in the GitHub Actions
workflow (or in a project-level `renv.lock`, restored before the export
step) if you need long-term reproducibility of a specific build.

**What's different in the browser-only build:** everything works exactly
as in the server-hosted version except the "Download PDF report" button,
which is hidden automatically (see `IS_SHINYLIVE` in `global.R`) because
PDF rendering needs a pandoc/LaTeX toolchain that doesn't exist inside a
browser sandbox. The HTML report export has no such dependency and works
identically in both builds.

**Troubleshooting: `call_indirect to a signature that does not match`
(or any other low-level WebAssembly crash right as the app starts).**
This is a WebAssembly ABI mismatch -- some compiled code was called with
a signature the wasm runtime didn't expect -- not an R logic error, so it
won't mention any of your own code. The most likely source in this app is
`bslib`'s theming: customizing `bs_theme()` with colors/fonts/radii (via
`primary =`, `base_font =`, etc.) makes bslib recompile Bootstrap's Sass
at runtime through compiled `sass`/libsass bindings, and that live
recompilation is not reliable inside webR. For this reason `app.R` uses a
**plain, uncustomized** `bs_theme(version = 5)` with no color/font args --
all of the app's actual branding lives in plain CSS in `www/styles.css`
instead, so this costs nothing visually. If you've modified `app_theme`
in `app.R` and hit this error, that's the first thing to revert.

If the crash persists even with a plain `bs_theme()`, the next suspect is
a version-mismatched WebAssembly build of one of the app's compiled
(C/C++/Fortran) R package dependencies. This is exactly what happened
during development: `WebPower`'s dependency chain (`lme4`, `Matrix`,
`lavaan`, `PearsonDS`) triggered this crash, and the fix was to drop
`WebPower` entirely -- see the "Resolved issue" note above. `R/power_logistic.R`
no longer depends on it, so this specific cause is closed. If a *new*
crash like this shows up after adding a package of your own, isolate it
the same way it was isolated here: temporarily remove the family module
that uses the suspect package from `app.R` (its `tabPanelBody()` block
and its `mod_<family>_server()` call) and from `global.R`'s `library()`
calls, re-export, and retest -- if the crash disappears, that package's
WebAssembly build is the culprit.

### PDF export dependency

The "Download PDF report" button uses `rmarkdown::render()` with
`pdf_document`, which requires a LaTeX distribution (e.g.
`tinytex::install_tinytex()`) on the deployment server. If no LaTeX
engine is available, the app falls back to a plain-text file rather than
producing a broken PDF; the HTML export has no such dependency and always
works.

## Extending: adding a simulation-based module later

The codebase is structured so a future simulation-based module (e.g.,
mixed models via `simr`) can be added without touching existing files:

1. Add pure functions to a new `R/power_mixed_models.R`, following the
   `power_*_n()` / `power_*_at_n()` / `power_*_min_*()` naming convention
   used by every other family.
2. Add a new `modules/mod_mixed_models.R` following the same
   `mod_<family>_ui()` / `mod_<family>_server()` pattern as the existing
   family modules, calling the shared `params_step_ui()`,
   `effect_size_step_ui()`, `results_panel_ui()` and
   `wire_results_server()` helpers from `modules/common_ui.R` /
   `modules/common_results.R`.
3. Add a `"mixed_models"` branch to `generate_power_curve()`
   (`R/power_curve.R`) and `sensitivity_min_effect()`
   (`R/sensitivity_analysis.R`).
4. Register the new module's UI/server in `app.R`.

No existing file needs to change beyond those two small `switch()`
additions -- this was a deliberate design constraint from the start.

## Method references

- Cohen, J. (1988). *Statistical Power Analysis for the Behavioral
  Sciences* (2nd ed.). Routledge.
- Champely, S. (2020). `pwr`: Basic Functions for Power Analysis. R
  package.
- Demidenko, E. (2007). Sample size determination for logistic regression
  revisited. *Statistics in Medicine*, 26(18), 3385-3397. (The formula
  this app implements directly in `R/power_logistic.R`, without a
  `WebPower` dependency; validated against `WebPower`'s own published
  reference values.)
- Perugini, M., Gallucci, M., & Costantini, G. (2014). Safeguard power as
  a protection against imprecise power estimates. *Perspectives on
  Psychological Science*, 9(3), 319-332.
- Donner, A., & Klar, N. (2000). *Design and Analysis of Cluster
  Randomization Trials in Health Research*. Arnold. (Source of the
  design-effect formula, `1 + (m-1)xICC`, used in
  `R/power_clustered.R`.)
- Chinn, S. (2000). A simple method for converting an odds ratio to
  effect size for use in meta-analysis. *Statistics in Medicine*,
  19(22), 3127-3131.
- Simonsohn, U. (2014). No-way interactions. Blog post,
  *Data Colada* -- source of the "~4x sample size for a halved-effect
  interaction" rule of thumb shown in the ANOVA warning.
- Connor, R. J. (1987). Sample size for testing differences in
  proportions for the paired-sample design. *Biometrics*, 43(1), 207-211.
  (Large-sample McNemar's-test sample-size formula, re-derived and
  cross-validated by Monte Carlo simulation before implementation in
  `R/power_mcnemar.R`.)
- Schuirmann, D. J. (1987). A comparison of the two one-sided tests
  procedure and the power approach for assessing the equivalence of
  average bioavailability. *Journal of Pharmacokinetics and
  Biopharmaceutics*, 15(6), 657-680.
- Phillips, K. F. (1990). Power of the two one-sided tests procedure in
  bioequivalence. *Journal of Pharmacokinetics and Biopharmaceutics*,
  18(2), 137-144. (Exact noncentral-t TOST power formula, re-derived and
  cross-validated by Monte Carlo simulation before implementation in
  `R/power_tost.R`.)
- Borm, G. F., Fransen, J., & Lemmens, W. A. (2007). A simple sample size
  formula for analysis of covariance in randomized clinical trials.
  *Journal of Clinical Epidemiology*, 60(12), 1234-1238. (Source of the
  covariate-adjusted f2 used in `R/power_ancova.R`.)
- Schoenfeld, D. A. (1983). Sample-size formula for the proportional-
  hazards regression model. *Biometrics*, 39(2), 499-503. (Log-rank
  events formula used in `R/power_survival.R`, cross-validated by Monte
  Carlo simulation before implementation; the app's own guided-mode text
  discloses that this formula is somewhat conservative for a strong
  hazard ratio combined with unequal allocation.)
- Faul, F., Erdfelder, E., Lang, A.-G., & Buchner, A. (2007). G*Power 3: A
  flexible statistical power analysis program for the social, behavioral, and
  biomedical sciences. *Behavior Research Methods*, 39(2), 175-191. (Source of
  the repeated-measures noncentrality/degrees-of-freedom formulation used in
  `R/power_rm_anova.R`, cross-validated against Monte Carlo simulation and,
  for the two-measurement case, against `pwr`'s paired t-test exactly.)
- Noether, G. E. (1987). Sample size determination for some common
  nonparametric tests. *Journal of the American Statistical Association*,
  82(398), 645-647. (Closed-form Wilcoxon-Mann-Whitney sample size used in
  `R/power_wilcoxon.R`, cross-validated against Monte Carlo simulation of
  `stats::wilcox.test()` before implementation.)
- Peto, R., & Peto, J. (1972). Asymptotically efficient rank invariant
  test procedures. *Journal of the Royal Statistical Society: Series A*,
  135(2), 185-198. (Source of the log-hazard-ratio variance approximation
  used in `safeguard_ci_logHR()`, `R/safeguard_power.R`.)

## Contributing, issues, and support

Bug reports, feature requests, and pull requests are welcome via the
[GitHub issue tracker](https://github.com/federicoatz/power-analysis-app/issues).
For a code contribution: fork the repository, make your change (see
"Project structure" above for where each kind of logic lives, and
"Running the tests" for how to validate it), and open a pull request
against `main`. New analysis families or calculation changes should
include a corresponding `testthat` file following the existing pattern
(a pinned textbook/G\*Power reference value where one exists, plus
monotonicity/consistency checks). For questions about using the app,
please open an issue rather than emailing directly, so the answer is
searchable for future users.

## License

MIT -- see [LICENSE](LICENSE).

## Citation

If you use this app in your research, please cite it -- see
[CITATION.cff](CITATION.cff) for the machine-readable citation record
(also picked up automatically by GitHub's "Cite this repository" button
and by Zenodo).
