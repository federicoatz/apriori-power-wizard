# Changelog

All notable changes to the **A Priori Power Analysis Wizard** are documented here.
Format loosely follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
versioning follows [Semantic Versioning](https://semver.org/) (patch = fix,
minor = new feature/family, major reserved for a future breaking redesign).

> **Note on history.** The repository's git history was rebuilt from scratch in
> August 2026 for reasons unrelated to the application itself (see the Zenodo
> notes in `CITATION.cff`). Every entry below corresponds to a real tagged
> release on the current history; nothing has been renumbered or backdated.

## [1.0.0] - 2026-08-05

First public 1.0.0: the core promise (route from a research question to a
validated closed-form calculation) is now itself validated, not just the
arithmetic underneath it.

- **Decision helper is now independently validated.** Extracted the Step
  1 decision-tree logic out of `app.R`'s server function into a pure,
  testable function (`R/decision_helper.R::recommend_family()`), added
  `validation/scenario_benchmark.csv` (seventeen real experimental-
  economics/behavioral-science scenarios, nine of them the exact worked
  examples already shipped in `R/example_library.R`) and
  `validation/scenario_validation.R` to check the mapping against it, plus
  the same checks as regression tests
  (`tests/testthat/test-decision_helper.R`) that run in CI on every push.
- **Fixed a real gap this validation found.** Exhaustive enumeration of
  every answer combination showed bivariate correlation was not reachable
  from the decision helper at all (the closest path routed to multiple
  regression instead). Split the ambiguous "a continuous predictor"
  question option into two explicit ones -- correlation vs. regression --
  so the decision helper now reaches fifteen of sixteen families; only the
  TOST equivalence test remains excluded, by design.
- **`renv.lock` is now machine-generated**, not hand-authored: pins all
  105 packages this repository actually uses (app, tests, and maintenance
  scripts alike) with exact versions, `renv::status()` reports the project
  consistent, and both CI workflows use the same R version as the
  lockfile. Fixed a real `shinylive::export()` crash this surfaced (a
  missing soft dependency, `S7`, that only `renv`'s stricter dependency
  resolution exposed).
- **Restructured the README** for a first-time visitor: reordered to what
  it does -> why it's different -> how to try it -> implementation detail,
  added a "Why this app?" section stating the actual differentiator
  explicitly, compressed the dense family list into a one-liner up top
  (full detail moved further down), and added three real screenshots
  captured by driving the running app in headless Chrome. Removed
  references to the manuscript (not part of this public repository) and
  the "Resolved issue" WebPower postmortem, which had outlived its
  usefulness as inline README content.

## [0.17.0] - 2026-08-04

Prepare for a public v1.0.0: versioning, CI, a public validation page,
and real accessibility fixes.

- Added a workflow-overview graphic to the README and moved manuscript
  drafts (`paper.tex`, the APA submission version, the bibliography) into a
  local-only, gitignored `manuscript/` folder so they stay out of the public
  repo history.
- Stamped every generated report and every saved/shared project file with
  the app version that produced it (`APP_VERSION` in `global.R`), with a
  regression test guarding it against drifting from `CITATION.cff`.
- Added CI: the full `testthat` suite now runs on every push and pull
  request (`.github/workflows/test.yml`); the shinylive/webR export is
  smoke-tested in a real headless browser and audited with axe-core before
  deployment (`.github/workflows/deploy-shinylive.yml`).
- Added `VALIDATION.md`: a public, family-by-family validation page with
  real `testthat` tolerances and a reproducible, seeded Monte Carlo script
  (`validation/monte_carlo_validation.R`) for the families that need
  simulation-based validation.
- Added `CHANGELOG.md` and a GitHub issue template for bug reports.
- Fixed real accessibility issues found by the new audit: insufficient
  color contrast in the light theme's muted text, a missing page `lang`
  attribute, a heading-level skip, and missing `<header>`/`<main>`
  landmarks.

## [0.16.0] - 2026-08-04

Renamed the project to `apriori-power-wizard`.

## [0.15.0] - 2026-08-04

Added a privacy notice (`PRIVACY.md`) and enabled opt-in usage analytics.

## [0.14.0] - 2026-08-04

Self-hosted the webfont: the app now makes no third-party network requests
at all.

## [0.13.1] - 2026-08-04

Added an opt-in, privacy-preserving analytics hook (inert until configured
with a GoatCounter code).

## [0.13.0] - 2026-08-04

Added a library of worked examples drawn from paradigms in experimental
economics and behavioral science (public goods games, trust games, risk
elicitation, and more), reachable as a third entry point from Step 1.

## [0.12.0] - 2026-08-04

Added clustered designs with a binary or categorical outcome, reusing the
two-proportions and chi-square machinery under a shared design-effect
correction.

## [0.11.1] - 2026-08-04

Added currency choice (EUR/GBP/USD) to the budget panel and made the
results-step tools more prominent.

## [0.11.0] - 2026-08-03

Added expected-attrition inflation and a participant-cost budget panel,
shared across every analysis family.

## [0.10.0] - 2026-08-03

Added the repeated-measures ANOVA family (within-subject main effect and
the within x between interaction of a mixed design).

## [0.9.0] - 2026-08-03

Added the Wilcoxon-Mann-Whitney (nonparametric) family, parameterized on
the probability of superiority rather than Cohen's *d*.

## [0.8.0] - 2026-08-03

Reframed the clustered family around interacting lab sessions / matching
groups, in addition to cluster-randomized field trials.

## [0.7.1] - 2026-08-03

Added PNG/SVG export for the power curve.

## [0.7.0] - 2026-08-03

Extended the scenario comparison on the results step to a third axis
(effect size), alongside alpha and power.

## [0.6.0] - 2026-08-03

Added the time-to-event (two-group log-rank test) family.

## [0.5.0] - 2026-08-03

Added the one-way ANCOVA family (group comparison adjusting for one
covariate).

## [0.4.7] - 2026-08-03

Polished the boot-time loading splash shown during the (unavoidable)
first-visit webR/package download in the browser-only build.

## [0.4.6] and earlier - 2026-08-03

Initial commit, consolidating everything built before the August 2026
history rebuild: the four-step wizard workflow, the plain-language decision
helper, 11 analysis families (two independent means, paired means, two
proportions, factorial ANOVA, multiple linear regression, logistic
regression, clustered designs, chi-square, correlation, McNemar's test, and
the TOST equivalence test), all three effect-size branches (Cohen's
conventions, safeguard power, SESOI), the shared Bonferroni
multiple-comparisons control, save/load/share-link project state, and
Guided Mode. The individual pre-rebuild commits this corresponds to
(originally tagged v0.1.0-v0.4.6) are no longer reachable from `main`, so
they are summarized here as one entry rather than itemized.
