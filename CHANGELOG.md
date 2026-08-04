# Changelog

All notable changes to the **A Priori Power Analysis Wizard** are documented here.
Format loosely follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
versioning follows [Semantic Versioning](https://semver.org/) (patch = fix,
minor = new feature/family, major reserved for a future breaking redesign).

> **Note on history.** The repository's git history was rebuilt from scratch in
> August 2026 for reasons unrelated to the application itself (see the Zenodo
> notes in `CITATION.cff`). Every entry below corresponds to a real tagged
> release on the current history; nothing has been renumbered or backdated.

## [Unreleased]

- Added a workflow-overview graphic to the README and moved manuscript
  drafts (`paper.tex`, the APA submission version, the bibliography) into a
  local-only, gitignored `manuscript/` folder so they stay out of the public
  repo history.
- Stamped every generated report and every saved/shared project file with
  the app version that produced it (`APP_VERSION` in `global.R`).
- Added CI: the full `testthat` suite now runs on every push and pull
  request, and the shinylive/webR export is smoke-tested in a real headless
  browser before deployment.

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
