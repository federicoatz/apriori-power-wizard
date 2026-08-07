# Changelog

All notable changes to the **A Priori Power Analysis Wizard** are documented here.
Format loosely follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
versioning follows [Semantic Versioning](https://semver.org/) (patch = fix,
minor = new feature/family, major reserved for a future breaking redesign).

> **Note on history.** The repository's git history was rebuilt from scratch in
> August 2026 for reasons unrelated to the application itself (see the Zenodo
> notes in `CITATION.cff`). Every entry below corresponds to a real tagged
> release on the current history; nothing has been renumbered or backdated.

## [1.2.3] - 2026-08-07

- **Fixed a rounding bug in `validation/eq2_audit.R`'s own summary line.**
  It stated the worst-case deviation across the four "exact" metrics as
  "within 0.09%", produced by `sprintf("%.2f%%", ...)` rounding the true
  0.0929% figure (log hazard ratio) to the nearest hundredth. For a
  *bound* claim, rounding to nearest can understate it -- 0.0929% is not,
  in fact, within 0.09%. The summary now rounds up, printing "within
  0.093%", a figure the true maximum actually satisfies. The accompanying
  paper is corrected to match; nothing about the audit's underlying
  numbers changed.

## [1.2.2] - 2026-08-07

No change to what the application computes or displays; this ships the
audit behind a quantitative claim in the accompanying paper, and corrects
that claim.

- **New `validation/eq2_audit.R`.** The working paper states a closed-form
  approximation for the safeguard branch's cost relative to a stated
  SESOI, together with an accuracy claim. Auditing it turned up that the
  claim was too strong: it holds comfortably (within 0.093%) for the four
  metrics whose required N is a constant over the squared effect --
  Cohen's *h*, *w*, the log hazard ratio, and the probability of
  superiority added in v1.2.0 -- but not for *r* or *d*, where the
  deviation is systematic rather than a rounding artifact and reaches
  3.6% and 7.9% respectively at large effects. The script measures each
  metric separately, twice (on fractional sample sizes where rounding is
  exactly zero, and end-to-end through this app's solvers at a sample
  size large enough for rounding to be negligible), and prints each
  configuration's rounding floor so the two can be told apart. A third
  block re-derives the *d* column under the pure normal approximation and
  returns exactly zero deviation, locating the gap in the solver being
  more exact than the approximation assumes rather than in the
  relationship itself. The manuscript has been corrected accordingly.
  The approximation errs toward recruiting more participants, never
  fewer, in every case measured.
- `VALIDATION.md` documents the audit alongside the existing Monte Carlo
  and decision-helper checks, and its test-count reference is updated to
  match the current suite.

## [1.2.1] - 2026-08-07

- **The Step-1 decision helper no longer sits buried at the bottom in
  Guided mode.** It used to be a collapsed accordion, visually equal in
  weight to "start from a worked example" and below the fold under the
  full 16-option picker -- exactly backwards for the visitors most likely
  to need it. With Guided mode on, it now opens automatically and moves
  to the top of the page, framed with the same accent highlight as the
  "New here?" banner; the worked-example accordion stays put, collapsed,
  at the bottom, where it already made sense. Default (non-Guided) mode
  is untouched: same order, same collapsed accordions, byte-for-byte.
  Implemented as a pure CSS `order` override on a shared flex container
  plus a client-side call to Bootstrap's own Collapse API (so the header
  button's aria-expanded state and chevron stay correct, and a manual
  click still closes it normally) -- no server round trip, and the
  decision helper's server-bound inputs stay a single DOM instance rather
  than being duplicated for the two layouts.

## [1.2.0] - 2026-08-06

Minor: two new user-facing capabilities and three new exported functions.

- **The Wilcoxon-Mann-Whitney safeguard branch now works entirely on the
  test's native scale.** It used to ask for a published Cohen's d, build
  the confidence interval on the d scale, and convert to P(X < Y) at the
  end -- quietly assuming the normality the rank test is chosen to avoid,
  and contradicting the app's own guidance. The published statistic can
  now be entered as P(X < Y) itself, as the Mann-Whitney U (whose
  U/(n1*n2) *is* the sample P(X < Y), assumption-free), or as the
  reported z (inverted through the test's own large-sample
  approximation); a published d remains available as a labelled last
  resort, where only the point conversion -- not the interval -- is
  normal-theory. The interval behind the safeguard bound is built
  directly on the P(X < Y) scale by the new `safeguard_ci_auc()`
  (Hanley & McNeil, 1982, via the AUC identity), with the same
  never-cross-the-null clamping as every other safeguard metric. The
  Hanley-McNeil standard error was checked against Monte Carlo sampling
  of U/(n1*n2) (within 2% at n = 25-100 per group). New conversions
  `u_to_p_superiority()` / `z_to_p_superiority()` are exported and
  unit-tested. Older saved projects that stored a published d for this
  family will ask to be re-entered rather than silently reinterpreting
  the value.
- **Šidák joins Bonferroni as a multiplicity-correction method.** A new
  "Correction method" choice in every family's Advanced options: alpha/k
  (default) or 1-(1-alpha)^(1/k), which is exact under independence and
  never demands a larger sample. The results banner and the generated
  report text name whichever method produced the per-test alpha. Older
  saved projects restore as Bonferroni, which was the only behaviour
  before.
- **Protective odds ratios are accepted in the logistic family.** The
  safeguard and SESOI odds-ratio inputs rejected values below 1, walling
  off half the literature with no explanation; both now accept them, the
  direction is preserved through the Chinn (2000) conversion and the
  safeguard shrink (toward 1, never across -- mirroring the hazard-ratio
  guarantee), and the inputs say so.
- The safeguard branch now states, per family, how a negatively-signed
  published effect should be entered (as a magnitude where the scale is
  unsigned; with its sign where it matters, as in correlation/regression).
- **New browser flow test in CI** (`tests/e2e/flow_test.R`): drives the
  real app through the full four-step wizard in headless Chrome and
  asserts the rendered banner, report, and figures match the solvers --
  the layer unit tests cannot see. Covers the multiplicity banner
  (Bonferroni and Šidák, including the reactive switch) and the new
  Wilcoxon z-to-safeguard route.
- Reference-value test coverage widened: correlation and two-proportions
  now cross-check a 30-point grid against `pwr` (plus one-sided and
  unbalanced cases) rather than a single benchmark each.

## [1.1.1] - 2026-08-06

- **Analytics were being sent to a GoatCounter site that does not exist.**
  The deployed page injected `federicoatz.goatcounter.com/count`, but the
  site registered on GoatCounter is `experimenter.goatcounter.com` -- so
  every request since the snippet was enabled returned 400 and no visit
  was ever recorded. The site code in `deploy/export_shinylive.R` now
  matches the registered one; verified by hitting both endpoints (the old
  code answers 400 "no such site", the correct one accepts the hit).

## [1.1.0] - 2026-08-06

Minor rather than patch: this adds user-facing output and two exported
functions rather than only correcting behaviour.

- **The multiple-comparisons correction now shows what it costs, not just
  its result.** When more than one comparison is planned, every solver
  receives a per-test alpha and the corrected N was the only figure the
  user ever saw, which absorbs the cost of multiplicity into a number they
  cannot decompose. The results step now solves a second time at the
  nominal alpha and reports both requirements side by side with the
  difference between them, mirroring what the safeguard branch already did
  for its own naive comparison. The generated report text states both as
  well, so a reader of a pre-registration can reconstruct the adjustment
  instead of taking it on trust.
  - No family-specific code was needed: the second solve reuses each
    family's own `solve_n_fn()`, the same entry point already backing the
    alpha/power comparison scenarios, and a new shared `display_n()` helper
    guarantees the two figures are the same quantity the value box reports
    rather than a total set against a per-cell N.
  - Verified in a real browser across the four interactions that could go
    wrong: absent at one comparison; both figures shown at two; attrition
    and the budget panel following the corrected figure (and saying so);
    and fully suppressed, along with the value boxes and the report, when
    the safeguard bound has collapsed.

## [1.0.6] - 2026-08-05

Follow-up review of the v1.0.5 fixes found that two of them were partial,
and widening the property generator then surfaced three further defects of
the same class.

- **The suppressed sample size was only suppressed in two places.**
  v1.0.5 hid the recommendation and the achieved-power box when the
  safeguard bound collapsed, but `safeguard_diverged()` was consumed at
  exactly those two call sites. The attrition note, the budget panel, the
  sensitivity analysis, both power curves and -- worst -- the paste-ready
  report text and both downloadable reports all still emitted the diverged
  figure. The report is precisely the artefact that ends up in a
  pre-registration, so the number removed from the screen reappeared in the
  text the user copies. All eleven call sites are now guarded.
- **Any allocation ratio below about 0.6 crashed two families outright.**
  `power_two_means_n()` and `power_proportions_n()` both searched for the
  smallest per-arm N by scanning `for (cand in 2:100000)`, which makes
  `n2 = 1` on the first iteration whenever the ratio is under 1; `pwr`
  rejects that with "number of observations in the second group must be at
  least 2". The interface permits ratios in [0.1, 10], so an ordinary
  design ("my control arm is twice my treatment arm") failed with an opaque
  error. Both now start from the closed-form solution and refine, which
  also removes up to 100,000 `pwr` calls per solve -- a frozen tab under
  webR, where R itself runs in the browser.
- **`round_up_n()` returned NA instead of failing above integer range.**
  `as.integer()` overflows past 2,147,483,647 with only a warning, so a
  diverged sample size became a silent NA that surfaced later, far from its
  cause, as "missing value where TRUE/FALSE needed". It now raises the same
  explicit error as `assert_solvable_n()`. McNemar's test additionally
  checks before rounding, for a message naming its own inputs.
- **`pwr` overflowed internally on large unbalanced designs.** It computes
  `n1 * n2` in whatever type it is handed; every call site now passes
  doubles.
- **Documented that `$lower`/`$upper` are raw interval endpoints**, not
  safeguard bounds: for a negatively signed estimate `$lower` is the edge
  *away* from the null. The field is not consumed anywhere in the app, but
  it is exported under an MIT licence. The test that asserted
  `d_safeguard == lower` is now scoped explicitly to the positive
  half-plane and paired with a negative-estimate case, since it previously
  certified the raw endpoint as if it were the safeguard bound by only ever
  looking where the two coincide.
- **The property generator now generates where it needs to.** It sampled
  effect sizes from `runif(0.05, 1.2)` -- the region where nothing goes
  wrong -- under a test named "never NA, NULL, or a hang", with the
  pathological region covered by eight hand-written calls: a value-based
  test wearing a property-based test's name. Sampling is now log-uniform
  down to 1e-5, all sixteen families have adapters (was eleven), and the
  allocation ratio is sampled rather than left at its default, which is how
  the crash above had stayed invisible. A new invariant separates
  "legitimately refuses a degenerate input" from "refuses an ordinary
  design"; it found the two-means crash immediately.
- Suite is 1,949 assertions (from 1,425), still a few seconds.

## [1.0.5] - 2026-08-05

- **When the safeguard bound collapses onto the null, no sample size is
  shown at all.** v1.0.4 clamped the bound so it could not cross the null,
  which was correct, but the clamped value still flowed into the solver:
  eight of the sixteen families then errored (caught, explained) while two
  returned a finite figure in the hundreds of millions and displayed it
  next to the warning. That repeated the original failure in a new shape --
  a number the user cannot account for, differing only in being large
  rather than wrongly signed. The recommendation and the achieved-power box
  are now suppressed in this state, leaving only the sentence: this prior
  study is too imprecise to support a safeguard input at this confidence
  level.
- **Added a property-based test layer**
  (`tests/testthat/test-properties.R`). The three defects fixed in v1.0.3
  and v1.0.4 all survived a suite of value-based tests, because a pinned
  reference value can only fail at the point it pins and every one of those
  points was plausible; the failures lived in the tails. The new layer
  samples input tuples at random across families and asserts invariants
  instead: sign preservation, shrinkage toward the null, finite-or-explicit
  failure from every solver, and monotonicity of N in effect size, alpha
  and power. Re-running the pre-fix code against these reproduces the
  hazard-ratio sign flip in ~8% of random draws and the correlation-sign
  defect in ~50%, so either would have failed on the first CI run.
- Suite is now 1,425 assertions (from 460), still under 5 seconds.

## [1.0.4] - 2026-08-05

Two silent-wrong-answer bugs in the safeguard-power branch, both of which
returned a plausible number rather than failing.

- **A safeguard bound could cross the null and flip the sign of the
  effect.** The bound is the confidence-interval edge that shrinks the
  effect, but nothing stopped it crossing zero. For the hazard-ratio
  family, which has no floor, a published `HR = 0.85` from 40 events came
  back as a safeguard `HR = 1.11` *at the 80% default* -- a finite,
  entirely reasonable-looking sample size for detecting harm when the
  user was planning for benefit. Bounds are now clamped just short of the
  null on the side they started from.
- **A negatively signed estimate was shrunk away from the null, not
  toward it.** All five constructions took the *lower* bound
  unconditionally, so a published `r = -.30` became `-.40` (a larger
  effect, the opposite of the correction's purpose) before a
  `max(bound, 1e-4)` floor wiped it out to `+0.0001`. Both are fixed by a
  single shared helper, `safeguard_shrink()`, which always moves toward
  the null from whichever side the estimate is on.
- **Diverged sample sizes no longer hang the app.** Three families
  (survival, Wilcoxon-Mann-Whitney, ANCOVA) refined a closed-form
  starting value upward one participant at a time with no upper bound, so
  a near-null effect -- now reachable precisely *because* the clamp above
  works correctly -- walked forever instead of returning. They now fail
  fast via `assert_solvable_n()`, and ANCOVA additionally translates
  `pwr`'s opaque "f() values at end points not of opposite sign" into the
  same message. The results step's guard turns that into an explanation.
- 23 regression tests pin all of the above (460 total, from 437).

Published values in the manuscript are unaffected: every positively
signed case is numerically unchanged.

## [1.0.3] - 2026-08-05

- **Guard the safeguard-power branch against an uninformative prior
  study.** The safeguard bound is `estimate - z * SE`, so it collapses
  toward zero as the *original* study's t statistic approaches z, and the
  required N (which goes as 1/effect²) diverges. At the 80% default this
  needs t <= 0.84 and is unreachable for a published result, but the
  confidence level is a user-facing slider: at 95% the threshold rises to
  t <= 1.65, which a marginally significant prior study can sit below.
  Both failure modes were silent -- either an absurd recommendation
  (N = 12,968 for d = 0.40 from n = 50/group at 95%) or a root-finder
  error that the surrounding `tryCatch` swallowed, leaving a blank panel.
  The results step now detects this and explains that the prior study is
  too imprecise to power from at the requested level, pointing to the
  lower default or the SESOI branch. Checked on the solved N rather than
  on any one effect-size metric, so it covers all sixteen families from
  one place.
- **Flag the normality assumption in the Wilcoxon-Mann-Whitney
  d-to-p converter.** The power formula is close to invariant to the
  shape of the outcome distribution once P(X<Y) is fixed, which is why
  this family is parameterized on it. But `d_to_p_superiority()` is
  `pnorm(d/sqrt(2))`, a normal-theory identity, so a user arriving via a
  published Cohen's d silently re-imports at the conversion step exactly
  the assumption the rest of the route avoids. The effect-size step now
  says so and points to stating P(X<Y) directly.

## [1.0.2] - 2026-08-05

- Link the SSRN working paper describing the tool's design and validation
  from the README, `CITATION.cff` (as a `references` entry), and the app's
  own footer.

## [1.0.1] - 2026-08-05

Reviewer-facing polish: a faster path to running the app locally, and the
metadata files reviewers look for by convention.

- **README**: added a "Quick start" section right after the title (clone,
  `renv::restore()`, `runApp()`) so a reviewer doesn't have to read the
  full "Reproducibility" section just to get the app running, plus three
  new badges (CI status, license, minimum R version) alongside the
  existing Zenodo DOI badge.
- **Added `CONTRIBUTING.md`**: coding style, the file-per-family pattern
  for adding a new analysis family, how to run the tests, and the pull
  request process -- surfaced automatically by GitHub's issue/PR UI,
  unlike the equivalent prose that used to live only inside the README.
- **Added `DESCRIPTION`**: documents runtime dependencies (`Imports`/
  `Suggests`), license, and author metadata in the format reviewers
  expect from an R codebase, without turning this into an installable
  package. Fixed a real bug this introduced: `renv` infers a `package`
  vs. `project` layout from `DESCRIPTION`'s presence, and by default
  treats anything with a `Package:` field as a package, which switches
  `renv::restore()`/`renv::status()` to an external, cache-keyed library
  path instead of this repository's committed-empty, locally-populated
  `renv/library/` -- silently breaking the reproducibility workflow
  documented in "Reproducibility" for anyone who added a plain
  `DESCRIPTION`. Setting `Type: project` explicitly keeps `renv` on the
  original, project-local library path; `renv::status()` confirmed clean
  and the full test suite confirmed passing after the fix.

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
