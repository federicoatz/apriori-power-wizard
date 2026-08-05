# Validation

This page documents two different things, checked two different ways:
that every one of the sixteen closed-form analysis families computes the
correct number, and that the decision helper routes a design description
to the correct family. Real tolerances and reproducible scripts for both
are below. This complements, and does not replace, the other sources of
truth in this repository: `tests/testthat/` (430+ pinned regression tests,
run on every push and pull request -- see `.github/workflows/test.yml`),
`validation/monte_carlo_validation.R` (formula correctness), and
`validation/scenario_validation.R` (decision-helper correctness).

**Scope.** "The formulas are correct" and "the decision helper's mapping
from design description to analysis family is correct" are both
established here, deterministically and reproducibly. Neither is the same
claim as "a first-time user, unassisted, successfully navigates this
interface" -- that remains a design claim, not a measured outcome, and is
described honestly as such in the "Limitations" section of the
accompanying manuscript (`manuscript/paper.tex`, not part of this public
repository -- see the paper once it is posted). What moved: the earlier
version of this page could only say the decision helper had never been
checked at all; it can now say the mapping itself -- given these exact
answers, does it recommend the family a methodologist would also pick? --
is checked against sixteen real experimental-economics/behavioral-science
scenarios, including every one currently shipped in the app's own worked-
example library. What has NOT moved: whether an actual first-time user
answers those questions correctly, unprompted, is still untested.

## Decision helper: scenario benchmark

The decision-tree logic behind Step 1's "Not sure which analysis to
choose?" helper is factored out of the Shiny app into a pure function,
`recommend_family()` in `R/decision_helper.R`, that takes the same four
answers a user gives (independent vs. interacting participants; outcome
type; how the outcome is explained) and returns a family key -- no
Shiny session required, so it can be checked directly.

`validation/scenario_benchmark.csv` lists sixteen scenarios: the answers a
researcher would give, and the family a methodologist would expect. An
exhaustive enumeration of every answer combination (see the header of
`validation/scenario_validation.R`) shows the decision helper can reach
fourteen of the sixteen closed-form families; the two it cannot reach at
all are the TOST equivalence test and bivariate correlation. TOST is
excluded by design -- equivalence testing is a specific analytical intent
a researcher already has to know they want, not something inferable from
"how is your outcome measured?" Correlation's absence was not a
deliberate design decision documented anywhere before this validation
exercise found it by exhaustive search; the closest existing path
("a continuous predictor" under the continuous-outcome branch) routes to
multiple regression, not to a simple bivariate correlation, which is a
narrower and arguably more common first request. Flagged here as a
genuine gap for the maintainer to decide on, not silently worked around.

Nine of the sixteen benchmark rows are the exact worked examples already
shipped in the app (`R/example_library.R`, reachable from Step 1's own
"Try a worked example" entry point), reused here rather than invented
separately, so the same scenario is checked twice, two different ways.
The other seven extend coverage to the remaining reachable families and
to the one combination the helper declines outright (a
clustered/interacting design with a time-to-event outcome -- see the
Discussion/Limitations section of the manuscript for why that is a
deliberate scope boundary).

```bash
Rscript validation/scenario_validation.R
```

Unlike the Monte Carlo checks above, this one is a real pass/fail gate
(exits 1 on any mismatch), because a decision-tree mapping is either
right or wrong -- there is no equivalent of "known approximation,
disclosed to the user." The same sixteen checks also run as regular
`testthat` assertions (`tests/testthat/test-decision_helper.R`), so a
change to the decision tree that breaks any of them fails CI on every
push, not just when someone remembers to run the standalone script.

Last verified: 2026-08-04, app version 0.17.0 -- 16/16 scenarios routed
to the expected family.

## Two validation methods, by family

Nine families reduce to a direct call to the `pwr` R package (seven
directly, plus two clustered families that inflate a `pwr`-derived N by a
design effect); for those, `pwr` itself -- an independently published,
widely used, separately validated package -- **is** the reference, so the
test suite checks for near-exact numerical agreement (`tolerance = 1e-8`
in most cases; a couple of families intentionally use a looser tolerance,
noted below, because they only wrap the `pwr`-derived number in an
additional integer-rounding or design-effect step).

Seven families are implemented directly in base R (no external
power-analysis package covers them, or the ones that did were removed for
browser-build reasons -- see `global.R`'s header comment). Their
closed-form formulas are published, but since nothing else in this app's
dependency tree computes the same quantity, they are checked against
**direct Monte Carlo simulation** of the actual test statistic instead:
simulate data under the assumed model many times, run the real
statistical test (`stats::mcnemar.test()`, `stats::wilcox.test()`,
`survival::survdiff()`, ...) on each replicate, and compare the empirical
rejection rate to the formula's predicted power.

| # | Family | Method | Tolerance / agreement | Notes |
|---|---|---|---|---|
| 1 | Two independent means | vs. `pwr::pwr.t.test()` | exact match on rounded N (textbook reference: d=0.5 &rarr; n=64/group) | |
| 2 | Paired / repeated measures (2 waves) | vs. `pwr::pwr.t.test(type="paired")` | `tolerance = 1e-8` | |
| 3 | Multiple linear regression | vs. `pwr::pwr.f2.test()` | exact match on rounded N | |
| 4 | Two independent proportions | vs. `pwr::pwr.2p.test()` | exact match on rounded N | |
| 5 | Bivariate correlation | vs. `pwr::pwr.r.test()` | exact match on rounded N | |
| 6 | Chi-square (goodness of fit / independence) | vs. `pwr::pwr.chisq.test()` | exact match on rounded N | |
| 7 | One-way ANCOVA | vs. `pwr::pwr.f2.test()` with covariate-adjusted f² | `tolerance = 1e-8` | Independent cross-check, not just an internal round-trip |
| 8 | Clustered, continuous outcome | `pwr`-derived N &times; design effect | `tolerance = 0.06` on the min-detectable-effect search | Design effect itself is exact algebra; tolerance reflects the search step, not the formula |
| 9 | Clustered, binary/categorical outcome | `pwr`-derived N &times; design effect | `tolerance = 0.03` | Design effect is a documented first-order approximation to Rao-Scott (1984) for the categorical case |
| 10 | Factorial ANOVA (between-subjects) | Monte Carlo (one-way case also cross-checked exactly against `pwr::pwr.anova.test()`) | see script output below | Originally also cross-validated during development against `Superpower`'s simulation output across twelve designs; `Superpower` is no longer a dependency (see `global.R`), so that specific historical check cannot be re-run from this repo, but the `pwr` cross-check for the one-way case is exact and reproducible |
| 11 | Logistic regression | vs. published benchmark table (Chinn, 2000) | `tolerance = 1e-5` | |
| 12 | McNemar's test | Monte Carlo (`stats::mcnemar.test()`) | see script output below | Large-sample approximation; small-n configurations can show a few points of deviation, disclosed in `R/power_mcnemar.R` |
| 13 | TOST equivalence test | Monte Carlo (two one-sided t-tests) | see script output below | |
| 14 | Repeated-measures ANOVA | Monte Carlo (manual sum-of-squares F-test under compound symmetry), plus an exact algebraic identity to the paired t-test at m=2 | see script output below | The m=2 identity is an exact regression test (`pwr::pwr.t.test(type="paired")` to 8 decimal places), not a simulation |
| 15 | Wilcoxon-Mann-Whitney | Monte Carlo (`stats::wilcox.test()`) | see script output below | Asymptotic formula; conservative under heavier-tailed (non-normal) parents, which is the case that motivates using this test in the first place |
| 16 | Time-to-event (log-rank test) | Monte Carlo (`survival::survdiff()` on simulated exponential survival times) | see script output below | Schoenfeld approximation; documented to become conservative for a strong hazard ratio combined with unbalanced allocation |

## Reproducing the Monte Carlo checks

```bash
# From the project root:
Rscript validation/monte_carlo_validation.R
```

Every simulation is seeded (`set.seed(20260804)`) and self-contained; the
script has no side effects on the app itself. It takes a few minutes to
run in full (McNemar and TOST alone run 8,000 replications per
configuration). Each line reports the formula's power, the simulated
rejection rate, their difference, and a rough Monte Carlo margin (2
standard errors); a difference outside that margin is flagged `WATCH`
rather than `FAIL`, because for McNemar and the log-rank family in
particular the closed-form relation is a **documented approximation** --
some configurations are expected to disagree by more than pure sampling
noise would predict, and that is the formula's known behavior, not a bug.

**Honesty note.** This script is a fresh, independently reproducible
validation, not a byte-for-byte replay of the original development-time
Monte Carlo runs described in the source code comments (e.g.
`R/power_rm_anova.R` references specific historical runs at 4,000
replications per configuration). Those runs' exact parameter grids and
seeds were never committed, so they cannot be reproduced verbatim --
which is precisely why this script exists: to give anyone, including a
future maintainer, a way to independently re-derive the same conclusion
from scratch rather than take the historical claim on faith.

### Last verified run

Captured 2026-08-04, app version 0.16.0, R 4.5.3. Re-run the command above
for a current result; this snapshot exists so a reader can see what a
passing run looks like without installing R first.

```
A Priori Power Analysis Wizard -- Monte Carlo validation
App version: 0.16.0 | R version: R version 4.5.3 (2026-03-11)

McNemar's test (Connor, 1987)
  [WATCH] McNemar                      n=100 p10=0.25 p01=0.10                formula=0.7241 sim=0.7384 diff=-0.0143 (2*SE=0.0098, reps=8000)
  [OK   ] McNemar                      n=200 p10=0.30 p01=0.15                formula=0.8913 sim=0.8936 diff=-0.0023 (2*SE=0.0069, reps=8000)
  [WATCH] McNemar                      n=60 p10=0.35 p01=0.10                 formula=0.8410 sim=0.8614 diff=-0.0203 (2*SE=0.0077, reps=8000)
TOST equivalence test (Schuirmann, 1987; Phillips, 1990)
  [OK   ] TOST                         n=60 delta=0.50 theta=0.00             formula=0.7106 sim=0.7188 diff=-0.0082 (2*SE=0.0101, reps=8000)
  [OK   ] TOST                         n=150 delta=0.30 theta=0.00            formula=0.6534 sim=0.6566 diff=-0.0032 (2*SE=0.0106, reps=8000)
  [OK   ] TOST                         n=80 delta=0.40 theta=0.10             formula=0.5259 sim=0.5335 diff=-0.0076 (2*SE=0.0112, reps=8000)
Wilcoxon-Mann-Whitney (Noether, 1987)
  [OK   ] Wilcoxon-Mann-Whitney        n1=40 p_sup=0.65                       formula=0.6420 sim=0.6468 diff=-0.0048 (2*SE=0.0123, reps=6000)
  [OK   ] Wilcoxon-Mann-Whitney        n1=60 p_sup=0.62                       formula=0.6243 sim=0.6150 diff=+0.0093 (2*SE=0.0125, reps=6000)
  [WATCH] Wilcoxon-Mann-Whitney        n1=30 p_sup=0.70                       formula=0.7653 sim=0.7768 diff=-0.0116 (2*SE=0.0108, reps=6000)
Repeated-measures ANOVA, within main effect (Cohen, 1988, ch. 8; G*Power 3 formulation)
  [OK   ] RM-ANOVA (within)            n=20 m=3 f=0.25 rho=0.50               formula=0.6505 sim=0.6548 diff=-0.0042 (2*SE=0.0150, reps=4000)
  [OK   ] RM-ANOVA (within)            n=15 m=4 f=0.25 rho=0.60               formula=0.6860 sim=0.6730 diff=+0.0130 (2*SE=0.0147, reps=4000)
  [OK   ] RM-ANOVA (within)            n=25 m=3 f=0.30 rho=0.30               formula=0.7733 sim=0.7690 diff=+0.0043 (2*SE=0.0132, reps=4000)
Time-to-event log-rank test (Schoenfeld, 1983)
  [OK   ] Log-rank                     n=300 hr=0.60 p_event=0.50 k=1.0       formula=0.8786 sim=0.8723 diff=+0.0063 (2*SE=0.0119, reps=3000)
  [OK   ] Log-rank                     n=200 hr=1.80 p_event=0.60 k=1.0       formula=0.8961 sim=0.8967 diff=-0.0006 (2*SE=0.0111, reps=3000)
  [OK   ] Log-rank                     n=250 hr=0.70 p_event=0.40 k=1.5       formula=0.4158 sim=0.4270 diff=-0.0112 (2*SE=0.0181, reps=3000)
Factorial ANOVA, one-way case, vs. pwr::pwr.anova.test() (Cohen, 1988, ch. 8)
  [OK   ] Factorial ANOVA (1-way)      k=3 n=20 f=0.25                        ours=0.374431 pwr=0.374431 diff=0.00e+00
  [OK   ] Factorial ANOVA (1-way)      k=4 n=15 f=0.40                        ours=0.708722 pwr=0.708722 diff=0.00e+00
  [OK   ] Factorial ANOVA (1-way)      k=5 n=10 f=0.30                        ours=0.324534 pwr=0.324534 diff=3.33e-16
```

Three of eighteen checks landed in the `WATCH` band (McNemar at small n,
Wilcoxon at one configuration); all three are small (1-2 percentage
points), all three are in the direction the source code already documents
(the formula being a large-sample approximation), and re-running with a
different seed moves which specific configurations land in `WATCH` without
changing that overall picture. None involve a family validated primarily
against `pwr` (rows 1-9 above), where the tolerance is exact rather than
simulation-based.

## Accessibility

Every build is also audited with [axe-core](https://github.com/dequelabs/axe-core)
(`validation/accessibility_check.R`, run as part of
`.github/workflows/deploy-shinylive.yml`, currently informational rather
than build-blocking). As of 2026-08-04 this catches one remaining issue --
`aria-prohibited-attr`, a byproduct of how Shiny's own `icon()` helper
annotates every icon, not something introduced by this app's code --
after missing `lang`, insufficient color contrast, missing landmarks, and
heading-order violations were all fixed (see that script's header comment
for exactly what changed). Automated tooling still cannot verify keyboard
navigation, screen-reader usability, or mobile layout, which need a
human. Run it yourself with:

```bash
Rscript deploy/export_shinylive.R   # build _site/ first if you haven't
Rscript deploy/serve_static.R _site 8921 &
Rscript validation/accessibility_check.R _site 8921
```

## Running the full regression suite

```bash
Rscript -e 'source("global.R"); testthat::test_dir("tests/testthat")'
```

This is what `.github/workflows/test.yml` runs on every push and pull
request; a red suite blocks merging to `main` in spirit (there is
currently no branch-protection rule enforcing this automatically -- see
the repository's GitHub Settings if you want to add one).
