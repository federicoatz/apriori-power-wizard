# Validation

This page documents two different things, checked two different ways:
that every one of the sixteen closed-form analysis families computes the
correct number, and that the decision helper routes a design description
to the correct family. Real tolerances and reproducible scripts for both
are below. This complements, and does not replace, the other sources of
truth in this repository: `tests/testthat/` (2,050+ assertions: pinned
regression tests plus property-based invariants,
run on every push and pull request -- see `.github/workflows/test.yml`),
`tests/e2e/flow_test.R` (the rendered wizard, driven in a real headless
browser, checked against the solvers),
`validation/monte_carlo_validation.R` (formula correctness),
`validation/scenario_validation.R` (decision-helper correctness), and
`validation/eq2_audit.R` (the safeguard-markup approximation).

**Scope.** "The formulas are correct" is established here,
deterministically and reproducibly. The decision helper is a weaker case
and the difference matters, so it is stated plainly rather than folded in.

`scenario_benchmark.csv` stores each scenario's answers **already coded**
into the helper's own categories (`dh_unit`, `dh_outcome`, ...) next to the
family a methodologist would expect. `recommend_family()` then maps the
first to the second through sixteen branches containing no arithmetic. So
what the benchmark shows is that two columns of one table agree under a
deterministic lookup: true and reproducible, but close to uninformative,
because the step that carries the actual risk is the one it skips. Reading
a study description and judging that its unit of assignment is a cluster
and its outcome continuous is exactly what a first-time user has to do and
can get wrong -- and in every row of that file, we did it ourselves before
the test ran.

It still earns its place: exhaustive enumeration over the same logic found
a real gap (bivariate correlation was unreachable, see below), and it fails
CI if the routing regresses. But it is an internal-consistency check, not a
validation of the routing. Closing the gap means collecting the answers
rather than supplying them -- giving published Method sections to readers
who did not write the router and recording how they code each design. That
has not been done. Until it is, treat the routing as internally consistent,
and treat "a first-time user, unassisted, navigates this successfully" as
an untested design claim, described as such in the Limitations section of
the accompanying manuscript.

## Decision helper: scenario benchmark

The decision-tree logic behind Step 1's "Not sure which analysis to
choose?" helper is factored out of the Shiny app into a pure function,
`recommend_family()` in `R/decision_helper.R`, that takes the same four
answers a user gives (independent vs. interacting participants; outcome
type; how the outcome is explained) and returns a family key -- no
Shiny session required, so it can be checked directly.

`validation/scenario_benchmark.csv` lists seventeen scenarios: the
answers a researcher would give, and the family a methodologist would
expect. An exhaustive enumeration of every answer combination (see the
header of `validation/scenario_validation.R`) shows the decision helper
now reaches fifteen of the sixteen closed-form families; the one it
cannot reach at all is the TOST equivalence test, excluded by design --
equivalence testing is a specific analytical intent a researcher already
has to know they want, not something inferable from "how is your outcome
measured?"

That fifteenth family, bivariate correlation, was a genuine gap until
this exercise found it by exhaustive search: nothing in the decision tree
routed to it, and the closest existing path ("a continuous predictor,
possibly with other control variables") routed to multiple regression
instead -- a narrower, and often less common, first request than simple
correlation. The fix was to split that question's option into two
explicit choices ("whether it's related to another continuous variable,
no other variables involved" vs. "a continuous predictor, controlling for
other variables") rather than leave correlation reachable only through
the family cards on Step 1.

Nine of the seventeen benchmark rows are the exact worked examples
already shipped in the app (`R/example_library.R`, reachable from Step
1's own "Try a worked example" entry point), reused here rather than
invented separately, so the same scenario is checked twice, two different
ways. The other eight extend coverage to the remaining reachable families
(including the newly-reachable correlation family) and to the one
combination the helper declines outright: a clustered/interacting design
with a time-to-event outcome, a deliberate scope boundary (combining the
design-effect correction with an events-based sample size is meaningfully
more involved than either piece alone) rather than an oversight.

```bash
Rscript validation/scenario_validation.R
```

Unlike the Monte Carlo checks above, this one is a real pass/fail gate
(exits 1 on any mismatch), because a decision-tree mapping is either
right or wrong -- there is no equivalent of "known approximation,
disclosed to the user." The same checks also run as regular `testthat`
assertions (`tests/testthat/test-decision_helper.R`), including an
exhaustive-enumeration test asserting the reachable-family set exactly
matches "all sixteen except TOST," so a change to the decision tree that
breaks any of them, or silently drops a family's reachability, fails CI
on every push, not just when someone remembers to run the standalone
script.

Last verified: 2026-08-05, app version 1.0.0 -- 17/17 scenarios routed
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
| 15 | Wilcoxon-Mann-Whitney | Monte Carlo (`stats::wilcox.test()`), under normal parents and, separately, at fixed `p` across normal/logistic/Laplace parents | see script output below | Asymptotic formula, mildly conservative throughout; power is close to invariant to parent shape once `p` is held fixed, which is what justifies parameterizing on `p = P(X<Y)` rather than on Cohen's `d` |
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

Captured 2026-08-05, app version 1.0.2, R 4.5.3 (re-run to confirm: the
seed is fixed, so these exact numbers are reproducible). Re-run the
command above for a current result; this snapshot exists so a reader can
see what a passing run looks like without installing R first.

```
A Priori Power Analysis Wizard -- Monte Carlo validation
App version: 1.0.2 | R version: R version 4.5.3 (2026-03-11)

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

Wilcoxon-Mann-Whitney: parent-shape robustness at fixed p_sup
  [OK   ] Wilcoxon-MW parent           normal n1=40 p_sup=0.65 (shift=0.545)  formula=0.6420 sim=0.6468 diff=-0.0048 (2*SE=0.0123, reps=6000)
  [OK   ] Wilcoxon-MW parent           logistic n1=40 p_sup=0.65 (shift=0.510) formula=0.6420 sim=0.6447 diff=-0.0027 (2*SE=0.0124, reps=6000)
  [WATCH] Wilcoxon-MW parent           laplace n1=40 p_sup=0.65 (shift=0.446) formula=0.6420 sim=0.6580 diff=-0.0160 (2*SE=0.0122, reps=6000)
  [WATCH] Wilcoxon-MW parent           normal n1=40 p_sup=0.70 (shift=0.742)  formula=0.8725 sim=0.8883 diff=-0.0158 (2*SE=0.0081, reps=6000)
  [WATCH] Wilcoxon-MW parent           logistic n1=40 p_sup=0.70 (shift=0.697) formula=0.8725 sim=0.9000 diff=-0.0275 (2*SE=0.0077, reps=6000)
  [WATCH] Wilcoxon-MW parent           laplace n1=40 p_sup=0.70 (shift=0.617) formula=0.8725 sim=0.8838 diff=-0.0113 (2*SE=0.0083, reps=6000)

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

Seven of twenty-four checks landed in the `WATCH` band (McNemar at small
n, Wilcoxon at one configuration, and four of the six parent-shape
checks). All are small (1-3 percentage points), and -- the part that
matters -- every one of them has the simulated power *above* the formula's
prediction, meaning the formula asks for a slightly larger sample than
strictly necessary. That is the direction the source code already
documents for these large-sample approximations, and the safe direction
for a planning tool to err in. Re-running with a different seed moves
which specific configurations land in `WATCH` without changing that
picture. None involve a family validated primarily against `pwr` (rows 1-9
above), where the tolerance is exact rather than simulation-based.

The parent-shape block is a deliberately non-circular test of the
Wilcoxon-Mann-Whitney family's *parameterization* rather than of its
arithmetic. Because the formula takes p = P(X<Y) rather than Cohen's d,
the claim that it needs no normality assumption can only be checked by
holding p fixed and varying the shape of the parent distribution:
comparing shapes at fixed d would be near-tautological, since heavier
tails at fixed d simply imply a different p. For each parent the location
shift is therefore solved numerically so that p is exactly on target. The
result is that power is close to invariant to parent shape (.888/.900/.884
for normal/logistic/Laplace at p = .70), which is the property that makes
p the defensible input for a rank test.

## Property-based tests

The checks above, and the `testthat` files behind them, are all
*value-based*: they pin a computed number at a chosen input. That layer is
necessary and it is what establishes the formulas are right, but it has a
structural blind spot -- a pinned value can only fail at the point it pins,
and the points anyone thinks to pin are the plausible ones.

Three defects reached a release despite it, all of them in the tails of the
input space rather than anywhere exotic:

- a safeguard bound that could cross the null and hand back a sample size
  for an effect of the **opposite sign** (a published `HR = 0.85` came back
  as a safeguard `HR = 1.11`, at the default confidence level);
- a negatively signed published estimate shrunk *away* from the null rather
  than toward it (`r = -.30` became `-.40`);
- refinement loops with no upper bound that did not terminate at all on a
  near-null effect.

`tests/testthat/test-properties.R` adds a second layer that samples input
tuples at random and asserts invariants instead of values. Four are enough
to close the class those three belong to:

| Invariant | Catches |
|---|---|
| `sign(corrected) == sign(published)` | the sign flip |
| `abs(corrected) <= abs(published)` | shrinking away from the null |
| every solver returns a finite positive N **or raises explicitly** | unbounded loops, silent `NA` |
| N is monotone in effect size, alpha, and power | solver monotonicity breaks |

These are not decorative. Re-running the pre-fix code against them
reproduces the hazard-ratio sign flip in roughly 8% of random draws and the
correlation-sign defect in roughly 50% -- either would have failed on the
first CI run. The seed is fixed so a failure is reproducible; raising `REPS`
in that file searches harder before a release.

## Safeguard-markup relationship (the manuscript's Equation 2)

The accompanying working paper states a closed-form approximation for what
the safeguard branch costs relative to a directly stated SESOI, depending
on nothing but the original study's *t* statistic and the chosen
confidence level. Because that claim is quantitative, it is auditable
rather than asserted:

```bash
Rscript validation/eq2_audit.R
```

The script measures the deviation metric by metric, twice: once on
fractional sample sizes (via `pwr`, where integer rounding is exactly
zero) and once end-to-end through this app's own solvers at a sample size
large enough that rounding is negligible, printing each configuration's
rounding floor alongside its residual so the two can be told apart.

As of v1.2.2 it reports agreement within **0.09%** for the four metrics
whose sample size is a constant over the squared effect (*h*, *w*,
log HR, and the probability of superiority), and a systematic,
effect-size-dependent gap for the other two: up to 3.6% for *r* (from the
additive `+3` term in its sample-size formula, shrinking as N grows) and
up to 7.9% for *d* at very large effects (because the app solves the exact
noncentral *t* rather than the normal approximation the proportionality
assumes). In both cases the approximation *overstates* the requirement,
so it errs toward recruiting more rather than fewer participants. A third
block re-derives the *d* column under the pure normal approximation and
returns exactly zero deviation, which locates the gap in the solver being
more exact than the approximation assumes rather than in the relationship
itself.

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
