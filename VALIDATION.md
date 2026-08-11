# Validation

This page documents two different things, checked two different ways:
that every one of the sixteen closed-form analysis families computes the
correct number, and that the decision helper routes a design description
to the correct family. Real tolerances and reproducible scripts for both
are below. This complements, and does not replace, the other sources of
truth in this repository: `tests/testthat/` (2,792 assertions as of
v1.5.1: pinned regression tests plus property-based invariants,
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

Last verified: 2026-08-11, app version 1.5.1 -- 17/17 scenarios routed
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

One qualification to that, added in v1.3.0 and worth stating up front
because it is the only place this app knowingly departs from what `pwr`
would return. The clustered continuous family takes its noncentrality
from `pwr` as before, but evaluates power against a *t* on cluster-level
degrees of freedom rather than on the degrees of freedom `pwr` would
infer from the design-effect-deflated N. The two agree when there are
many clusters and diverge sharply when there are few large ones -- the
regime this app's audience actually works in. `pwr` is not wrong here;
it is answering the question it was asked, and the deflated N is the
wrong thing to ask it about df. The reference for that family is
therefore the noncentral *t* itself, checked exactly against `pwr` at
`cluster_size = 1`, where the two questions coincide.

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
| 8 | Clustered, continuous outcome | `pwr`-derived N &times; design effect for the noncentrality, then a noncentral *t* on **cluster-level** df (k&#8321;+k&#8322;&minus;2) built directly in base R | exact match against `pwr::pwr.t.test()` at `cluster_size = 1`; Monte Carlo agreement within `2*SE` at four configurations down to 4 clusters/arm (see script output below); `tolerance = 0.15` on the min-detectable-effect search | The design effect is exact algebra, but it only fixes the noncentrality. Handing the deflated N to `pwr` would also take the DEGREES OF FREEDOM from it, claiming more than the number of clusters supports -- optimistic by up to .136 at four clusters per arm. Since v1.3.0 the df come from the cluster count instead; see the header of `R/power_clustered.R`. The `cluster_size = 1` identity is an exact regression test, not a simulation |
| 9 | Clustered, binary/categorical outcome | `pwr`-derived N &times; design effect | `tolerance = 0.03` | Design effect is a documented first-order approximation to Rao-Scott (1984) for the categorical case. These two do **not** get the cluster-level df treatment applied to family 8, and cannot: `pwr.2p.test()` is a pure z-test with no df at all, and `pwr.chisq.test()` takes its df from the contingency table rather than from N. What remains is that both are large-sample approximations that get optimistic with few clusters, in a way no df substitution can correct -- stated in the header of `R/power_clustered_cat.R` |
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

Captured 2026-08-07, app version 1.4.0, R 4.5.3 (re-run to confirm: the
seed is fixed, so these exact numbers are reproducible). Every block
above the clustered one is byte-identical to the snapshot from app
version 1.0.2 -- none of the releases between the two touched the
formulas they check, and re-running at v1.3.0 reproduced them exactly.
The clustered block is new in v1.3.0: that family stopped being a pure
`pwr` wrapper in this release (see the degrees-of-freedom note above), so
it moved into the set that gets checked against a simulation of the
actual test. Re-run the command above for a current result; this snapshot
exists so a reader can see what a passing run looks like without
installing R first.

```
A Priori Power Analysis Wizard -- Monte Carlo validation
App version: 1.4.0 | R version: R version 4.5.3 (2026-03-11)

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

Clustered design, continuous outcome (Donner & Klar, 2000; cluster-level df)
  [OK   ] Clustered (continuous)       m=4 icc=0.30 k=31/arm d=0.50           formula=0.8024 sim=0.8064 diff=-0.0040 (2*SE=0.0072, reps=12000)
  [OK   ] Clustered (continuous)       m=8 icc=0.15 k=17/arm d=0.50           formula=0.7974 sim=0.7982 diff=-0.0008 (2*SE=0.0073, reps=12000)
  [OK   ] Clustered (continuous)       m=20 icc=0.05 k=7/arm d=0.50           formula=0.7852 sim=0.7899 diff=-0.0048 (2*SE=0.0074, reps=12000)
  [OK   ] Clustered (continuous)       m=30 icc=0.02 k=4/arm d=0.50           formula=0.7290 sim=0.7328 diff=-0.0038 (2*SE=0.0081, reps=12000)

  Pre-v1.3.0 calculation (df from the deflated individual N), same designs:
  [OK   ] Clustered (pre-1.3.0)        m=4 icc=0.30 k=31/arm d=0.50           formula=0.8092 sim=0.8064 diff=+0.0028 (2*SE=0.0072, reps=12000)
  [WATCH] Clustered (pre-1.3.0)        m=8 icc=0.15 k=17/arm d=0.50           formula=0.8155 sim=0.7982 diff=+0.0174 (2*SE=0.0071, reps=12000)
  [WATCH] Clustered (pre-1.3.0)        m=20 icc=0.05 k=7/arm d=0.50           formula=0.8450 sim=0.7899 diff=+0.0551 (2*SE=0.0066, reps=12000)
  [WATCH] Clustered (pre-1.3.0)        m=30 icc=0.02 k=4/arm d=0.50           formula=0.8646 sim=0.7328 diff=+0.1318 (2*SE=0.0062, reps=12000)
```

Seven of thirty-two checks landed in the `WATCH` band (McNemar at small
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

### The clustered block, read directly

The last two blocks are the evidence behind the v1.3.0 degrees-of-freedom
change, and they are printed side by side deliberately: the same seeded
simulation is compared against the current calculation and against the
one it replaced.

The current calculation lands within `2*SE` at all four configurations,
including four clusters per arm, and every deviation is negative --
simulated power slightly *above* the formula, the conservative direction.
The calculation it replaced agrees at 31 clusters per arm and then
degrades monotonically as the cluster count falls: **+0.017** at 17
clusters, **+0.055** at 7, **+0.132** at 4. Those are not sampling noise
against a margin of 0.006-0.007, and they are in the optimistic
direction -- the app would have promised .865 for a design delivering
.733.

Note also what is *not* claimed. The simulation draws normal data with
equal cluster sizes and a correctly specified compound-symmetry
structure, so it validates the degrees-of-freedom arithmetic, not the
robustness of a cluster-randomized design to unequal cluster sizes, a
misspecified ICC, or non-normal outcomes. Four clusters per arm remains
a fragile design whatever the formula says about it; what changed is
that the app no longer overstates how much power it buys.

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

The safeguard tests also target the collapse boundary directly. For confidence
levels .80, .90, .95, and .99, they construct balanced two-group effects with
the original-study statistic immediately below and above the relevant
`z_gamma` threshold. On both sides the corrected effect must retain its sign
and never grow; below the threshold it must reduce to the explicit near-null
floor rather than cross it. Before a release, the same property suite is run
with a wider seeded search, for example:

```bash
PROPERTY_REPS=300 PROPERTY_SOLVER_DRAWS=60 Rscript --vanilla tests/testthat.R
```

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

Re-run at v1.3.0 (output unchanged; nothing in this release touches the
safeguard branch) it reports agreement within **0.093%** for the four metrics
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
human.

**Colour contrast is checked separately, and more thoroughly, on every
push.** The axe-core run above only ever saw the deployed build's landing
page in the light theme, which left the dark theme and every step past
the first unchecked. `tests/e2e/flow_test.R` now scans every visible text
node across all four wizard steps in **both** themes and asserts WCAG AA
(4.5:1 normal text, 3:1 large). Two things it gets right that a naive
check does not: the measured foreground composites any inherited
`opacity` -- reading `color` alone scored the value-box labels 3.93:1
when white at opacity .75 over the tile is really 2.87:1 -- and the
background is resolved by walking up to the first non-transparent
ancestor. Both themes currently report zero failures, and the scan is
verified non-vacuous by disabling the fix and confirming it fails. What
introduced the defect it caught is documented in
[Results that changed between versions](#results-that-changed-between-versions)
and in the "Bootstrap variable bridge" comment in `www/styles.css`.

Run the axe-core audit yourself with:

```bash
Rscript deploy/export_shinylive.R   # build _site/ first if you haven't
Rscript deploy/serve_static.R _site 8921 &
Rscript validation/accessibility_check.R _site 8921
```

## Results that changed between versions

Every report this application generates is stamped with the app version
that produced it (`APP_VERSION`, see `R/report_text.R`). That is only
useful if there is somewhere to look up what a version boundary means, so
this section lists every release that changed a number the app reports,
and by how much. Anything not listed here left the calculations untouched.

If you are holding an older report and re-running it gives a different
sample size, this table is the first place to check -- **before** assuming
a bug.

| Change | Versions | Effect |
|---|---|---|
| Clustered continuous family moved from effective-individual to cluster-level degrees of freedom | up to 1.2.3 &rarr; 1.3.0 onward | **Larger N, sometimes substantially**, in the few-large-clusters regime. Negligible with many small clusters |
| `power_achieved` recomputed at the rounded N in multiple regression and balanced two proportions | up to 1.2.3 &rarr; 1.3.0 onward | N unchanged; the *reported power* rises slightly (e.g. .8000 &rarr; .8051), because it now describes the integer sample rather than the target that was requested |
| Attrition inflation applied to the design unit rather than the grand total | up to 1.3.0 &rarr; 1.4.0 onward | Recruitment target changes only when attrition > 0 **and** the design has structure (clusters, cells, groups, arms). Typically a few participants, always upward, so the design stays evenly divisible |
| Light-theme `--positive` darkened #00926F &rarr; #008062, and the value-box label lost its 0.75 opacity | up to 1.4.2 &rarr; 1.4.3 onward | Appearance only; no calculation touched. White-on-green was 3.93:1 at full opacity and 2.87:1 as actually rendered, both below WCAG AA |
| Bootstrap `--bs-*` variables bridged to the app's own tokens in dark mode | up to 1.4.2 &rarr; 1.4.3 onward | Appearance only. Previously any Bootstrap component the stylesheet did not restyle kept light-theme colours in dark mode -- inline `code` at 1.10:1, `helpText()` at 1.04:1, i.e. invisible |
| Multiple regression: covariates now consume residual degrees of freedom | up to 1.4.6 &rarr; 1.5.0 onward | **Larger N whenever covariates > 0**, by exactly the number of covariates, and the reported power falls into line with what that N delivers. The old figure was optimistic: at f&sup2; = 0.15 with ten covariates it reported .805 for a design giving .719 |
| Safeguard branch for within-subject designs now uses the paired variance | up to 1.4.6 &rarr; 1.5.0 onward | **Substantially smaller N** in the paired and repeated-measures families when the safeguard branch is used. The old route applied the two-independent-groups variance to n/2 per "group", inflating the standard error about twofold: at d&#8338; = 0.40 from 30 pairs it asked for 980 pairs where 138 are needed |

### The clustered change, quantified

This is the one worth stating precisely, because it is the only change in
the app's history that made a previously reported number **optimistic**
rather than merely imprecise. At $d = 0.5$, $\alpha = .05$, target power
.80:

| Cluster size | ICC | Clusters/arm (&le;1.2.3) | Power actually delivered | Clusters/arm (&ge;1.3.0) |
|---|---|---|---|---|
| 4 | .30 | 31 | .802 | 31 |
| 8 | .15 | 17 | .797 | 18 |
| 12 | .10 | 12 | .799 | 13 |
| 20 | .05 | 7 | .785 | 8 |
| 30 | .02 | 4 | **.729** | 5 |

A study planned with four large clusters per arm under v1.2.3 was told it
had 86.5% power for a design delivering 72.9%. With 31 small clusters the
two versions agree exactly. The reasoning, the cost of the change (ICC = 0 no longer
collapses to the individually-randomized answer), and the Monte Carlo
evidence are in the header of `R/power_clustered.R` and in the clustered
block of the simulation output above.

Note also what did **not** change: the clustered binary and categorical
families. They rest on normal and chi-square approximations with no
n-derived degrees of freedom to correct, so their numbers are identical
across all versions -- see the header of `R/power_clustered_cat.R` for why
that is a limitation rather than a clean bill of health.

## Running the full regression suite

```bash
Rscript -e 'source("global.R"); testthat::test_dir("tests/testthat")'
```

This is what `.github/workflows/test.yml` runs on every push and pull
request; a red suite blocks merging to `main` in spirit (there is
currently no branch-protection rule enforcing this automatically -- see
the repository's GitHub Settings if you want to add one).
