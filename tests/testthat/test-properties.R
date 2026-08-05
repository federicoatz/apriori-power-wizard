## test-properties.R
## -----------------------------------------------------------------------
## Property-based tests: assert INVARIANTS over randomly sampled inputs
## rather than pinned values at hand-chosen points.
##
## Why this file exists. Three defects shipped despite a suite of 437
## value-based tests -- a safeguard bound that crossed the null and flipped
## the sign of the effect, a negatively signed estimate shrunk away from the
## null instead of toward it, and unbounded refinement loops that hung on a
## near-null effect. None was exotic, and all three were invisible to the
## existing tests for the same reason: a pinned reference value can only
## fail at the point it pins, and every one of those points was plausible.
## The failures all lived in the tails of the input space.
##
## Each property below is chosen so that it would have caught at least one
## of those three defects, and so that it holds for reasons a reader can
## check independently of the implementation:
##
##   sign(corrected) == sign(published)   catches the sign flip
##   |corrected| <= |published|           catches shrinking away from null
##   N finite or an explicit error        catches the unbounded loops
##   N non-increasing in |effect|         catches solver monotonicity breaks
##
## The seed is fixed so a failure is reproducible. REPS and SOLVER_DRAWS are
## sized for CI (the whole suite stays under a few seconds); raising them
## locally is the cheap way to search harder before a release.
##
## Reproducing the non-vacuity figures quoted in VALIDATION.md. Those rates
## -- the pre-fix hazard-ratio bound violating sign preservation in roughly
## 8% of draws, the pre-fix correlation bound in roughly 50% -- were
## measured at 300 draws, not at the REPS below. To reproduce them, set
## REPS <- 300 and re-run against the pre-fix implementations; at the
## default REPS the same defects still fail, just with a smaller sample
## behind the percentage.
## -----------------------------------------------------------------------

set.seed(20260805)
REPS <- 60            # draws per safeguard invariant
SOLVER_DRAWS <- 12    # draws per family for the solver invariants

# Draw a value uniformly, optionally allowing negatives, avoiding an exact
# zero (which is the null itself and not a meaningful published estimate).
runif_signed <- function(n, lo, hi, signed = FALSE) {
  v <- runif(n, lo, hi)
  if (signed) v * sample(c(-1, 1), n, replace = TRUE) else v
}

# --- Safeguard-bound invariants -----------------------------------------

test_that("safeguard bounds never flip sign and never grow, across the input space", {
  for (i in seq_len(REPS)) {
    conf <- runif(1, 0.55, 0.99)
    one_sided <- sample(c(TRUE, FALSE), 1)

    # d: signed, since a published mean difference can point either way
    d <- runif_signed(1, 0.01, 2.5, signed = TRUE)
    n1 <- sample(5:400, 1); n2 <- sample(5:400, 1)
    ds <- safeguard_ci_d(d, n1, n2, conf, one_sided)$d_safeguard
    expect_identical(sign(ds), sign(d), info = sprintf("d=%.4f n1=%d n2=%d conf=%.3f", d, n1, n2, conf))
    expect_lte(abs(ds), abs(d) + 1e-12)

    # h: signed
    h <- runif_signed(1, 0.01, 2.0, signed = TRUE)
    hs <- safeguard_ci_h(h, n1, n2, conf, one_sided)$h_safeguard
    expect_identical(sign(hs), sign(h))
    expect_lte(abs(hs), abs(h) + 1e-12)

    # r: signed, and shrinkage is asserted on the Fisher z scale where the
    # bound is actually taken (tanh is monotone, so it holds on r too)
    r <- runif_signed(1, 0.01, 0.95, signed = TRUE)
    nr <- sample(10:800, 1)
    rs <- safeguard_ci_r(r, nr, conf, one_sided)$r_safeguard
    expect_identical(sign(rs), sign(r))
    expect_lte(abs(rs), abs(r) + 1e-12)

    # w: a magnitude, so positive only
    w <- runif(1, 0.01, 1.2)
    nw <- sample(20:2000, 1); dfw <- sample(1:10, 1)
    ws <- safeguard_ci_w(w, nw, dfw, conf, one_sided)$w_safeguard
    expect_gt(ws, 0)
    expect_lte(ws, w + 1e-12)

    # hazard ratio: "sign" means which side of 1 it falls on
    hr <- exp(runif_signed(1, 0.01, 1.2, signed = TRUE))
    ev <- sample(10:600, 1)
    hrs <- safeguard_ci_logHR(hr, ev, conf_level = conf, one_sided = one_sided)$hr_safeguard
    expect_identical(hr > 1, hrs > 1,
                     info = sprintf("hr=%.4f events=%d conf=%.3f", hr, ev, conf))
    expect_lte(abs(log(hrs)), abs(log(hr)) + 1e-12)
  }
})

test_that("safeguard shrinkage increases with confidence and decreases with the original N", {
  for (i in seq_len(REPS)) {
    d <- runif(1, 0.1, 1.5)
    n <- sample(20:400, 1)
    # a stricter confidence level can only shrink the estimate further
    lo <- safeguard_ci_d(d, n, n, conf_level = 0.70)$d_safeguard
    hi <- safeguard_ci_d(d, n, n, conf_level = 0.95)$d_safeguard
    expect_lte(hi, lo + 1e-12)
    # a more precise original study can only shrink it less
    small <- safeguard_ci_d(d, n, n)$d_safeguard
    large <- safeguard_ci_d(d, n * 4, n * 4)$d_safeguard
    expect_gte(large, small - 1e-12)
  }
})

# --- Solver invariants ---------------------------------------------------

# Sampling is LOG-uniform and reaches down to 1e-5, not uniform on the
# plausible range. This matters more than it looks: the earlier version of
# this file drew effect sizes from runif(1, 0.05, 1.2), i.e. exactly the
# region where nothing goes wrong, while carrying a test named "never NA,
# NULL, or a hang". The pathological region was covered only by a short
# list of hand-written calls -- a value-based test wearing a property-based
# test's name, and the same limitation this file exists to escape.
rloguniform <- function(n, lo, hi) exp(runif(n, log(lo), log(hi)))

# All sixteen families. Adapters take (effect, alpha, power, ratio) so that
# allocation ratio is SAMPLED rather than left at its default -- the
# two-proportions family crashed outright for any ratio below about 0.6,
# and no test saw it because every adapter used balanced allocation.
solvers <- list(
  two_means       = function(e, a, p, k) power_two_means_n(e, a, p, allocation_ratio = k)$n_total,
  paired          = function(e, a, p, k) power_paired_t_n(e, a, p)$n_total,
  proportions     = function(e, a, p, k) power_proportions_n(h = e, sig_level = a, power = p,
                                                             allocation_ratio = k)$n_total,
  correlation     = function(e, a, p, k) power_correlation_n(min(e, 0.95), a, p)$n_total,
  chisq           = function(e, a, p, k) power_chisq_n(w = e, df = 3, sig_level = a, power = p)$n_total,
  regression      = function(e, a, p, k) power_regression_n(f2 = e^2, n_predictors_tested = 3,
                                                            sig_level = a, power = p)$n_total,
  ancova          = function(e, a, p, k) power_ancova_n(k = 3, f_target = e, r_cov = 0.4,
                                                        sig_level = a, power = p)$n_total,
  anova_factorial = function(e, a, p, k) power_anova_factorial_n(levels_a = 2, levels_b = 2,
                                                                 focal = "A", f_target = e,
                                                                 sig_level = a, power = p)$n_total,
  rm_anova        = function(e, a, p, k) power_rm_anova_n(m = 3, f_target = e, rho = 0.5,
                                                          sig_level = a, power = p)$n_total,
  logistic        = function(e, a, p, k) power_logistic_n(p0 = 0.3,
                                                          p1 = min(0.3 + e / 4, 0.95),
                                                          sig_level = a, power = p)$n_total,
  mcnemar         = function(e, a, p, k) power_mcnemar_n(p10 = 0.1 + e / 8, p01 = 0.1,
                                                         sig_level = a, power = p)$n,
  tost            = function(e, a, p, k) power_tost_n(delta_eq = e, theta = 0,
                                                      sig_level = a, power = p)$n,
  wilcoxon        = function(e, a, p, k) power_wilcoxon_n(p_sup = min(0.5 + e / 4, 0.999),
                                                          sig_level = a, power = p)$n_total,
  clustered       = function(e, a, p, k) power_clustered_n(d = e, icc = 0.05, cluster_size = 8,
                                                           sig_level = a, power = p)$n_total,
  clustered_cat   = function(e, a, p, k) power_clustered_cat_n(w = e, df = 3, icc = 0.05,
                                                               cluster_size = 8, sig_level = a,
                                                               power = p)$n_total,
  survival        = function(e, a, p, k) power_survival_n(hr = exp(e), p_event = 0.6,
                                                          alloc_ratio = k,
                                                          sig_level = a, power = p)$n_total
)

test_that("every family covers the whole input range without hanging or returning NA", {
  # The contract being asserted is deliberately weak on VALUE and strict on
  # BEHAVIOUR: either a finite positive N, or an explicit error. Never a
  # silent NA, never a non-finite number, and never a call that does not
  # return. Time is capped per call so a regression to an unbounded loop
  # fails the suite instead of stalling CI.
  for (nm in names(solvers)) {
    for (i in seq_len(SOLVER_DRAWS)) {
      e <- rloguniform(1, 1e-5, 2)
      a <- sample(c(0.001, 0.01, 0.05, 0.10), 1)
      p <- sample(c(0.70, 0.80, 0.90, 0.95), 1)
      k <- sample(c(0.1, 0.5, 1, 2, 10), 1)
      ctx <- sprintf("%s: e=%.2e a=%.3f p=%.2f k=%.1f", nm, e, a, p, k)

      t0 <- proc.time()[["elapsed"]]
      n <- tryCatch(solvers[[nm]](e, a, p, k), error = function(err) NA_integer_)
      expect_lt(proc.time()[["elapsed"]] - t0, 10, label = ctx)

      if (!is.na(n)) {
        expect_true(is.finite(n), info = ctx)
        expect_gt(n, 0)
      }
    }
  }
})

test_that("no family refuses an ordinary design", {
  # The invariant above deliberately accepts an explicit error, because a
  # diverged input SHOULD be refused. That tolerance has a cost: it would
  # equally accept a family that fails on a perfectly reasonable design.
  # This test draws the line. Inside the plausible region -- an effect a
  # researcher would actually plan for, at any allocation ratio the
  # interface permits -- a solver must SUCCEED, not merely fail tidily.
  #
  # This is the property that the two-proportions family violated: every
  # allocation ratio below about 0.6 raised "number of observations in the
  # second group must be at least 2" from inside pwr, for inputs as
  # ordinary as "my control arm is twice my treatment arm".
  for (nm in names(solvers)) {
    for (e in c(0.2, 0.5, 1.0)) {
      for (k in c(0.1, 0.5, 1, 2, 10)) {
        ctx <- sprintf("%s: e=%.2f ratio=%.1f", nm, e, k)
        n <- tryCatch(solvers[[nm]](e, 0.05, 0.80, k),
                      error = function(err) structure(NA_integer_, msg = conditionMessage(err)))
        expect_false(is.na(n),
                     info = sprintf("%s refused an ordinary design: %s", ctx, attr(n, "msg")))
      }
    }
  }
})

test_that("required N is non-increasing in the effect size, for every family", {
  for (nm in names(solvers)) {
    grid <- seq(0.15, 1.0, length.out = 6)
    ns <- vapply(grid, function(e) tryCatch(as.numeric(solvers[[nm]](e, 0.05, 0.80, 1)),
                                            error = function(err) NA_real_), numeric(1))
    ns <- ns[!is.na(ns)]
    if (length(ns) >= 2) {
      expect_true(all(diff(ns) <= 0),
                  info = sprintf("%s not monotone in effect: %s", nm, paste(ns, collapse = ", ")))
    }
  }
})

test_that("required N moves the right way with alpha and with power, for every family", {
  for (nm in names(solvers)) {
    e <- 0.5
    strict <- tryCatch(as.numeric(solvers[[nm]](e, 0.01, 0.80, 1)), error = function(err) NA_real_)
    loose  <- tryCatch(as.numeric(solvers[[nm]](e, 0.10, 0.80, 1)), error = function(err) NA_real_)
    if (!is.na(strict) && !is.na(loose)) {
      expect_gte(strict, loose, label = sprintf("%s alpha=.01 N", nm))
    }
    hi <- tryCatch(as.numeric(solvers[[nm]](e, 0.05, 0.95, 1)), error = function(err) NA_real_)
    lo <- tryCatch(as.numeric(solvers[[nm]](e, 0.05, 0.70, 1)), error = function(err) NA_real_)
    if (!is.na(hi) && !is.na(lo)) {
      expect_gte(hi, lo, label = sprintf("%s power=.95 N", nm))
    }
  }
})

test_that("the near-null region specifically resolves or raises, for every family", {
  # Drawn from the region the safeguard clamp can actually reach, rather
  # than listed by hand: the point is coverage of all sixteen families, not
  # of the eight someone remembered to type out.
  for (nm in names(solvers)) {
    for (e in c(1e-5, 1e-4, 1e-3)) {
      ctx <- sprintf("%s at e=%.0e", nm, e)
      t0 <- proc.time()[["elapsed"]]
      n <- tryCatch(solvers[[nm]](e, 0.05, 0.80, 1), error = function(err) NA_integer_)
      expect_lt(proc.time()[["elapsed"]] - t0, 10, label = ctx)
      if (!is.na(n)) expect_true(is.finite(n), info = ctx)
    }
  }
})
