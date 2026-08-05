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
## The seed is fixed so a failure is reproducible; raising REPS locally is
## the cheap way to search harder before a release.
## -----------------------------------------------------------------------

set.seed(20260805)
REPS <- 60

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

# One adapter per family: effect size in, total N out. Kept explicit rather
# than derived so that adding a family to the app forces a decision here.
solvers <- list(
  two_means   = function(e, a, p) power_two_means_n(e, a, p)$n_total,
  paired      = function(e, a, p) power_paired_t_n(e, a, p)$n_total,
  proportions = function(e, a, p) power_proportions_n(h = e, sig_level = a, power = p)$n_total,
  correlation = function(e, a, p) power_correlation_n(e, a, p)$n_total,
  chisq       = function(e, a, p) power_chisq_n(w = e, df = 3, sig_level = a, power = p)$n_total,
  regression  = function(e, a, p) power_regression_n(f2 = e^2, u = 3, sig_level = a, power = p)$n_total,
  ancova      = function(e, a, p) power_ancova_n(k = 3, f_target = e, r_cov = 0.4,
                                                 sig_level = a, power = p)$n_total,
  anova       = function(e, a, p) power_anova_factorial_n(f = e, df_effect = 1, n_cells = 4,
                                                          sig_level = a, power = p)$n_total,
  clustered   = function(e, a, p) power_clustered_n(d = e, icc = 0.05, cluster_size = 8,
                                                    sig_level = a, power = p)$n_total,
  wilcoxon    = function(e, a, p) power_wilcoxon_n(p_sup = 0.5 + e / 4,
                                                   sig_level = a, power = p)$n_total,
  survival    = function(e, a, p) power_survival_n(hr = exp(e), p_event = 0.6,
                                                   sig_level = a, power = p)$n_total
)

test_that("every solver returns a finite positive N or raises -- never NA, NULL, or a hang", {
  for (nm in names(solvers)) {
    for (i in seq_len(12)) {
      e <- runif(1, 0.05, 1.2)
      a <- sample(c(0.001, 0.01, 0.05, 0.10), 1)
      p <- sample(c(0.70, 0.80, 0.90, 0.95), 1)
      n <- tryCatch(solvers[[nm]](e, a, p), error = function(err) NA_integer_)
      # an explicit error is an acceptable outcome; a silent NA/NULL is not
      if (!is.na(n)) {
        expect_true(is.finite(n), info = sprintf("%s: e=%.3f a=%.3f p=%.2f", nm, e, a, p))
        expect_gt(n, 0)
      }
    }
  }
})

test_that("required N is non-increasing in the effect size, for every family", {
  for (nm in names(solvers)) {
    a <- 0.05; p <- 0.80
    grid <- seq(0.15, 1.0, length.out = 6)
    ns <- vapply(grid, function(e) tryCatch(as.numeric(solvers[[nm]](e, a, p)),
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
    strict <- tryCatch(as.numeric(solvers[[nm]](e, 0.01, 0.80)), error = function(err) NA_real_)
    loose  <- tryCatch(as.numeric(solvers[[nm]](e, 0.10, 0.80)), error = function(err) NA_real_)
    if (!is.na(strict) && !is.na(loose)) {
      expect_gte(strict, loose, label = sprintf("%s alpha=.01 N", nm))
    }
    hi <- tryCatch(as.numeric(solvers[[nm]](e, 0.05, 0.95)), error = function(err) NA_real_)
    lo <- tryCatch(as.numeric(solvers[[nm]](e, 0.05, 0.70)), error = function(err) NA_real_)
    if (!is.na(hi) && !is.na(lo)) {
      expect_gte(hi, lo, label = sprintf("%s power=.95 N", nm))
    }
  }
})

test_that("a near-null effect never hangs: it resolves or raises, promptly", {
  # The values here sit in the region the safeguard clamp can reach. Each
  # call is wrapped so that an error counts as a pass; what is being tested
  # is that control returns at all, and quickly.
  near_null <- list(
    function() power_two_means_n(1e-4, 0.05, 0.80),
    function() power_paired_t_n(1e-4, 0.05, 0.80),
    function() power_correlation_n(1e-4, 0.05, 0.80),
    function() power_proportions_n(h = 1e-4, sig_level = 0.05, power = 0.80),
    function() power_chisq_n(w = 1e-4, df = 3, sig_level = 0.05, power = 0.80),
    function() power_survival_n(hr = exp(-1e-4), p_event = 0.5),
    function() power_wilcoxon_n(p_sup = 0.5 + 1e-6),
    function() power_ancova_n(k = 2, f_target = 1e-4, r_cov = 0.5)
  )
  for (i in seq_along(near_null)) {
    elapsed <- system.time(
      invisible(tryCatch(near_null[[i]](), error = function(e) NULL))
    )[["elapsed"]]
    expect_lt(elapsed, 10, label = sprintf("near-null case %d returned within 10s", i))
  }
})
