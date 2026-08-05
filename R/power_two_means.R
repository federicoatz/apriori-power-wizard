## power_two_means.R
## -----------------------------------------------------------------------
## Analysis family 1: comparison of two independent means.
## Thin, testable wrapper around pwr::pwr.t.test() / pwr::pwr.t2n.test().
## -----------------------------------------------------------------------

#' Sample size for a two-independent-means comparison
#'
#' @param d numeric, Cohen's d (standardized mean difference)
#' @param sig_level numeric, alpha
#' @param power numeric, target power (1 - beta)
#' @param alternative "two.sided", "greater", or "less"
#' @param allocation_ratio numeric, n2/n1. 1 = balanced design (default).
#'   When != 1, uses pwr::pwr.t2n.test() by solving n1 iteratively (pwr has
#'   no closed-form unequal-n solver, so we search over n1).
#' @return list with n1, n2, n_total, and the pwr object/metadata used
#' @export
power_two_means_n <- function(d, sig_level = 0.05, power = 0.80,
                               alternative = c("two.sided", "greater", "less"),
                               allocation_ratio = 1) {
  alternative <- match.arg(alternative)
  stopifnot(d > 0, sig_level > 0, sig_level < 1, power > 0, power < 1)

  if (isTRUE(all.equal(allocation_ratio, 1))) {
    fit <- pwr::pwr.t.test(d = d, sig.level = sig_level, power = power,
                            type = "two.sample", alternative = alternative)
    n1 <- round_up_n(fit$n)
    n2 <- n1
  } else {
    # Closed-form starting point, then refine -- not a scan from n1 = 2.
    # With n2 = k*n1 the standard error of the difference is
    # sqrt((1 + 1/k)/n1), so d/SE = z gives
    #   n1 = (z_alpha + z_beta)^2 * (1 + 1/k) / d^2
    # as a normal approximation to the exact noncentral-t solve, close
    # enough that the refinement below is a few steps rather than a walk.
    #
    # The previous scan started at cand = 2, which for any allocation ratio
    # below about 0.6 makes n2 = 1 and pwr rejects the design outright --
    # so "my control arm is twice my treatment arm" (ratio = 0.5, well
    # inside the [0.1, 10] the interface permits) failed with an opaque
    # error from inside pwr. The two-proportions family had the identical
    # defect; both are fixed the same way.
    za <- if (identical(alternative, "two.sided")) {
      stats::qnorm(1 - sig_level / 2)
    } else {
      stats::qnorm(1 - sig_level)
    }
    n1_exact <- (za + stats::qnorm(power))^2 * (1 + 1 / allocation_ratio) / d^2
    assert_solvable_n(n1_exact, "effect size d")

    # Both arms need at least 2 observations; for a ratio below 1 that
    # binds on n2, not on n1.
    min_n1 <- max(2L, round_up_n(2 / allocation_ratio))
    n1 <- max(round_up_n(n1_exact), min_n1)

    achieved_at <- function(cand) {
      n2_cand <- max(round_up_n(cand * allocation_ratio), 2L)
      pwr::pwr.t2n.test(n1 = as.double(cand), n2 = as.double(n2_cand), d = d,
                         sig.level = sig_level,
                         alternative = alternative)$power
    }
    not_reached <- function(cand) {
      pw <- tryCatch(achieved_at(cand), error = function(e) NA_real_)
      !is.finite(pw) || pw < power
    }
    while (not_reached(n1)) {
      n1 <- n1 + 1L
      assert_solvable_n(n1, "effect size d")
    }
    while (n1 > min_n1 && !not_reached(n1 - 1L)) n1 <- n1 - 1L

    n2 <- max(round_up_n(n1 * allocation_ratio), 2L)
  }

  # Report power at the ROUNDED-UP n1/n2 actually recruited, not at the
  # fractional n the continuous solve targeted -- rounding up always
  # increases power slightly above the target, and the reported number
  # should reflect the real (integer) sample size the study will use.
  power_achieved <- power_two_means_at_n(n1 = n1, n2 = n2, d = d,
                                          sig_level = sig_level,
                                          alternative = alternative)

  list(
    n1 = n1, n2 = n2, n_total = n1 + n2,
    d = d, sig_level = sig_level, power_target = power,
    power_achieved = power_achieved,
    alternative = alternative, allocation_ratio = allocation_ratio,
    method = "Two independent means (Student's t-test), pwr::pwr.t.test/pwr.t2n.test"
  )
}

#' Achieved power for a two-means comparison at a given N (curve helper)
#'
#' @export
power_two_means_at_n <- function(n1, n2 = n1, d, sig_level = 0.05,
                                  alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  if (n1 == n2) {
    pwr::pwr.t.test(n = n1, d = d, sig.level = sig_level,
                     type = "two.sample", alternative = alternative)$power
  } else {
    pwr::pwr.t2n.test(n1 = n1, n2 = n2, d = d, sig.level = sig_level,
                       alternative = alternative)$power
  }
}

#' Minimum detectable d for a fixed N (inverse / sensitivity analysis)
#' @export
power_two_means_min_d <- function(n1, n2 = n1, sig_level = 0.05, power = 0.80,
                                   alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  if (n1 == n2) {
    fit <- pwr::pwr.t.test(n = n1, sig.level = sig_level, power = power,
                            type = "two.sample", alternative = alternative)
  } else {
    fit <- pwr::pwr.t2n.test(n1 = n1, n2 = n2, sig.level = sig_level,
                              power = power, alternative = alternative)
  }
  fit$d
}
