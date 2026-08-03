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
    # Search the smallest n1 (1..100000) such that pwr.t2n.test achieves
    # the target power with n2 = round_up(n1 * allocation_ratio).
    n1 <- NA_integer_
    for (cand in 2:100000) {
      n2_cand <- round_up_n(cand * allocation_ratio)
      p <- pwr::pwr.t2n.test(n1 = cand, n2 = n2_cand, d = d,
                              sig.level = sig_level,
                              alternative = alternative)$power
      if (p >= power) { n1 <- cand; break }
    }
    if (is.na(n1)) stop("No feasible N found below 100000 per group; check inputs.")
    n2 <- round_up_n(n1 * allocation_ratio)
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
