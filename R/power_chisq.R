## power_chisq.R
## -----------------------------------------------------------------------
## Analysis family: chi-square test for categorical data -- either a
## single categorical variable against a reference distribution
## (goodness-of-fit) or two categorical variables crossed in a
## contingency table (test of independence). Both share the same power
## formula once degrees of freedom (df) are known, so a single set of
## functions covers both designs; only how `df` is computed from the
## design differs (see modules/mod_chisq.R). Wraps pwr::pwr.chisq.test().
## Effect size is Cohen's (1988, ch. 7) w.
## -----------------------------------------------------------------------

#' Sample size for a chi-square test (goodness-of-fit or independence)
#'
#' @param w numeric, Cohen's w
#' @param df integer, degrees of freedom (k - 1 for a goodness-of-fit test
#'   with k categories; (rows - 1) * (cols - 1) for an r x c contingency
#'   table)
#' @param sig_level,power as in other families
#' @param n_max integer, upper bound on the returned N (guards against a
#'   silently enormous requirement for a tiny w)
#' @export
power_chisq_n <- function(w, df, sig_level = 0.05, power = 0.80, n_max = 1e6) {
  stopifnot(sig_level > 0, sig_level < 1, power > 0, power < 1, df >= 1)
  w <- abs(w)

  fit <- pwr::pwr.chisq.test(w = w, df = df, sig.level = sig_level, power = power)
  n_total <- round_up_n(fit$N)
  if (n_total > n_max) {
    stop("Required N exceeds n_max; check the effect size and df inputs.")
  }

  # Recompute achieved power at the rounded (integer) N rather than trusting
  # the continuous solve -- same pattern used by every other family, since
  # rounding N up always yields power >= power_target, never exactly equal.
  power_achieved <- power_chisq_at_n(n_total, w = w, df = df, sig_level = sig_level)

  list(
    n_total = n_total, df = df, w = w,
    sig_level = sig_level, power_target = power, power_achieved = power_achieved,
    method = "Chi-square test, pwr::pwr.chisq.test"
  )
}

#' @export
power_chisq_at_n <- function(n_total, w, df, sig_level = 0.05) {
  pwr::pwr.chisq.test(w = abs(w), N = n_total, df = df, sig.level = sig_level)$power
}

#' @export
power_chisq_min_w <- function(n_total, df, sig_level = 0.05, power = 0.80) {
  pwr::pwr.chisq.test(N = n_total, df = df, sig.level = sig_level, power = power)$w
}
