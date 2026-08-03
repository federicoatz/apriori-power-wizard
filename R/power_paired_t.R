## power_paired_t.R
## -----------------------------------------------------------------------
## Analysis family: paired-samples / repeated-measures comparison of two
## conditions measured on the same participants (paired-samples t-test).
## Thin, testable wrapper around pwr::pwr.t.test(type = "paired").
##
## Note on the effect size: `d` here is the difference-score standardized
## mean (often written d_z = mean(diff) / sd(diff)), NOT the same
## quantity as the independent-samples d used by power_two_means.R -- it
## already reflects the correlation between the two measurements, and
## pwr::pwr.t.test(type = "paired") uses the identical formula as
## type = "one.sample" applied to the difference scores.
## -----------------------------------------------------------------------

#' Sample size (number of pairs) for a paired-samples comparison
#'
#' @param d numeric, difference-score standardized effect (d_z)
#' @param sig_level numeric, alpha
#' @param power numeric, target power (1 - beta)
#' @param alternative "two.sided", "greater", or "less"
#' @return list with n_pairs, n_total, and the pwr metadata used
#' @export
power_paired_t_n <- function(d, sig_level = 0.05, power = 0.80,
                              alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  stopifnot(d > 0, sig_level > 0, sig_level < 1, power > 0, power < 1)

  fit <- pwr::pwr.t.test(d = d, sig.level = sig_level, power = power,
                          type = "paired", alternative = alternative)
  n_pairs <- round_up_n(fit$n)
  # Report power at the ROUNDED-UP n_pairs actually recruited, not at the
  # fractional n the continuous solve targeted -- rounding up always
  # increases power slightly above the target, and the reported number
  # should reflect the real (integer) sample size the study will use.
  power_achieved <- power_paired_t_at_n(n = n_pairs, d = d, sig_level = sig_level,
                                         alternative = alternative)

  list(
    n_pairs = n_pairs, n1 = n_pairs, n2 = n_pairs, n_total = n_pairs,
    d = d, sig_level = sig_level, power_target = power,
    power_achieved = power_achieved, alternative = alternative,
    method = "Paired-samples t-test (difference-score standardized d), pwr::pwr.t.test(type=\"paired\")"
  )
}

#' Achieved power for a paired comparison at a given number of pairs
#' (curve helper)
#' @export
power_paired_t_at_n <- function(n, d, sig_level = 0.05,
                                 alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  pwr::pwr.t.test(n = n, d = d, sig.level = sig_level,
                   type = "paired", alternative = alternative)$power
}

#' Minimum detectable d for a fixed number of pairs (inverse / sensitivity)
#' @export
power_paired_t_min_d <- function(n, sig_level = 0.05, power = 0.80,
                                  alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  fit <- pwr::pwr.t.test(n = n, sig.level = sig_level, power = power,
                          type = "paired", alternative = alternative)
  fit$d
}
