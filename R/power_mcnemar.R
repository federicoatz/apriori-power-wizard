## power_mcnemar.R
## -----------------------------------------------------------------------
## Analysis family: McNemar's test for paired binary outcomes (e.g., the
## same participants classified yes/no under two conditions). Only the
## DISCORDANT pairs are informative -- pairs where the outcome is the same
## under both conditions carry no information about a change. The
## effect is therefore parameterized directly by the two discordant-pair
## probabilities:
##   p10 = P(positive under condition 1, negative under condition 2)
##   p01 = P(negative under condition 1, positive under condition 2)
##
## Not implemented via `pwr` (it has no McNemar function). The formula
## below is the standard large-sample approximation (Connor, 1987,
## "Sample size for testing differences in proportions for the
## paired-sample design", Biometrics 43(1), 207-211; reproduced in
## Machin, Campbell, Tan & Tan, "Sample Size Tables for Clinical
## Studies", and implemented equivalently by PASS and G*Power's McNemar
## procedures). Re-derived from first principles and cross-checked
## against a direct Monte Carlo simulation of the McNemar chi-square
## statistic during development (formula and simulated power agreed to
## within Monte Carlo noise across several (n, p10, p01) configurations).
##
## Derivation sketch: let X, Y count the "10" and "01" discordant pairs
## out of n total pairs; the (uncorrected) McNemar statistic is
## Z = (X - Y) / sqrt(X + Y), asymptotically standard normal under H0.
## With p_d = p10 + p01 and delta = |p10 - p01|,
##   E[X - Y] = n * delta,  Var[X - Y] = n * (p_d - delta^2),
## and approximating the denominator's expectation sqrt(X+Y) ~= sqrt(n*p_d)
## gives Z ~ N(delta*sqrt(n/p_d), 1 - delta^2/p_d) under H1. Solving
## P(|Z| > z_(1-alpha/2)) = power for n yields the closed form below.
## -----------------------------------------------------------------------

#' Sample size (number of pairs) for McNemar's test
#'
#' @param p10 numeric in (0,1), P(positive under condition 1, negative
#'   under condition 2)
#' @param p01 numeric in (0,1), P(negative under condition 1, positive
#'   under condition 2). p10 + p01 (the proportion of discordant pairs)
#'   must be less than 1.
#' @param sig_level,power as in other families (two-sided only -- McNemar's
#'   test is not conventionally offered as one-sided)
#' @export
power_mcnemar_n <- function(p10, p01, sig_level = 0.05, power = 0.80, n_max = 1e6) {
  stopifnot(sig_level > 0, sig_level < 1, power > 0, power < 1)
  stopifnot(p10 > 0, p10 < 1, p01 > 0, p01 < 1, p10 + p01 < 1)

  p_d <- p10 + p01
  delta <- abs(p10 - p01)
  if (delta <= 0) stop("p10 and p01 must differ (delta = 0 requires an infinite N).")

  za <- stats::qnorm(1 - sig_level / 2)
  zb <- stats::qnorm(power)
  n <- (za * sqrt(p_d) + zb * sqrt(p_d - delta^2))^2 / delta^2
  n_total <- round_up_n(n)
  if (n_total > n_max) stop("Required N exceeds n_max; check the discordant proportions.")

  power_achieved <- power_mcnemar_at_n(n_total, p10 = p10, p01 = p01, sig_level = sig_level)

  list(
    n = n_total, n_total = n_total, p10 = p10, p01 = p01, delta = delta, p_discordant = p_d,
    sig_level = sig_level, power_target = power, power_achieved = power_achieved,
    method = "McNemar's test, Connor (1987) large-sample approximation"
  )
}

#' @export
power_mcnemar_at_n <- function(n, p10, p01, sig_level = 0.05) {
  p_d <- p10 + p01
  delta <- abs(p10 - p01)
  za <- stats::qnorm(1 - sig_level / 2)
  mu1 <- delta * sqrt(n / p_d)
  sigma1 <- sqrt(max(1 - delta^2 / p_d, 1e-8))
  1 - stats::pnorm((za - mu1) / sigma1)
}

#' Minimum detectable |p10 - p01| for a fixed N and p_discordant
#'
#' @param n integer, number of pairs
#' @param p_discordant numeric in (0,1), assumed p10 + p01
#' @param sig_level,power as in other families
#' @export
power_mcnemar_min_delta <- function(n, p_discordant = 0.3, sig_level = 0.05, power = 0.80) {
  stopifnot(p_discordant > 0, p_discordant < 1)
  f <- function(delta) {
    za <- stats::qnorm(1 - sig_level / 2)
    zb <- stats::qnorm(power)
    n_needed <- (za * sqrt(p_discordant) + zb * sqrt(p_discordant - delta^2))^2 / delta^2
    n_needed - n
  }
  upper <- sqrt(p_discordant) * 0.999
  stats::uniroot(f, interval = c(1e-4, upper))$root
}
