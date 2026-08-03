## power_tost.R
## -----------------------------------------------------------------------
## Analysis family: TOST (Two One-Sided Tests) equivalence/non-inferiority
## test for two independent means, on Cohen's d scale, equal n per group.
## Not implemented via `pwr` (it has no TOST function). Uses the standard
## exact noncentral-t formulation of TOST power (Phillips, 1990, "Power
## of the two one-sided tests procedure in bioequivalence", Journal of
## Pharmacokinetics and Biopharmaceutics, 18(2), 137-144; Schuirmann,
## 1987, for the original TOST procedure). Re-derived here and
## cross-checked against a direct Monte Carlo simulation of the two
## one-sided t-tests during development (formula and simulated power
## agreed to within Monte Carlo noise across several (n, margin, true
## difference) configurations).
##
## Derivation sketch: with equal n per group, d-hat has SE = sqrt(2/n)
## and df = 2n-2. Testing H0: theta <= -delta_eq vs H0: theta >= delta_eq
## (theta = true standardized difference, delta_eq = equivalence margin)
## gives two t-statistics T1 = (d_hat+delta_eq)/SE, T2 = (d_hat-delta_eq)/SE
## that differ only by the CONSTANT 2*delta_eq/SE, so "reject both" is
## equivalent to T1 falling in a single interval:
##   Power = P(t_crit < T1 < 2*delta_eq/SE - t_crit)
## with T1 ~ noncentral-t(df, ncp = (theta+delta_eq)/SE).
## -----------------------------------------------------------------------

#' Achieved TOST equivalence power at a given per-group N
#'
#' @param n integer, per-group sample size (equal n1 = n2 = n)
#' @param delta_eq numeric > 0, equivalence margin in Cohen's d units
#' @param theta numeric, assumed TRUE standardized difference for planning
#'   purposes (0 = the conservative default: plan assuming no true
#'   difference at all)
#' @param sig_level numeric, significance level of EACH one-sided test
#'   (the conventional TOST parameterization; the procedure is valid at
#'   this nominal level for the overall equivalence claim with no further
#'   correction)
#' @export
power_tost_at_n <- function(n, delta_eq, theta = 0, sig_level = 0.05) {
  stopifnot(delta_eq > 0, n > 1)
  df <- 2 * n - 2
  se <- sqrt(2 / n)
  t_crit <- stats::qt(1 - sig_level, df)
  ncp1 <- (theta + delta_eq) / se
  upper <- 2 * delta_eq / se - t_crit
  # stats::pt() with a noncentrality argument emits a benign "full precision
  # may not have been achieved" warning from its underlying algorithm
  # (Lenth, 1989) for some (df, ncp) combinations; the returned value
  # remains accurate to several digits, well within what a sample-size
  # recommendation needs, so it is suppressed here rather than surfaced.
  suppressWarnings(
    pmax(stats::pt(upper, df, ncp = ncp1) - stats::pt(t_crit, df, ncp = ncp1), 0)
  )
}

#' Per-group sample size for TOST equivalence power
#'
#' @param delta_eq,theta,sig_level as in [power_tost_at_n()]
#' @param power target power
#' @export
power_tost_n <- function(delta_eq, theta = 0, sig_level = 0.05, power = 0.80,
                          n_max = 1e6) {
  stopifnot(sig_level > 0, sig_level < 1, power > 0, power < 1, delta_eq > 0)
  if (abs(theta) >= delta_eq) {
    stop("The assumed true difference must be smaller (in absolute value) ",
         "than the equivalence margin, or equivalence can never be shown.")
  }

  f <- function(n) power_tost_at_n(n, delta_eq = delta_eq, theta = theta, sig_level = sig_level) - power
  lo <- 3
  hi <- 100
  while (f(hi) < 0 && hi < n_max) hi <- hi * 2
  if (f(hi) < 0) stop("Required N exceeds n_max; check the equivalence margin and assumed difference.")

  n_cont <- stats::uniroot(f, interval = c(lo, hi))$root
  n <- round_up_n(n_cont)
  power_achieved <- power_tost_at_n(n, delta_eq = delta_eq, theta = theta, sig_level = sig_level)

  list(
    n = n, n_total = 2 * n, n_per_group = n, delta_eq = delta_eq, theta = theta,
    sig_level = sig_level, power_target = power, power_achieved = power_achieved,
    method = "TOST equivalence test (two independent means), exact noncentral-t"
  )
}

#' Smallest equivalence margin achievable at a fixed per-group N
#' @export
power_tost_min_delta_eq <- function(n, theta = 0, sig_level = 0.05, power = 0.80) {
  f <- function(delta_eq) power_tost_at_n(n, delta_eq = delta_eq, theta = theta, sig_level = sig_level) - power
  upper <- max(abs(theta) * 3, 3)
  lo <- abs(theta) + 1e-4
  # Widen upward until the bracket actually contains a sign change.
  while (f(upper) < 0 && upper < 50) upper <- upper * 2
  stats::uniroot(f, interval = c(lo, upper))$root
}
