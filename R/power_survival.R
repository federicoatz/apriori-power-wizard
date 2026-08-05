## power_survival.R
## -----------------------------------------------------------------------
## Analysis family: two-group log-rank test for a time-to-event outcome.
## Scoped deliberately narrow, matching this app's formula-based (non-
## accrual-curve, non-simulation) style rather than a full clinical-trial
## design tool: the user states the expected OVERALL proportion of
## participants who experience the event by the end of the study
## (`p_event`), not a staggered accrual/censoring schedule.
##
## Framing note: this app targets experimental economics and psychology,
## where a genuine time-to-event outcome is a niche (not absent) tool --
## e.g., time until a participant exits a market/game in an experimental-
## economics study, or attrition/dropout analysis in a longitudinal design.
## Kept here for those cases, not for clinical-trial planning.
##
## Formula (Schoenfeld, 1983, "Sample-size formula for the proportional-
## hazards regression model", Biometrics, 39(2), 499-503): the required
## NUMBER OF EVENTS to detect hazard ratio HR with allocation ratio
## k = n2/n1 is
##   d = (1+k)^2 / k * (z_(alpha/2) + z_beta)^2 / (log(HR))^2
## (two-sided; substitute z_alpha for one-tailed tests). Converting events
## to total N uses N_total = d / p_event, an average-event-probability
## simplification (the same style used by common online calculators for
## this design, and consistent with `power_clustered.R`'s explicit design-
## effect approximation elsewhere in this app).
##
## VALIDATION, done during development against a direct Monte Carlo
## simulation (exponential survival times, fixed-time administrative
## censoring, `survival::survdiff()` -- NOT a runtime dependency of this
## app, see README "Browser-only deployment" for why `survival` itself was
## removed): agreement is excellent (simulated power within ~0.01 of the
## formula) when HR is close to 1 and allocation is close to balanced, but
## the formula becomes noticeably CONSERVATIVE (true power some
## percentage points HIGHER than the formula predicts -- i.e., the
## recommended N ends up somewhat larger than strictly necessary, never
## smaller) for strong effects (HR far from 1) combined with unequal
## allocation. This is a well-documented property of the Schoenfeld
## approximation in the biostatistics literature, not an implementation
## bug; it is disclosed to the user in the app's guided-mode explanation
## rather than silently hidden.
## -----------------------------------------------------------------------

#' Critical z-value for a given alpha and test direction
#' @keywords internal
.survival_z_alpha <- function(sig_level, tails = "two.sided") {
  if (identical(tails, "two.sided")) stats::qnorm(1 - sig_level / 2) else stats::qnorm(1 - sig_level)
}

#' Achieved log-rank power for a given total N
#'
#' @param n_total integer, total sample size across both arms
#' @param hr numeric > 0, != 1, hazard ratio (arm 2 vs. arm 1)
#' @param p_event numeric in (0,1), expected overall proportion of
#'   participants who experience the event by the end of the study
#' @param alloc_ratio numeric > 0, allocation ratio k = n2/n1 (1 = balanced)
#' @param sig_level numeric, alpha
#' @param tails "two.sided", "greater", or "less"
#' @export
power_survival_at_n <- function(n_total, hr, p_event, alloc_ratio = 1,
                                 sig_level = 0.05, tails = "two.sided") {
  stopifnot(hr > 0, hr != 1, p_event > 0, p_event < 1, alloc_ratio > 0)
  k <- alloc_ratio
  events <- n_total * p_event
  za <- .survival_z_alpha(sig_level, tails)
  zb <- sqrt(events * k) / (1 + k) * abs(log(hr)) - za
  stats::pnorm(zb)
}

#' Total sample size for a two-group log-rank test
#'
#' @param hr,p_event,alloc_ratio,sig_level,tails as in [power_survival_at_n()]
#' @param power target power
#' @export
power_survival_n <- function(hr, p_event, alloc_ratio = 1, sig_level = 0.05,
                              power = 0.80, tails = "two.sided") {
  stopifnot(hr > 0, hr != 1, p_event > 0, p_event < 1, alloc_ratio > 0,
            power > 0, power < 1)
  k <- alloc_ratio
  za <- .survival_z_alpha(sig_level, tails)
  zb <- stats::qnorm(power)
  d_events <- (1 + k)^2 / k * (za + zb)^2 / (log(hr))^2
  n_total_exact <- d_events / p_event
  assert_solvable_n(n_total_exact, "hazard ratio")
  n_total <- round_up_n(n_total_exact)

  achieved <- power_survival_at_n(n_total, hr, p_event, k, sig_level, tails)
  while (is.na(achieved) || achieved < power) {
    n_total <- n_total + 1
    achieved <- power_survival_at_n(n_total, hr, p_event, k, sig_level, tails)
    assert_solvable_n(n_total, "hazard ratio")
  }

  n1 <- round_up_n(n_total / (1 + k))
  n2 <- n_total - n1

  list(
    n_total = n_total, n1 = n1, n2 = n2, events_required = ceiling(d_events),
    hr = hr, p_event = p_event, alloc_ratio = k,
    sig_level = sig_level, power_target = power, power_achieved = achieved,
    method = "Two-group log-rank test, Schoenfeld (1983) events formula, base R only (no external dependency)"
  )
}

#' Minimum detectable hazard ratio for a fixed total N
#'
#' Returns the detectable HR on the "protective" (< 1) side; by symmetry
#' in log(HR), 1/value is equally detectable on the harmful (> 1) side.
#' @export
power_survival_min_hr <- function(n_total, p_event, alloc_ratio = 1,
                                   sig_level = 0.05, power = 0.80,
                                   tails = "two.sided") {
  stopifnot(n_total > 1, p_event > 0, p_event < 1, alloc_ratio > 0)
  k <- alloc_ratio
  events <- n_total * p_event
  za <- .survival_z_alpha(sig_level, tails)
  zb <- stats::qnorm(power)
  log_hr_min <- (za + zb) * (1 + k) / sqrt(events * k)
  exp(-log_hr_min)
}
