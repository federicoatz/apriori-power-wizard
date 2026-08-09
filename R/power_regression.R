## power_regression.R
## -----------------------------------------------------------------------
## Analysis family: multiple linear regression (test of a focal predictor,
## or of a set of predictors, over and above covariates already in the
## model). Wraps pwr::pwr.f2.test().
##
## Parameterization follows Cohen (1988), ch. 9:
##   u  = numerator degrees of freedom = number of predictors in the
##        tested set (1 for a single focal predictor)
##   v  = denominator degrees of freedom = n - u - 1  (solved for N)
##   f2 = effect size = R2_ab / (1 - R2_ab)  for an omnibus test, or
##        (R2_full - R2_reduced) / (1 - R2_full) for a focal-predictor test
##        controlling for other covariates already in the model
## -----------------------------------------------------------------------

#' Cohen's f2 from R2 values
#'
#' @param r2_full numeric, R2 of the full model (focal predictor + covariates)
#' @param r2_reduced numeric, R2 of the reduced model (covariates only,
#'   0 if there are no covariates and this is a simple regression /
#'   omnibus test of the full model)
#' @export
r2_to_f2 <- function(r2_full, r2_reduced = 0) {
  stopifnot(r2_full > r2_reduced, r2_full < 1, r2_reduced >= 0)
  (r2_full - r2_reduced) / (1 - r2_full)
}

#' Sample size for a multiple regression predictor / predictor-set test
#'
#' @param f2 numeric, Cohen's f2 for the tested predictor(s)
#' @param n_predictors_tested integer, number of predictors in the focal
#'   test (u); 1 for a single focal predictor's partial effect
#' @param n_covariates integer, number of OTHER predictors already in the
#'   model. Does not change u (the tested set is what u counts), but it DOES
#'   consume residual degrees of freedom: Cohen's denominator df for the
#'   increment test is v = N - u - n_covariates - 1, so every covariate
#'   costs one more participant.
#' @param sig_level,power as usual
#' @export
power_regression_n <- function(f2, n_predictors_tested = 1, n_covariates = 0,
                                sig_level = 0.05, power = 0.80) {
  stopifnot(f2 > 0, n_predictors_tested >= 1, n_covariates >= 0,
            sig_level > 0, sig_level < 1, power > 0, power < 1)

  fit <- pwr::pwr.f2.test(u = n_predictors_tested, f2 = f2,
                           sig.level = sig_level, power = power)
  # pwr returns the required DENOMINATOR df. Converting it back to a sample
  # size has to subtract every predictor in the model, not only the tested
  # set: for the increment test of Cohen (1988, ch. 9),
  #   v = N - u - n_covariates - 1,  so  N = v + u + n_covariates + 1.
  # Omitting n_covariates here (as this did until v1.5.0) understates N by
  # exactly the number of covariates and, worse, overstates the power the
  # returned N delivers -- at f2 = 0.15 with ten covariates it reported
  # .805 for a design that gives .719. The interface has always collected
  # the covariate count; only the solver ignored it.
  n_total <- round_up_n(fit$v + n_predictors_tested + n_covariates + 1)

  # Recompute achieved power at the rounded (integer) N -- same pattern used
  # by every other family. fit$power is the power that was ASKED FOR, which
  # the continuous solve returns unchanged; rounding N up always lands
  # slightly above it (f2 = 0.15 gives 0.8051 at N = 55, not 0.8000), and
  # the number reported to the user should describe the sample they will
  # actually collect.
  power_achieved <- power_regression_at_n(n_total, f2 = f2,
                                           n_predictors_tested = n_predictors_tested,
                                           n_covariates = n_covariates,
                                           sig_level = sig_level)

  list(
    n_total = n_total,
    u = n_predictors_tested, v_achieved = fit$v,
    f2 = f2, sig_level = sig_level, power_target = power,
    power_achieved = power_achieved,
    n_covariates = n_covariates,
    method = "Multiple linear regression, pwr::pwr.f2.test (Cohen 1988, ch. 9)"
  )
}

#' @export
power_regression_at_n <- function(n_total, f2, n_predictors_tested = 1,
                                   n_covariates = 0, sig_level = 0.05) {
  v <- n_total - n_predictors_tested - n_covariates - 1
  if (v <= 0) return(NA_real_)
  pwr::pwr.f2.test(u = n_predictors_tested, v = v, f2 = f2,
                    sig.level = sig_level)$power
}

#' @export
power_regression_min_f2 <- function(n_total, n_predictors_tested = 1,
                                     n_covariates = 0, sig_level = 0.05,
                                     power = 0.80) {
  v <- n_total - n_predictors_tested - n_covariates - 1
  if (v <= 0) return(NA_real_)
  pwr::pwr.f2.test(u = n_predictors_tested, v = v, sig.level = sig_level,
                    power = power)$f2
}
