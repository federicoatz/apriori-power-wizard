## power_ancova.R
## -----------------------------------------------------------------------
## Analysis family: one-way between-subjects ANCOVA (k groups, one
## continuous covariate). Wraps pwr::pwr.f2.test(), the same function
## R/power_regression.R already uses for multiple regression.
##
## Parameterization (Cohen, 1988, ch. 9; the covariate adjustment is the
## same one popularized for randomized trials by Borm, Fransen, & Lemmens,
## 2007, "A simple sample size formula for analysis of covariance in
## randomized clinical trials", J Clin Epidemiol 60(12), 1234-1238):
##   u  = numerator degrees of freedom = k - 1 (the group effect)
##   v  = denominator degrees of freedom = N_total - k - 1 (one FEWER than
##        plain one-way ANOVA's N_total - k, because estimating the
##        covariate's own regression coefficient costs a degree of
##        freedom even when its true population correlation is 0)
##   f2 = Cohen's f2 for the group effect, adjusted upward for the
##        variance the covariate removes from the error term:
##          f2_adjusted = f^2 / (1 - r_cov^2)
##        where f is the UNADJUSTED group-effect Cohen's f (same
##        small/medium/large convention as plain ANOVA) and r_cov is the
##        expected correlation between the covariate and the outcome.
##        r_cov = 0 recovers the plain-ANOVA effect size exactly (only the
##        one-df cost from df2 remains), matching the well-known result
##        that an uninformative covariate can only ever cost power, never
##        add it.
## Cross-checked in tests directly against pwr::pwr.f2.test (same u/v/f2
## parameterization, so agreement is exact, not approximate) and against
## power_anova_factorial_at_n() in the r_cov = 0 limit.
## -----------------------------------------------------------------------

#' Cohen's f2 for the ANCOVA group effect, adjusted for a covariate
#' @param f_target numeric, unadjusted Cohen's f for the group effect
#' @param r_cov numeric in [0, 1), expected correlation between the
#'   covariate and the outcome
#' @keywords internal
.ancova_f2_adj <- function(f_target, r_cov) {
  stopifnot(f_target > 0, r_cov >= 0, r_cov < 1)
  f_target^2 / (1 - r_cov^2)
}

#' Achieved power for a one-way ANCOVA group effect at a given per-group N
#'
#' @param n_per_group integer, participants per group
#' @param k integer, number of groups (k >= 2)
#' @param f_target numeric, unadjusted Cohen's f for the group effect
#' @param r_cov numeric in [0, 1), expected covariate-outcome correlation
#' @param sig_level numeric, alpha
#' @export
power_ancova_at_n <- function(n_per_group, k, f_target, r_cov = 0, sig_level = 0.05) {
  stopifnot(k >= 2, sig_level > 0, sig_level < 1)
  n_total <- n_per_group * k
  v <- n_total - k - 1
  if (v <= 0) return(NA_real_)
  f2_adj <- .ancova_f2_adj(f_target, r_cov)
  pwr::pwr.f2.test(u = k - 1, v = v, f2 = f2_adj, sig.level = sig_level)$power
}

#' Sample size (per group and total) for a one-way ANCOVA group effect
#'
#' @param k integer, number of groups (k >= 2)
#' @param f_target numeric, unadjusted Cohen's f for the group effect
#' @param r_cov numeric in [0, 1), expected covariate-outcome correlation
#' @param sig_level,power as usual
#' @export
power_ancova_n <- function(k, f_target, r_cov = 0, sig_level = 0.05, power = 0.80) {
  stopifnot(k >= 2, power > 0, power < 1)
  f2_adj <- .ancova_f2_adj(f_target, r_cov)
  fit <- pwr::pwr.f2.test(u = k - 1, f2 = f2_adj, sig.level = sig_level, power = power)
  # fit$v = N_total - k - 1  =>  N_total = v + k + 1
  n_total_exact <- fit$v + k + 1
  n_per_group <- round_up_n(n_total_exact / k)
  n_total <- n_per_group * k

  achieved <- power_ancova_at_n(n_per_group, k, f_target, r_cov, sig_level)
  while (is.na(achieved) || achieved < power) {
    n_per_group <- n_per_group + 1
    n_total <- n_per_group * k
    achieved <- power_ancova_at_n(n_per_group, k, f_target, r_cov, sig_level)
  }

  list(
    n_per_group = n_per_group, n_total = n_total, k = k,
    f = f_target, r_cov = r_cov, f2_adjusted = f2_adj,
    sig_level = sig_level, power_target = power, power_achieved = achieved,
    method = "One-way ANCOVA, pwr::pwr.f2.test with covariate-adjusted f2 (Cohen 1988, ch. 9; Borm, Fransen & Lemmens 2007)"
  )
}

#' Minimum detectable (unadjusted) f for a fixed per-group N
#' @export
power_ancova_min_f <- function(n_per_group, k, r_cov = 0, sig_level = 0.05, power = 0.80) {
  stopifnot(k >= 2, power > 0, power < 1)
  n_total <- n_per_group * k
  v <- n_total - k - 1
  if (v <= 0) return(NA_real_)
  f2_adj <- pwr::pwr.f2.test(u = k - 1, v = v, sig.level = sig_level, power = power)$f2
  sqrt(f2_adj * (1 - r_cov^2))
}
