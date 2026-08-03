## sensitivity_analysis.R
## -----------------------------------------------------------------------
## Inverse / "sensitivity" power analysis: given a budget-constrained
## maximum N, what is the smallest effect the design can reliably detect
## at the chosen alpha and power? Thin dispatcher over the family-specific
## `power_*_min_*()` functions.
## -----------------------------------------------------------------------

#' Minimum detectable effect for a fixed maximum N
#'
#' @param family character, analysis family (see [generate_power_curve()])
#' @param n_max integer, maximum feasible total (or per-cell, for ANOVA) N
#' @param ... family-specific arguments (sig_level, power, alternative,
#'   allocation, factor levels, focal, sd, predictor spec, etc.)
#' @return list(effect_metric, effect_value)
#' @export
sensitivity_min_effect <- function(family = c("two_means", "anova_factorial",
                                               "regression", "logistic",
                                               "proportions", "paired_t",
                                               "clustered_rct", "chisq",
                                               "correlation", "mcnemar", "tost",
                                               "ancova", "survival"),
                                    n_max, ...) {
  family <- match.arg(family)
  extra <- list(...)

  switch(family,
    two_means = list(
      metric = "d",
      value = do.call(power_two_means_min_d, c(list(n1 = n_max), extra))
    ),
    proportions = list(
      metric = "h",
      value = do.call(power_proportions_min_h, c(list(n1 = n_max), extra))
    ),
    regression = list(
      metric = "f2",
      value = do.call(power_regression_min_f2, c(list(n_total = n_max), extra))
    ),
    logistic = list(
      metric = "p1 (minimum detectable, holding p0 fixed)",
      value = do.call(power_logistic_min_p1, c(list(n_total = n_max), extra))
    ),
    anova_factorial = list(
      metric = "f",
      value = do.call(power_anova_factorial_min_f, c(list(n_per_cell = n_max), extra))
    ),
    paired_t = list(
      metric = "dz",
      value = do.call(power_paired_t_min_d, c(list(n = n_max), extra))
    ),
    clustered_rct = list(
      metric = "d (individual-level, pre-design-effect)",
      value = do.call(power_clustered_min_d, c(list(n1 = n_max), extra))
    ),
    chisq = list(
      metric = "w",
      value = do.call(power_chisq_min_w, c(list(n_total = n_max), extra))
    ),
    correlation = list(
      metric = "r",
      value = do.call(power_correlation_min_r, c(list(n_total = n_max), extra))
    ),
    mcnemar = list(
      metric = "delta (p10 - p01)",
      value = do.call(power_mcnemar_min_delta, c(list(n = n_max), extra))
    ),
    tost = list(
      metric = "delta_eq (equivalence margin, Cohen's d)",
      value = do.call(power_tost_min_delta_eq, c(list(n = n_max), extra))
    ),
    ancova = list(
      metric = "f (unadjusted, before the covariate)",
      value = do.call(power_ancova_min_f, c(list(n_per_group = n_max), extra))
    ),
    survival = list(
      metric = "HR (protective side; 1/value is equally detectable on the harmful side)",
      value = do.call(power_survival_min_hr, c(list(n_total = n_max), extra))
    )
  )
}
