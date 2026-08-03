## power_curve.R
## -----------------------------------------------------------------------
## Generic power-vs-N curve generator. Dispatches to the "_at_n" function
## of the relevant analysis family so the plotting module never has to
## know about family-specific internals.
## -----------------------------------------------------------------------

#' Generate a data frame of (n, power) pairs for plotting
#'
#' @param family character, one of "two_means", "anova_factorial",
#'   "regression", "logistic", "proportions", "paired_t", "clustered_rct",
#'   "chisq"
#' @param n_solution integer, the N found by the main solver (used to set
#'   a sensible range and to flag the solution point)
#' @param n_range integer vector of length 2, explicit range override
#' @param n_points integer, number of grid points
#' @param ... family-specific arguments passed to the matching
#'   `power_*_at_n()` function (excluding n / n_per_cell / n1 / n_total,
#'   which are supplied by the grid)
#' @return data.frame(n, power, is_solution)
#' @export
generate_power_curve <- function(family = c("two_means", "anova_factorial",
                                             "regression", "logistic",
                                             "proportions", "paired_t",
                                             "clustered_rct", "chisq",
                                             "correlation", "mcnemar", "tost",
                                             "ancova", "survival", "wilcoxon"),
                                  n_solution, n_range = NULL, n_points = 60,
                                  ...) {
  family <- match.arg(family)
  extra <- list(...)

  if (is.null(n_range)) {
    lo <- max(2, round(n_solution * 0.3))
    hi <- round(n_solution * 1.8) + 5
  } else {
    lo <- n_range[1]; hi <- n_range[2]
  }
  grid <- unique(round(seq(lo, hi, length.out = n_points)))
  grid <- grid[grid >= 2]

  power_fun <- switch(family,
    two_means      = function(n) do.call(power_two_means_at_n, c(list(n1 = n), extra)),
    proportions    = function(n) do.call(power_proportions_at_n, c(list(n1 = n), extra)),
    regression     = function(n) do.call(power_regression_at_n, c(list(n_total = n), extra)),
    logistic       = function(n) do.call(power_logistic_at_n, c(list(n_total = n), extra)),
    anova_factorial = function(n) do.call(power_anova_factorial_at_n, c(list(n_per_cell = n), extra)),
    paired_t       = function(n) do.call(power_paired_t_at_n, c(list(n = n), extra)),
    clustered_rct  = function(n) do.call(power_clustered_at_n, c(list(n1 = n), extra)),
    chisq          = function(n) do.call(power_chisq_at_n, c(list(n_total = n), extra)),
    correlation    = function(n) do.call(power_correlation_at_n, c(list(n_total = n), extra)),
    mcnemar        = function(n) do.call(power_mcnemar_at_n, c(list(n = n), extra)),
    tost           = function(n) do.call(power_tost_at_n, c(list(n = n), extra)),
    ancova         = function(n) do.call(power_ancova_at_n, c(list(n_per_group = n), extra)),
    survival       = function(n) do.call(power_survival_at_n, c(list(n_total = n), extra)),
    wilcoxon       = function(n) do.call(power_wilcoxon_at_n, c(list(n1 = n), extra))
  )

  powers <- vapply(grid, function(n) {
    res <- tryCatch(power_fun(n), error = function(e) NA_real_)
    if (length(res) == 0) NA_real_ else res
  }, numeric(1))

  data.frame(n = grid, power = powers, is_solution = grid == n_solution)
}
