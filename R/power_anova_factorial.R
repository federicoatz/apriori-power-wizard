## power_anova_factorial.R
## -----------------------------------------------------------------------
## Analysis family: between-subjects factorial ANOVA (up to 2 factors in
## this version). Closed-form (non-simulation) noncentral-F power, base R
## only -- no external package dependency.
##
## THIS USED TO WRAP Superpower::ANOVA_design()/ANOVA_exact(). That
## required building a synthetic cell-mean pattern (to isolate whichever
## focal effect -- main effect of A, of B, or the A:B interaction -- was
## selected) purely so Superpower's simulation-oriented API had something
## to compute a noncentral F test from. But the noncentrality parameter
## for ANY fixed-effects ANOVA factor has a direct, well-known closed
## form in terms of Cohen's f (Cohen, 1988, ch. 8-9):
##   lambda = N_total * f^2
## where N_total is the total sample size across every cell in the full
## design (not just the margin of the tested factor). Degrees of freedom
## follow directly from the design:
##   df1 (main effect of a factor with k levels)      = k - 1
##   df1 (interaction of two factors, k1 and k2 levels) = (k1-1) * (k2-1)
##   df2 (error df, any effect, any design)            = N_total - n_cells
## Power is then the standard noncentral-F tail probability:
##   power = P(F > qf(1 - alpha, df1, df2) | df1, df2, lambda)
## which is exactly stats::pf()/stats::qf() -- both base R. This is the
## same relationship pwr::pwr.anova.test() uses internally for one-way
## designs (lambda = k*n*f^2); this file is the direct two-factor
## generalization of it. Validated against the previous Superpower-based
## implementation across 12 designs (1- and 2-factor, main effects and
## interactions, several alpha levels) with 0 numerical difference before
## the Superpower dependency was removed.
##
## Dropping Superpower also removes ~98MB of transitive wasm downloads
## (afex, lme4, car, emmeans, Matrix, doBy, survival, forecast, and more)
## from the browser-only (shinylive) build -- see README.md, "Browser-only
## deployment", for the measured before/after.
## -----------------------------------------------------------------------

#' Numerator degrees of freedom for the focal effect
#' @keywords internal
.anova_df1 <- function(levels_a, levels_b, focal) {
  if (is.null(levels_b)) return(levels_a - 1)
  switch(focal,
    A = levels_a - 1,
    B = levels_b - 1,
    `A:B` = (levels_a - 1) * (levels_b - 1)
  )
}

#' Achieved power for a factorial ANOVA focal contrast at a given per-cell N
#'
#' @param n_per_cell integer, participants per cell
#' @param levels_a,levels_b integer, number of levels per factor
#'   (levels_b = NULL for a single between-subjects factor)
#' @param focal "A", "B", or "A:B"
#' @param f_target numeric, Cohen's f for the focal contrast
#' @param sd unused; retained for signature compatibility with the other
#'   families' `power_*_at_n()` functions (Cohen's f is already expressed
#'   relative to the within-cell SD, so no separate sd argument is needed
#'   in the noncentral-F formula itself)
#' @param sig_level numeric, alpha
#' @export
power_anova_factorial_at_n <- function(n_per_cell, levels_a, levels_b = NULL,
                                        focal = c("A", "B", "A:B"),
                                        f_target, sd = 1, sig_level = 0.05) {
  focal <- match.arg(focal)
  stopifnot(levels_a >= 2, f_target > 0, sig_level > 0, sig_level < 1)
  if (!is.null(levels_b)) stopifnot(levels_b >= 2)

  n_cells <- levels_a * (if (is.null(levels_b)) 1 else levels_b)
  n_total <- n_per_cell * n_cells
  df1 <- .anova_df1(levels_a, levels_b, focal)
  df2 <- n_total - n_cells
  if (df2 <= 0) return(NA_real_)
  ncp <- n_total * f_target^2

  stats::pf(stats::qf(1 - sig_level, df1, df2), df1, df2, ncp = ncp, lower.tail = FALSE)
}

#' Sample size (per cell and total) for a factorial ANOVA focal contrast
#'
#' @param levels_a,levels_b integer, number of levels per factor
#'   (levels_b = NULL for a single between-subjects factor)
#' @param focal "A", "B", or "A:B"
#' @param f_target numeric, Cohen's f for the focal contrast
#' @param sd unused; see [power_anova_factorial_at_n()]
#' @param sig_level,power as usual
#' @param n_max integer, upper bound for the per-cell search
#' @export
power_anova_factorial_n <- function(levels_a, levels_b = NULL,
                                     focal = c("A", "B", "A:B"),
                                     f_target, sd = 1,
                                     sig_level = 0.05, power = 0.80,
                                     n_max = 100000) {
  focal <- match.arg(focal)
  stopifnot(power > 0, power < 1)
  n_cells <- levels_a * (if (is.null(levels_b)) 1 else levels_b)

  achieved <- function(n) {
    power_anova_factorial_at_n(n, levels_a, levels_b, focal, f_target, sd, sig_level)
  }
  if (is.na(achieved(n_max)) || achieved(n_max) < power) {
    stop(sprintf("Target power not reached with up to %d participants per cell.", n_max))
  }

  # Solve continuously (power is monotonic increasing in n_per_cell), then
  # round UP to a whole number of participants and confirm the achieved
  # power at that integer N -- round_up_n() never rounds a solution down,
  # so this can only ever meet or exceed the target, never fall short.
  n_exact <- stats::uniroot(function(n) achieved(n) - power, lower = 2, upper = n_max)$root
  n_per_cell <- round_up_n(n_exact)
  while (is.na(achieved(n_per_cell)) || achieved(n_per_cell) < power) n_per_cell <- n_per_cell + 1

  list(
    n_per_cell = n_per_cell,
    n_total = n_per_cell * n_cells,
    n_cells = n_cells,
    levels_a = levels_a, levels_b = levels_b, focal = focal,
    f = f_target, sd = sd, sig_level = sig_level,
    power_target = power, power_achieved = achieved(n_per_cell),
    method = "Factorial between-subjects ANOVA, closed-form noncentral F power (Cohen, 1988); base R only (stats::pf/stats::qf)"
  )
}

#' Minimum detectable f for a fixed per-cell N (inverse of [power_anova_factorial_n()])
#' @export
power_anova_factorial_min_f <- function(n_per_cell, levels_a, levels_b = NULL,
                                         focal = c("A", "B", "A:B"),
                                         sd = 1, sig_level = 0.05, power = 0.80) {
  focal <- match.arg(focal)
  f_gap <- function(f) power_anova_factorial_at_n(n_per_cell, levels_a, levels_b, focal, f, sd, sig_level) - power
  stats::uniroot(f_gap, lower = 1e-4, upper = 5)$root
}

#' Text of the interaction-vs-main-effect sample-size warning
#'
#' Shown prominently whenever focal == "A:B". A textbook illustration:
#' detecting a "disordinal" interaction where a main effect halves between
#' conditions typically requires roughly FOUR TIMES the per-cell N needed
#' to detect that same main effect on its own (Simonsohn, 2014; a rule of
#' thumb repeatedly rediscovered in the power literature).
#' @export
interaction_power_warning <- function() {
  paste(
    "Interactions require much larger samples than the main effects they",
    "are built from. As a rule of thumb, detecting an interaction in",
    "which an effect is halved between conditions requires roughly FOUR",
    "TIMES the per-cell sample size needed to detect that main effect by",
    "itself (see Simonsohn, 2014, 'No-way interactions'). Underpowering",
    "interaction tests is one of the most common mistakes in factorial",
    "design planning -- double-check that the effect size you entered is",
    "specific to the interaction contrast, not to a main effect."
  )
}
