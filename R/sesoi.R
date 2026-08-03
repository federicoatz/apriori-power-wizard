## sesoi.R
## -----------------------------------------------------------------------
## Branch 3 of the effect-size step: Smallest Effect Size Of Interest.
## The user states directly the smallest difference/association they
## consider theoretically or practically meaningful, either in raw units
## (with an expected SD, converted to a standardized effect size) or
## already in standardized units.
## -----------------------------------------------------------------------

#' Convert a raw mean difference + SD into Cohen's d
#'
#' @param raw_diff numeric, minimal difference of practical/theoretical
#'   interest, in the outcome's raw units (e.g., dollars, points, seconds)
#' @param sd_pooled numeric, expected (pooled) standard deviation of the
#'   outcome in raw units, e.g., from pilot data or comparable prior work
#' @return numeric, standardized effect size d = raw_diff / sd_pooled
#' @export
sesoi_raw_to_d <- function(raw_diff, sd_pooled) {
  stopifnot(sd_pooled > 0)
  raw_diff / sd_pooled
}

#' Convert a raw regression slope SESOI into Cohen's f2 (multiple regression)
#'
#' For a focal predictor's minimal slope of interest b, with SD of the
#' predictor sd_x, SD of the outcome sd_y, and the proportion of predictor
#' variance already explained by other covariates (r2_x_others, 0 if the
#' predictor is orthogonal to the rest of the model), the semi-partial
#' correlation is:
#'   pr = b * sd_x * sqrt(1 - r2_x_others) / sd_y
#' and Cohen's f2 for that single predictor, controlling for the full
#' model R2, is f2 = pr^2 / (1 - R2_full).
#'
#' @param b numeric, minimal slope of interest (raw units)
#' @param sd_x numeric, SD of the focal predictor
#' @param sd_y numeric, SD of the outcome
#' @param r2_full numeric in [0,1), total R2 of the full model (all
#'   predictors); defaults to using pr^2 directly (equivalent to R2_full=0)
#' @param r2_x_others numeric in [0,1), R2 of the focal predictor regressed
#'   on all other predictors (0 = orthogonal / simple regression)
#' @export
sesoi_slope_to_f2 <- function(b, sd_x, sd_y, r2_full = 0, r2_x_others = 0) {
  stopifnot(sd_x > 0, sd_y > 0, r2_full >= 0, r2_full < 1,
            r2_x_others >= 0, r2_x_others < 1)
  pr <- b * sd_x * sqrt(1 - r2_x_others) / sd_y
  pr^2 / (1 - r2_full)
}

#' Convert a raw proportion difference into Cohen's h
#' @param p1,p2 numeric in (0,1), the two proportions defining the SESOI
#' @export
sesoi_proportions_to_h <- function(p1, p2) {
  stopifnot(p1 > 0, p1 < 1, p2 > 0, p2 < 1)
  2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
}

#' Convert partial eta-squared to Cohen's f (ANOVA effect size)
#'
#' f = sqrt(eta2 / (1 - eta2))  (Cohen, 1988, ch. 8)
#' @param eta2 numeric in [0,1), partial eta-squared for the focal effect
#' @export
eta2_to_f <- function(eta2) {
  stopifnot(eta2 >= 0, eta2 < 1)
  sqrt(eta2 / (1 - eta2))
}

#' @export
f_to_eta2 <- function(f) f^2 / (1 + f^2)

#' Raw mean difference SESOI for a two-level ANOVA factor, expressed as
#' Cohen's f (exact identity f = d/2 for a two-level factor; Cohen 1988)
#' @export
sesoi_raw_to_f_twolevel <- function(raw_diff, sd_pooled) {
  sesoi_raw_to_d(raw_diff, sd_pooled) / 2
}

#' Package a SESOI branch choice into a standard effect-size record
#'
#' @param mode character, "standardized" (user supplies d/f/f2/h/OR
#'   directly) or "raw" (user supplies raw-unit inputs, converted here)
#' @param value numeric, effect size (used when mode = "standardized")
#' @param ... passed to the relevant `sesoi_*_to_*()` converter when
#'   mode = "raw"
#' @export
sesoi_effect_size <- function(mode = c("standardized", "raw"),
                               metric = c("d", "f2", "h"),
                               value = NULL, ...) {
  mode <- match.arg(mode)
  metric <- match.arg(metric)
  if (mode == "standardized") {
    stopifnot(!is.null(value))
    return(value)
  }
  switch(metric,
    d  = sesoi_raw_to_d(...),
    f2 = sesoi_slope_to_f2(...),
    h  = sesoi_proportions_to_h(...)
  )
}
