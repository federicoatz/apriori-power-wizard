## utils.R
## -----------------------------------------------------------------------
## Small, dependency-free helper functions shared across the power-analysis
## modules. Kept separate from statistical logic so it can be unit tested
## in isolation.
## -----------------------------------------------------------------------

#' Round a sample size up to the nearest integer
#'
#' All sample-size outputs in this app are rounded UP (ceiling), never to
#' the nearest integer and never down, because a fractional participant is
#' not realizable and under-recruiting relative to the analytic target
#' inflates the true Type II error rate above the nominal beta.
#'
#' @param x numeric, a (possibly fractional) sample size
#' @return integer, ceiling of x
#' @export
round_up_n <- function(x) {
  if (is.null(x) || length(x) == 0 || any(is.na(x))) return(NA_integer_)
  as.integer(ceiling(x))
}

#' Apply an allocation ratio to a total sample size
#'
#' Given a total N and an allocation ratio r = n2/n1, return per-group sizes
#' such that n1 + n2 = N (approximately, after rounding up) and n2/n1 ~ r.
#'
#' @param n1 numeric, size of group 1 (reference)
#' @param ratio numeric, allocation ratio n2/n1 (1 = balanced design)
#' @return list with n1, n2 (both rounded up)
#' @export
apply_allocation_ratio <- function(n1, ratio = 1) {
  n1 <- round_up_n(n1)
  n2 <- round_up_n(n1 * ratio)
  list(n1 = n1, n2 = n2)
}

#' Safe numeric input coercion with bounds checking
#'
#' @param x value to coerce
#' @param min minimum allowed value (inclusive)
#' @param max maximum allowed value (inclusive)
#' @param default value returned if x is NA/NULL/out of bounds
#' @export
safe_numeric <- function(x, min = -Inf, max = Inf, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x) || x < min || x > max) return(default)
  x
}

#' Map a one/two-sided UI choice to the string used by `pwr` / `WebPower`
#'
#' @param sided character, one of "two.sided", "less", "greater"
#' @export
normalize_alternative <- function(sided = c("two.sided", "greater", "less")) {
  match.arg(sided)
}

#' Format a p-like statistic for display (never show p = 0)
#' @export
format_stat <- function(x, digits = 3) {
  if (is.null(x) || is.na(x)) return("NA")
  formatC(x, digits = digits, format = "f")
}
