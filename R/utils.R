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

#' Extra effect-size values to offer in the "Compare across effect size"
#' scenario grid, expressed relative to the CURRENT effect (never including
#' the current value itself -- scenario_grid() always adds that separately,
#' the same way alpha_set()/power_set() always include the current alpha/
#' power regardless of which checkboxes are ticked).
#'
#' Deliberately framed as "how much weaker/stronger than what you entered"
#' rather than trying to map every family onto Cohen's (somewhat arbitrary)
#' small/medium/large convention -- most families here don't have one that
#' applies (McNemar's two discordant-pair probabilities, TOST's margin, a
#' hazard ratio), and this framing works identically for all of them.
#'
#' @param current numeric, the family's current effect-size value
#' @param kind "magnitude" for an effect expressed as a distance from zero
#'   (d, f, f2, h, w, r, dz, an equivalence margin -- scaled by simple
#'   multiplication), or "ratio" for an effect expressed as a ratio around a
#'   null value of 1 (a hazard ratio -- scaled in LOG space so "50% weaker"
#'   means half as far from 1 on the log scale, not literally HR * 0.5)
#' @param factors numeric vector, multipliers applied to `current` (or to
#'   log(current) for kind = "ratio")
#' @return named numeric vector (possibly empty if `current` is invalid)
#' @export
effect_comparison_values <- function(current, kind = c("magnitude", "ratio"),
                                      factors = c(0.5, 0.8, 1.2, 1.5)) {
  kind <- match.arg(kind)
  if (is.null(current) || length(current) == 0 || is.na(current) || current <= 0) {
    return(stats::setNames(numeric(0), character(0)))
  }
  values <- if (kind == "magnitude") current * factors else exp(log(current) * factors)
  labels <- vapply(factors, function(f) {
    if (f < 1) sprintf("%d%% weaker", round((1 - f) * 100)) else sprintf("%d%% stronger", round((f - 1) * 100))
  }, character(1))
  keep <- !duplicated(round(values, 6)) & abs(values - current) > 1e-9
  stats::setNames(values[keep], labels[keep])
}

#' Format a money amount for display
#'
#' Deliberately currency-agnostic: the app has users in different
#' currencies and no way to know which one is meant, so amounts are shown
#' as plain formatted numbers with thousands separators and no symbol.
#' @param x numeric
#' @export
format_money <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("NA")
  formatC(x, format = "f", digits = if (x >= 100) 0 else 2, big.mark = ",")
}
