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
  # as.integer() returns NA (with only a warning) above .Machine$integer.max,
  # which turns a diverged sample size into a silently corrupt one: the NA
  # then propagates into whatever comparison the caller makes next and
  # surfaces as "missing value where TRUE/FALSE needed", far from its cause.
  # A near-null effect size reaches this in several families, so it is
  # converted here into the same explicit failure assert_solvable_n() gives.
  if (any(ceiling(x) > .Machine$integer.max)) {
    assert_solvable_n(Inf, "effect size")
  }
  as.integer(ceiling(x))
}

#' Derive both group sizes from group 1's size and an allocation ratio
#'
#' Given group 1's (possibly fractional) size and an allocation ratio
#' r = n2/n1, return both per-group sizes rounded up, so n2/n1 ~ r.
#'
#' @param n1 numeric, size of group 1 (reference), possibly fractional
#' @param ratio numeric, allocation ratio n2/n1 (1 = balanced design)
#' @return list with n1, n2 (both rounded up)
#' @export
apply_allocation_ratio <- function(n1, ratio = 1) {
  n1 <- round_up_n(n1)
  n2 <- round_up_n(n1 * ratio)
  list(n1 = n1, n2 = n2)
}

#' How many participants to recruit to retain an analytic sample, given a
#' design's structure
#'
#' Inflating for attrition is arithmetically trivial -- n / (1 - a) -- but
#' applying it to the grand total silently assumes the design has no
#' structure to preserve, and half of this app's families do. A cluster-
#' randomized trial derives its power from the NUMBER OF CLUSTERS; losing
#' participants uniformly shrinks the clusters, so the recruitment target
#' has to inflate the cluster SIZE and keep the count, and the result must
#' still be a whole number of participants per cluster. A factorial design
#' balances on cells and a fixed-groups design on arms: inflating the total
#' and leaving the split implicit produces a number that cannot be
#' allocated evenly, so a researcher following it literally ends up with
#' unequal cells -- exactly the thing the power calculation assumed away.
#'
#' Each rounding is UP and happens at the level of the design unit, so the
#' answer is always at least the naive total, never below it.
#'
#' Dispatch is on which structural fields the result object carries rather
#' than on a family key, so a new family inherits the right behaviour by
#' reporting the same fields the others do, with no list here to update.
#'
#' @param res list, a family's result object (needs `n_total`, plus
#'   whichever structural fields it defines)
#' @param attrition numeric in [0, 1), expected proportion lost before
#'   analysis
#' @return list with `n_recruit` (total to recruit), `unit` (the design
#'   unit the inflation was applied to, or "participant"), `n_units`,
#'   `per_unit_analytic` and `per_unit_recruit`
#' @export
recruitment_target <- function(res, attrition = 0) {
  na_out <- list(n_recruit = NA_integer_, unit = "participant", n_units = NA_integer_,
                 per_unit_analytic = NA_integer_, per_unit_recruit = NA_integer_)
  if (is.null(res) || is.null(res$n_total) || is.na(res$n_total)) return(na_out)
  a <- if (is.null(attrition) || is.na(attrition)) 0 else attrition
  if (a < 0 || a >= 1) return(na_out)

  inflate <- function(x) round_up_n(x / (1 - a))

  built <- function(unit, n_units, per_unit) {
    per_recruit <- if (a <= 0) per_unit else inflate(per_unit)
    list(n_recruit = round_up_n(n_units * per_recruit), unit = unit,
         n_units = round_up_n(n_units), per_unit_analytic = round_up_n(per_unit),
         per_unit_recruit = round_up_n(per_recruit))
  }

  has <- function(...) all(vapply(list(...), function(x) !is.null(x) && !is.na(x) && x > 0, logical(1)))

  # Clusters first: a clustered result also carries n1/n2, and the cluster
  # structure is the binding one.
  if (has(res$n_clusters_total, res$cluster_size)) {
    return(built("cluster", res$n_clusters_total, res$cluster_size))
  }
  if (has(res$n_cells, res$n_per_cell)) {
    return(built("cell", res$n_cells, res$n_per_cell))
  }
  if (has(res$k, res$n_per_group)) {
    return(built("group", res$k, res$n_per_group))
  }
  if (has(res$n_groups, res$n_per_group)) {
    return(built("group", res$n_groups, res$n_per_group))
  }
  if (has(res$n1, res$n2)) {
    n1r <- if (a <= 0) res$n1 else inflate(res$n1)
    n2r <- if (a <= 0) res$n2 else inflate(res$n2)
    return(list(n_recruit = round_up_n(n1r + n2r), unit = "arm", n_units = 2L,
                per_unit_analytic = round_up_n(res$n1),
                per_unit_recruit = round_up_n(n1r)))
  }
  # Unstructured total (single-sample and whole-sample families).
  n_r <- if (a <= 0) res$n_total else inflate(res$n_total)
  list(n_recruit = round_up_n(n_r), unit = "participant",
       n_units = round_up_n(n_r), per_unit_analytic = 1L, per_unit_recruit = 1L)
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
#' @param x numeric amount
#' @param symbol character, currency symbol to prefix (e.g. "\u20ac", "\u00a3",
#'   "$"); "" gives a bare number. Prefixed rather than suffixed for all
#'   supported currencies, which is the convention in English-language
#'   academic writing even for the euro.
#' @export
format_money <- function(x, symbol = "") {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("NA")
  paste0(symbol, formatC(x, format = "f",
                          digits = if (x >= 100) 0 else 2, big.mark = ","))
}

#' Currency choices offered by the budget panel, as symbol lookup
#'
#' Kept here rather than inline in the UI so the selectInput and the
#' formatting helper can never drift apart.
#' @export
currency_symbols <- function() c(EUR = "\u20ac", GBP = "\u00a3", USD = "$")

#' Refuse to solve for a sample size that has diverged
#'
#' Several families refine a closed-form starting value upward one
#' participant at a time until the achieved power meets the target. That
#' loop is fast and safe whenever the starting value is close, which it is
#' for any realistic effect size -- but it has no natural upper bound, so an
#' effect size arbitrarily close to the null sends it either to an
#' astronomically large starting point or into an unbounded walk, and the
#' app hangs rather than returning anything.
#'
#' Near-null effects are reachable in normal use: the safeguard-power branch
#' shrinks a published estimate toward the null by z*SE, so a sufficiently
#' imprecise prior study (or a high confidence level) legitimately produces
#' one. See safeguard_shrink() in R/safeguard_power.R.
#'
#' The cap is deliberately far above any feasible study rather than tuned to
#' this app's audience: the point is to convert a hang into a clean error
#' that the calling module can catch and explain, not to second-guess what
#' sample size a user is allowed to ask for.
#'
#' @param n numeric, a candidate sample size
#' @param what character, the input to name in the error message
#' @param max_n numeric, the largest sample size treated as solvable
#' @return invisible(TRUE); raises an error otherwise
#' @export
assert_solvable_n <- function(n, what = "effect size", max_n = 1e7) {
  if (!is.finite(n) || n > max_n) {
    stop(sprintf(
      paste0("Required sample size exceeds %s, which means the %s given is ",
             "indistinguishable from no effect at all. Increase it, or ",
             "(if it came from the safeguard branch) lower the confidence ",
             "level or use a stated smallest effect of interest instead."),
      format(max_n, big.mark = ",", scientific = FALSE), what),
      call. = FALSE)
  }
  invisible(TRUE)
}
