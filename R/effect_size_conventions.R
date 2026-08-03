## effect_size_conventions.R
## -----------------------------------------------------------------------
## Branch 1 of the effect-size step: Cohen's (1988) rule-of-thumb
## conventions. These are provided purely as a fallback for users with no
## domain-specific benchmark and no pilot/prior study, and the UI must
## always display the accompanying warning that these thresholds are
## arbitrary and not calibrated to any particular research area.
## -----------------------------------------------------------------------

#' Cohen's (1988) benchmark effect sizes by test family
#'
#' @param metric character, one of "d" (two means), "f" (ANOVA, any effect),
#'   "f2" (multiple regression), "h" (two proportions), "w" (chi-square
#'   goodness-of-fit / independence), "r" (bivariate correlation), "or"
#'   (logistic regression odds ratio, via the Chinn 2000 approximate
#'   transformation of Cohen's d)
#' @return named numeric vector c(small=, medium=, large=)
#' @export
cohen_benchmarks <- function(metric = c("d", "f", "f2", "h", "w", "r", "or")) {
  metric <- match.arg(metric)
  switch(metric,
    d  = c(small = 0.20, medium = 0.50, large = 0.80),
    f  = c(small = 0.10, medium = 0.25, large = 0.40),
    f2 = c(small = 0.02, medium = 0.15, large = 0.35),
    h  = c(small = 0.20, medium = 0.50, large = 0.80),
    w  = c(small = 0.10, medium = 0.30, large = 0.50),
    r  = c(small = 0.10, medium = 0.30, large = 0.50),
    # Odds ratios corresponding to small/medium/large d, using the
    # logistic-normal approximation logOR = d * pi / sqrt(3)
    # (Chinn S. A simple method for converting an odds ratio to effect
    # size for use in meta-analysis. Stat Med. 2000;19(22):3127-3131.)
    or = exp(c(small = 0.20, medium = 0.50, large = 0.80) * pi / sqrt(3))
  )
}

#' Human-readable warning text for the Cohen's-convention branch
#' @export
cohen_warning_text <- function() {
  paste(
    "Cohen's (1988) small/medium/large thresholds are historical,",
    "field-agnostic rules of thumb. They were never intended to describe",
    "effects typical of experimental economics or behavioral science, and",
    "using them without a domain-specific justification is a common source",
    "of under- or over-powered designs. Whenever possible, prefer a",
    "previous-study estimate (with safeguard correction) or a directly",
    "stated smallest effect size of interest (SESOI)."
  )
}

#' Convert an odds ratio to Cohen's-scale logistic regression input
#'
#' Not itself a power-analysis function; exposed so the UI/tests can trace
#' how the "or" convention values above are derived.
#' @export
d_to_or <- function(d) exp(d * pi / sqrt(3))

#' @export
or_to_d <- function(or) log(or) * sqrt(3) / pi
