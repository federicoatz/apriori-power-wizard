## power_logistic.R
## -----------------------------------------------------------------------
## Analysis family: logistic regression with a single binary or continuous
## focal predictor.
##
## This is a dependency-free reimplementation of the Demidenko (2007)
## asymptotic Wald-test power formula for a single-predictor logistic
## regression, following the same algorithm as WebPower::wp.logistic()
## (Zhang & Yuan, 2018, "Practical Statistical Power Analysis Using
## WebPower and R"). We deliberately do NOT depend on the WebPower package:
## its transitive dependency chain (lme4, Matrix, lavaan, PearsonDS, ...)
## is large, mostly irrelevant to this one function, and was found to be
## the source of a WebAssembly ABI crash in the browser-only (shinylive)
## build of this app. The formula below only needs base R (`integrate`,
## `uniroot`, `pnorm`, `qnorm`), which are always available.
##
## Algorithm summary (Demidenko 2007):
##   Given the outcome probabilities p0 = P(Y=1 | X=0) and p1 = P(Y=1 |
##   X=1), the implied logistic-regression slope is
##     beta1 = log( odds(p1) / odds(p0) ) = log(OR)
##   and intercept beta0 = log(p0 / (1-p0)). Under the assumed distribution
##   of the focal predictor X, the asymptotic variance of the MLE for
##   beta1 is derived from the Fisher information of the model, and a
##   normal-approximation Wald test then gives a closed-form power formula
##   as a function of n, alpha, and beta1.
##
##   We implement the two predictor distributions this app actually
##   offers: "binary" (X ~ Bernoulli(prop_predictor), family = "Bernoulli"
##   in WebPower's terminology) and "normal" (X ~ Normal(0, 1), standardized
##   continuous predictor).
##
## Validated against the reference values published in WebPower's own
## documentation (family = "normal", parameter = c(0, 1)):
##   p0 = 0.15, p1 = 0.1, n = 200, alpha = 0.05 (two-sided)
##     -> beta1 = -0.4626235, power = 0.6299315
##   p0 = 0.15, p1 = 0.1, power = 0.8, alpha = 0.05 (two-sided)
##     -> n = 298.9207
## See tests/testthat/test-power_logistic.R for the automated check.
## -----------------------------------------------------------------------

#' Odds ratio implied by two probabilities
#' @export
probs_to_or <- function(p0, p1) (p1 / (1 - p1)) / (p0 / (1 - p0))

#' Asymptotic variance of the MLE for the focal-predictor slope (beta1),
#' Fisher-information terms following Demidenko (2007) / WebPower's
#' internal derivation. Internal helper, not exported.
.logistic_beta_v1 <- function(p0, p1, predictor_dist, prop_predictor) {
  beta0 <- log(p0 / (1 - p0))
  odds <- (p1 / (1 - p1)) / (p0 / (1 - p0))
  beta1 <- log(odds)

  if (predictor_dist == "binary") {
    B <- prop_predictor
    d <- B * p1 * (1 - p1) + (1 - B) * p0 * (1 - p0)
    e <- B * p1 * (1 - p1)
    f <- B * p1 * (1 - p1)
    v1 <- d / (d * f - e^2)
  } else {
    # Standardized normal predictor: X ~ Normal(0, 1)
    mu <- 0; sigma <- 1
    px <- function(x) 1 / (1 + exp(-beta0 - beta1 * x))
    d <- stats::integrate(function(x) (1 - px(x)) * px(x) * stats::dnorm(x, mu, sigma),
                           -Inf, Inf, subdivisions = 100L)$value
    e <- stats::integrate(function(x) x * (1 - px(x)) * px(x) * stats::dnorm(x, mu, sigma),
                           -Inf, Inf, subdivisions = 100L)$value
    f <- stats::integrate(function(x) x^2 * (1 - px(x)) * px(x) * stats::dnorm(x, mu, sigma),
                           -Inf, Inf, subdivisions = 100L)$value
    v1 <- d / (d * f - e^2)
  }

  list(beta0 = beta0, beta1 = beta1, v1 = v1)
}

#' Power for a given n, beta1, v1, alpha, alternative (Demidenko 2007
#' normal-approximation Wald test). Internal helper, not exported.
.logistic_power_formula <- function(n, beta1, v1, sig_level, alternative) {
  se <- sqrt(v1 / n)
  z_beta1 <- beta1 / se
  if (alternative == "two.sided") {
    a <- sig_level / 2
    stats::pnorm(-stats::qnorm(1 - a) - z_beta1) + stats::pnorm(-stats::qnorm(1 - a) + z_beta1)
  } else if (alternative == "less") {
    stats::pnorm(-stats::qnorm(1 - sig_level) - z_beta1)
  } else { # "greater"
    stats::pnorm(-stats::qnorm(1 - sig_level) + z_beta1)
  }
}

#' Sample size for a logistic regression focal predictor
#'
#' @param p0 numeric in (0,1), outcome probability at the reference level
#'   of the focal predictor (or at its mean, for a continuous predictor)
#' @param p1 numeric in (0,1), outcome probability implied by the target
#'   effect (one-unit / one-category shift in the focal predictor)
#' @param predictor_dist character, "binary" (Bernoulli-distributed focal
#'   predictor, e.g., a treatment indicator) or "normal" (continuous,
#'   standardized focal predictor)
#' @param prop_predictor numeric in (0,1), P(X=1) when predictor_dist =
#'   "binary" (e.g., 0.5 for a balanced treatment indicator); ignored for
#'   "normal"
#' @param sig_level,power,alternative as usual
#' @export
power_logistic_n <- function(p0, p1,
                              predictor_dist = c("binary", "normal"),
                              prop_predictor = 0.5,
                              sig_level = 0.05, power = 0.80,
                              alternative = c("two.sided", "greater", "less")) {
  predictor_dist <- match.arg(predictor_dist)
  alternative <- match.arg(alternative)
  stopifnot(p0 > 0, p0 < 1, p1 > 0, p1 < 1, p0 != p1)

  bv <- .logistic_beta_v1(p0, p1, predictor_dist, prop_predictor)

  n_exact <- stats::uniroot(
    function(n) .logistic_power_formula(n, bv$beta1, bv$v1, sig_level, alternative) - power,
    c(2 + 1e-10, 1e7)
  )$root

  n_total <- round_up_n(n_exact)
  power_achieved <- .logistic_power_formula(n_total, bv$beta1, bv$v1, sig_level, alternative)

  list(
    n_total = n_total,
    p0 = p0, p1 = p1, odds_ratio = probs_to_or(p0, p1),
    predictor_dist = predictor_dist, prop_predictor = prop_predictor,
    sig_level = sig_level, power_target = power,
    power_achieved = power_achieved,
    alternative = alternative,
    method = "Logistic regression, single focal predictor (Demidenko, 2007)"
  )
}

#' @export
power_logistic_at_n <- function(n_total, p0, p1,
                                 predictor_dist = c("binary", "normal"),
                                 prop_predictor = 0.5,
                                 sig_level = 0.05,
                                 alternative = c("two.sided", "greater", "less")) {
  predictor_dist <- match.arg(predictor_dist)
  alternative <- match.arg(alternative)
  bv <- .logistic_beta_v1(p0, p1, predictor_dist, prop_predictor)
  .logistic_power_formula(n_total, bv$beta1, bv$v1, sig_level, alternative)
}

#' Minimum detectable OR (as p1, holding p0 fixed) for a fixed N
#'
#' Bisection search on p1 (for p1 > p0; the symmetric case p1 < p0 is
#' obtained by swapping roles) until the achieved power matches the target.
#' @export
power_logistic_min_p1 <- function(n_total, p0,
                                   predictor_dist = c("binary", "normal"),
                                   prop_predictor = 0.5,
                                   sig_level = 0.05, power = 0.80,
                                   alternative = c("two.sided", "greater", "less"),
                                   direction = c("greater", "less")) {
  predictor_dist <- match.arg(predictor_dist)
  alternative <- match.arg(alternative)
  direction <- match.arg(direction)

  achieved <- function(p1) {
    power_logistic_at_n(n_total, p0, p1, predictor_dist, prop_predictor,
                         sig_level, alternative)
  }

  if (direction == "greater") {
    lo <- p0 + 1e-4; hi <- 0.999
  } else {
    lo <- 0.001; hi <- p0 - 1e-4
  }

  # Bisection: power increases monotonically as p1 moves away from p0.
  for (i in 1:60) {
    mid <- (lo + hi) / 2
    p_mid <- achieved(mid)
    if (direction == "greater") {
      if (p_mid < power) lo <- mid else hi <- mid
    } else {
      if (p_mid < power) hi <- mid else lo <- mid
    }
  }
  (lo + hi) / 2
}
