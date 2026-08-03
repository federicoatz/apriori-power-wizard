## safeguard_power.R
## -----------------------------------------------------------------------
## Branch 2 of the effect-size step: "safeguard power" after
## Perugini, Gallucci & Costantini (2014, "Safeguard power as a protection
## against imprecise power estimates", Perspectives on Psychological
## Science, 9(3), 319-332).
##
## Rationale: an effect size taken directly from a single published study
## is a noisy point estimate, and published effects are systematically
## inflated by publication bias and the "winner's curse" (significant
## results are more likely to be published, and among significant results
## the largest ones are most likely to clear the significance threshold by
## chance). Using the published point estimate as-is therefore tends to
## under-power replications and extensions. The safeguard-power approach
## uses the lower bound of a one-sided confidence interval around the
## published estimate as a more conservative input to the sample-size
## calculation.
## -----------------------------------------------------------------------

#' Confidence interval around a published Cohen's d
#'
#' Uses the standard (large-sample normal-approximation) sampling variance
#' of Cohen's d for two independent groups:
#'   Var(d) = (n1+n2)/(n1*n2) + d^2 / (2*(n1+n2))
#' (Hedges & Olkin, 1985, Statistical Methods for Meta-Analysis, eq. 3 of
#' ch. 5; also used by Perugini et al. 2014, eq. 2).
#'
#' @param d_published numeric, the published (point-estimate) Cohen's d
#' @param n1,n2 integer, per-group sample sizes of the ORIGINAL study
#'   (if only a total N is known and the original design was balanced,
#'   pass n1 = n2 = N/2)
#' @param conf_level numeric in (0,1), confidence level for the interval
#'   (Perugini et al. recommend an 80% ONE-SIDED interval as the default
#'   "safeguard" level; 95% two-sided is offered as a stricter alternative)
#' @param one_sided logical, TRUE = one-sided lower bound (default, per the
#'   original safeguard-power proposal), FALSE = two-sided interval
#' @return list(se, lower, upper, d_published)
#' @export
safeguard_ci_d <- function(d_published, n1, n2, conf_level = 0.80,
                            one_sided = TRUE) {
  stopifnot(n1 > 0, n2 > 0, conf_level > 0, conf_level < 1)

  se <- sqrt((n1 + n2) / (n1 * n2) + d_published^2 / (2 * (n1 + n2)))

  if (one_sided) {
    z <- stats::qnorm(conf_level)
    lower <- d_published - z * se
    upper <- Inf
  } else {
    z <- stats::qnorm(1 - (1 - conf_level) / 2)
    lower <- d_published - z * se
    upper <- d_published + z * se
  }

  list(
    d_published = d_published,
    se = se,
    lower = lower,
    upper = upper,
    conf_level = conf_level,
    one_sided = one_sided,
    # The safeguard effect size is floored at a tiny positive number so
    # downstream N-solvers don't choke on d <= 0 (a published effect whose
    # lower CI bound crosses zero signals a genuinely under-informative
    # prior study; the UI surfaces this as an explicit warning rather than
    # silently returning an enormous or undefined N).
    d_safeguard = max(lower, 1e-4)
  )
}

#' Safeguard-corrected input for a proportions/logistic effect (h or log-OR)
#'
#' Same logic as [safeguard_ci_d()] but generic for any effect size metric
#' whose sampling variance can be approximated as Var(theta) ~ 1/n1 + 1/n2
#' scaled by a metric-specific constant. For Cohen's h (arcsine-difference
#' effect size for two proportions) the large-sample variance of h is
#' approximately (1/n1 + 1/n2) (Cohen, 1988, ch. 6).
#'
#' @param h_published numeric, published Cohen's h
#' @param n1,n2 integer, original study's per-group n
#' @param conf_level numeric, confidence level (default 0.80 one-sided)
#' @param one_sided logical
#' @export
safeguard_ci_h <- function(h_published, n1, n2, conf_level = 0.80,
                            one_sided = TRUE) {
  stopifnot(n1 > 0, n2 > 0)
  se <- sqrt(1 / n1 + 1 / n2)
  if (one_sided) {
    z <- stats::qnorm(conf_level)
    lower <- h_published - z * se
    upper <- Inf
  } else {
    z <- stats::qnorm(1 - (1 - conf_level) / 2)
    lower <- h_published - z * se
    upper <- h_published + z * se
  }
  list(
    h_published = h_published, se = se, lower = lower, upper = upper,
    conf_level = conf_level, one_sided = one_sided,
    h_safeguard = max(lower, 1e-4)
  )
}

#' Confidence interval around a published correlation (Fisher z), used as
#' the safeguard input for regression and logistic-regression effect
#' sizes when the published estimate is a (semi-partial) correlation r
#' or a standardized beta approximated as r.
#'
#' Uses the standard Fisher (1921) variance-stabilizing transformation:
#'   z = atanh(r), SE(z) = 1 / sqrt(n - 3)
#' which is exact under bivariate normality and is the conventional way
#' to build a confidence interval around a correlation coefficient.
#'
#' @param r_published numeric in (-1,1), published correlation / standardized beta
#' @param n integer, total N of the original study
#' @param conf_level numeric, confidence level (default 0.80 one-sided,
#'   consistent with the safeguard-power default used elsewhere in this app)
#' @param one_sided logical
#' @export
safeguard_ci_r <- function(r_published, n, conf_level = 0.80, one_sided = TRUE) {
  stopifnot(n > 3, r_published > -1, r_published < 1)
  z <- atanh(r_published)
  se <- 1 / sqrt(n - 3)
  if (one_sided) {
    zcrit <- stats::qnorm(conf_level)
    lower_z <- z - zcrit * se
  } else {
    zcrit <- stats::qnorm(1 - (1 - conf_level) / 2)
    lower_z <- z - zcrit * se
  }
  lower_r <- tanh(lower_z)
  list(
    r_published = r_published, se_z = se, lower = lower_r,
    conf_level = conf_level, one_sided = one_sided,
    r_safeguard = max(lower_r, 1e-4)
  )
}

#' Safeguard-corrected input for a chi-square effect (Cohen's w)
#'
#' Derived from the noncentral chi-square distribution rather than adapted
#' from a published closed-form variance (unlike [safeguard_ci_d()]/
#' [safeguard_ci_h()]/[safeguard_ci_r()], no single textbook formula for
#' Var(w) exists across arbitrary df). Under the alternative, the test
#' statistic X^2 ~ noncentral-chi-square(df, ncp = N*w^2), with
#' Var(X^2) = 2*df + 4*N*w^2 (a standard noncentral-chi-square identity).
#' Since w-hat = sqrt(X^2 / N), the delta method on g(x) = sqrt(x) gives
#' Var(w-hat) ~= Var(X^2) / (4 * N^2 * w^2) = df / (2*N^2*w^2) + 1/N, a
#' large-sample approximation (accurate when N is not tiny relative to df).
#'
#' @param w_published numeric, published Cohen's w
#' @param n_published integer, total N of the original study
#' @param df integer, degrees of freedom of the CURRENT design (not the
#'   published study's df -- the safeguard correction is being applied to
#'   the effect size you plan to power a new study for)
#' @param conf_level numeric, confidence level (default 0.80 one-sided)
#' @param one_sided logical
#' @export
safeguard_ci_w <- function(w_published, n_published, df, conf_level = 0.80,
                            one_sided = TRUE) {
  stopifnot(n_published > 0, w_published > 0, df >= 1)
  se <- sqrt(df / (2 * n_published^2 * w_published^2) + 1 / n_published)
  if (one_sided) {
    z <- stats::qnorm(conf_level)
    lower <- w_published - z * se
    upper <- Inf
  } else {
    z <- stats::qnorm(1 - (1 - conf_level) / 2)
    lower <- w_published - z * se
    upper <- w_published + z * se
  }
  list(
    w_published = w_published, se = se, lower = lower, upper = upper,
    conf_level = conf_level, one_sided = one_sided,
    w_safeguard = max(lower, 1e-4)
  )
}

#' Citation string for the safeguard-power method (used in the report text)
#' @export
safeguard_power_citation <- function() {
  paste(
    "Perugini, M., Gallucci, M., & Costantini, G. (2014). Safeguard power",
    "as a protection against imprecise power estimates. Perspectives on",
    "Psychological Science, 9(3), 319-332.",
    "https://doi.org/10.1177/1745691614528519"
  )
}

#' Short explanatory note for the UI (why safeguard power matters)
#' @export
safeguard_power_explainer <- function() {
  paste(
    "Effect sizes drawn from a single published study are noisy point",
    "estimates, and published effects tend to be inflated by publication",
    "bias and the 'winner's curse' (a result had to be large enough,",
    "often by chance, to clear the significance threshold in order to be",
    "published at all). Powering a new study on the published point",
    "estimate therefore systematically under-powers it. Safeguard power",
    "uses the lower bound of a one-sided confidence interval around the",
    "published effect as a more conservative, defensible input."
  )
}
