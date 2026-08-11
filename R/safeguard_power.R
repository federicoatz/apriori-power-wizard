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

#' Shrink an estimate toward the null by z*SE, never past it
#'
#' Every safeguard bound in this file is "the confidence-interval edge that
#' makes the effect look SMALLER". For a positive estimate that is the lower
#' bound; for a negative one it is the UPPER bound. Taking the lower bound
#' unconditionally, as an earlier version of this file did, is wrong in two
#' distinct ways for a negatively-signed effect:
#'
#'   1. It moves the estimate AWAY from the null (a published r = -.30
#'      became -.40), which is anti-conservative -- the opposite of what
#'      safeguard power is for.
#'   2. Combined with a `max(bound, 1e-4)` floor it then silently flipped
#'      the sign, or (in the hazard-ratio case, which has no such floor)
#'      crossed HR = 1 outright and returned a finite, entirely
#'      plausible-looking N for an effect in the OPPOSITE direction from
#'      the one hypothesized. A published HR of 0.85 from 40 events came
#'      back as a safeguard HR of 1.11 at the 80% default.
#'
#' The bound is additionally clamped so it can never reach or cross the
#' null. When it would, the estimate is left just short of it: the required
#' N then diverges, which is the honest answer (the prior study cannot
#' support a directional claim at this confidence level) and is what the
#' results step's own guard detects and explains to the user.
#'
#' @param estimate numeric, the published point estimate, on a scale whose
#'   null value is 0 (d, h, w, Fisher's z, or log HR -- not HR itself)
#' @param se numeric > 0, its standard error on that same scale
#' @param conf_level numeric in (0,1)
#' @param one_sided logical, TRUE for the one-sided safeguard bound
#' @param floor_abs numeric > 0, how close to the null the bound may get
#' @return numeric, the shrunk estimate, with the same sign as `estimate`
#' @export
safeguard_shrink <- function(estimate, se, conf_level = 0.80,
                              one_sided = TRUE, floor_abs = 1e-4) {
  stopifnot(se > 0, conf_level > 0, conf_level < 1, floor_abs > 0)
  z <- if (one_sided) {
    stats::qnorm(conf_level)
  } else {
    stats::qnorm(1 - (1 - conf_level) / 2)
  }
  if (estimate >= 0) max(estimate - z * se, floor_abs)
  else               min(estimate + z * se, -floor_abs)
}

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
#' Plain-language disclosure for families whose safeguard interval routes
#' through a conversion that is exact only in a special case
#'
#' The safeguard interval is built on the sampling variance of Cohen's d.
#' Families whose effect metric is f, or a log odds ratio, reach that scale
#' through a conversion (f = d/2; Chinn's log-OR/1.81) which is EXACT only
#' for two groups or a single binary predictor, and approximate otherwise.
#' Saying so is the alternative to either restricting the branch or
#' pretending the approximation is an identity -- and it is the same
#' disclosure discipline the rest of the application already applies to
#' the Schoenfeld and Rao-Scott approximations.
#'
#' @param metric one of "f" (ANOVA-family) or "logor" (logistic)
#' @return character(1)
#' @export
safeguard_conversion_note <- function(metric = c("f", "logor")) {
  metric <- match.arg(metric)
  common <- paste(
    "The safeguard interval is constructed on Cohen's d, whose sampling",
    "variance is the one this correction is defined for.")
  if (identical(metric, "f")) {
    paste(common,
      "Your effect is entered as f, which reaches that scale through",
      "f = d/2 -- an identity for two groups or two measurements, and an",
      "approximation for more than that. The correction is therefore",
      "indicative rather than exact for this family, and the more levels",
      "the focal factor has, the looser it is. If the design has a small",
      "number of levels the approximation is close; if you need an exact",
      "bound, prefer the smallest-effect-of-interest branch, which depends",
      "on no prior estimate at all.")
  } else {
    paste(common,
      "Your effect is entered as a log odds ratio, which reaches that",
      "scale through Chinn's (2000) constant 1.81. That conversion assumes",
      "an underlying logistic latent variable; it is standard in",
      "meta-analysis but it is a conversion, not an identity. The",
      "correction is therefore indicative rather than exact here.")
  }
}

#' Safeguard interval for a WITHIN-SUBJECT (paired) standardized effect
#'
#' The paired counterpart to [safeguard_ci_d()], and a separate function
#' because using the two-independent-groups variance on a paired design is
#' not a small approximation. With n pairs,
#'   Var(d_z) = 1/n + d_z^2 / (2n)
#' (Becker, 1988; the same first-order form Hedges & Olkin give, with the
#' paired design's single sample of n difference scores in place of two
#' independent groups). The two-group expression evaluated at n1 = n2 = n/2
#' -- which is what this family did until v1.5.0, since it had only
#' safeguard_ci_d() to call -- yields a leading term of 4/n instead of 1/n,
#' so the standard error comes out roughly twice too large and the interval
#' shrinks the published effect far more than the evidence warrants. At
#' d_z = 0.40 from 30 pairs it recommended 980 pairs where this returns 138.
#'
#' The error was conservative, not dangerous: it asked for too many
#' participants rather than too few. It nevertheless made the safeguard
#' branch unusable for exactly the designs -- repeated measures, pre/post --
#' where a published estimate is most often the only thing available.
#'
#' @param d_published numeric, the published paired effect (Cohen's d_z)
#' @param n_pairs integer, number of PAIRS in the original study (for a
#'   within-subject design this is the number of participants)
#' @param conf_level,one_sided as in [safeguard_ci_d()]
#' @return list with d_published, se, lower, upper, conf_level, one_sided,
#'   and d_safeguard (always the edge toward the null, whatever the sign)
#' @export
safeguard_ci_dz <- function(d_published, n_pairs, conf_level = 0.80,
                            one_sided = TRUE) {
  stopifnot(n_pairs > 1, conf_level > 0, conf_level < 1)

  se <- sqrt(1 / n_pairs + d_published^2 / (2 * n_pairs))

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
    d_published = d_published, se = se,
    # Raw endpoints, as in safeguard_ci_d(): `lower` is below the estimate
    # whatever its sign, so for a negative effect it is the edge AWAY from
    # the null. Use d_safeguard.
    lower = lower, upper = upper,
    conf_level = conf_level, one_sided = one_sided,
    # Delegated rather than reimplemented. safeguard_shrink() already owns
    # the sign discipline AND the floor that keeps a collapsed interval
    # from returning exactly zero (which would make the downstream solver
    # diverge); duplicating that logic here reproduced the sign bug the
    # property tests were written to catch.
    d_safeguard = safeguard_shrink(d_published, se, conf_level, one_sided)
  )
}

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
    # Kept for backward compatibility, but note these are the raw interval
    # endpoints on the number line: `lower` is BELOW the estimate whatever
    # its sign, so for a negative published effect it is the edge AWAY from
    # the null, not the safeguard bound. Use `d_safeguard`, which is always
    # the edge toward the null. See safeguard_shrink().
    lower = lower,
    upper = upper,
    conf_level = conf_level,
    one_sided = one_sided,
    d_safeguard = safeguard_shrink(d_published, se, conf_level, one_sided)
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
    h_published = h_published, se = se,
    # See the note in safeguard_ci_d(): `lower`/`upper` are raw endpoints,
    # `h_safeguard` is the one that respects the sign of the estimate.
    lower = lower, upper = upper,
    conf_level = conf_level, one_sided = one_sided,
    h_safeguard = safeguard_shrink(h_published, se, conf_level, one_sided)
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
  # Shrinking happens on the Fisher z scale (where the SE is defined and
  # free of the estimate) and is transformed back afterwards. A negative
  # published r is shrunk UPWARD toward zero, not downward away from it.
  safeguard_r <- tanh(safeguard_shrink(z, se, conf_level, one_sided))
  list(
    r_published = r_published, se_z = se, lower = lower_r,
    conf_level = conf_level, one_sided = one_sided,
    r_safeguard = safeguard_r
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
    w_safeguard = safeguard_shrink(w_published, se, conf_level, one_sided)
  )
}

#' Safeguard-corrected input for a probability of superiority (AUC scale)
#'
#' The Wilcoxon-Mann-Whitney family's native effect size is the
#' probability of superiority p = P(X < Y) -- formally identical to the
#' area under an ROC curve, since the Mann-Whitney U statistic IS the
#' count of (X, Y) pairs with X < Y and p-hat = U/(n1*n2) is the sample
#' AUC. That identity is what makes a distribution-free interval possible
#' here: the sampling variance of the AUC has a standard large-sample form
#' (Hanley & McNeil, 1982, "The meaning and use of the area under a
#' receiver operating characteristic (ROC) curve", Radiology, 143(1),
#' 29-36):
#'
#'   Var(p-hat) = [ p(1-p) + (n1-1)(Q1 - p^2) + (n2-1)(Q2 - p^2) ] / (n1*n2)
#'   with Q1 = p/(2-p),  Q2 = 2*p^2/(1+p)
#'
#' (Q1/Q2 are the exponential-model values Hanley & McNeil recommend for
#' general use; they are conservative for distributions closer to normal.)
#' Unlike routing a published d through [safeguard_ci_d()] and converting
#' at the end -- which silently re-imports the normality assumption the
#' rank test exists to avoid -- this interval never leaves the scale the
#' test actually operates on.
#'
#' Shrinking happens on the delta = p - 0.5 scale, whose null is 0, via
#' [safeguard_shrink()]: a p above 0.5 is pulled down toward 0.5, a p
#' below 0.5 is pulled up toward it, and the bound is clamped so it can
#' never reach or cross 0.5 (the diverging-N case the results step
#' detects and explains).
#'
#' @param p_published numeric in (0,1), the published probability of
#'   superiority P(X < Y) (equivalently the sample AUC, U/(n1*n2))
#' @param n1,n2 integer, per-group sample sizes of the ORIGINAL study
#'   (if only a total N is known and the original design was balanced,
#'   pass n1 = n2 = N/2)
#' @param conf_level numeric, confidence level (default 0.80 one-sided)
#' @param one_sided logical
#' @return list(p_published, se, lower, upper, conf_level, one_sided,
#'   p_safeguard)
#' @export
safeguard_ci_auc <- function(p_published, n1, n2, conf_level = 0.80,
                              one_sided = TRUE) {
  stopifnot(p_published > 0, p_published < 1, n1 > 1, n2 > 1,
            conf_level > 0, conf_level < 1)
  p <- p_published
  q1 <- p / (2 - p)
  q2 <- 2 * p^2 / (1 + p)
  se <- sqrt((p * (1 - p) + (n1 - 1) * (q1 - p^2) + (n2 - 1) * (q2 - p^2)) /
               (n1 * n2))
  if (one_sided) {
    z <- stats::qnorm(conf_level)
    lower <- p - z * se
    upper <- Inf
  } else {
    z <- stats::qnorm(1 - (1 - conf_level) / 2)
    lower <- p - z * se
    upper <- p + z * se
  }
  list(
    p_published = p, se = se,
    # As in safeguard_ci_d(): `lower`/`upper` are raw interval endpoints on
    # the number line; `p_safeguard` is the edge toward the null (0.5),
    # whichever side of it the estimate sits on.
    lower = lower, upper = upper,
    conf_level = conf_level, one_sided = one_sided,
    p_safeguard = 0.5 + safeguard_shrink(p - 0.5, se, conf_level, one_sided)
  )
}

#' Safeguard-corrected input for a hazard ratio (log-rank / survival)
#'
#' Uses the standard large-sample variance of the log hazard ratio from a
#' two-group log-rank/Cox analysis, Var(log HR) ~= 1/d1 + 1/d2 (Peto &
#' Peto, 1972; see also Parmar & Machin, 1995, "Survival Analysis: A
#' Practical Approach", ch. 8), where d1/d2 are the number of events
#' observed in each arm. Given only the TOTAL number of events in the
#' original study (the quantity usually reported, e.g. "with 87 deaths
#' recorded..."), events are apportioned between arms by the allocation
#' ratio, giving the same (1+k)^2/k structure used in
#' [power_survival_n()]'s own required-events formula:
#'   Var(log HR) = (1+k)^2 / (k * events_published)
#'
#' @param hr_published numeric > 0, published hazard ratio
#' @param events_published integer, TOTAL number of events observed in the
#'   original study (not its total N -- event count is what determines
#'   precision in a survival analysis)
#' @param alloc_ratio numeric > 0, allocation ratio k = n2/n1 assumed for
#'   the ORIGINAL study (1 = balanced, the usual default absent other info)
#' @param conf_level numeric, confidence level (default 0.80 one-sided,
#'   consistent with the safeguard-power default used elsewhere in this app)
#' @param one_sided logical
#' @return list(se_log_hr, lower (HR scale), hr_safeguard)
#' @export
safeguard_ci_logHR <- function(hr_published, events_published, alloc_ratio = 1,
                                conf_level = 0.80, one_sided = TRUE) {
  stopifnot(hr_published > 0, events_published > 0, alloc_ratio > 0)
  k <- alloc_ratio
  log_hr <- log(hr_published)
  se <- sqrt((1 + k)^2 / (k * events_published))
  # Shrinking happens on the LOG HR scale, whose null is 0 (HR = 1) and on
  # which the SE is defined. safeguard_shrink() moves the estimate toward
  # that null from whichever side it starts on, and -- critically here --
  # clamps it just short of crossing. Without the clamp a protective
  # HR = 0.85 from 40 events came back as a safeguard HR of 1.11 even at
  # the 80% default: a finite, plausible-looking sample size for an effect
  # in the opposite direction from the one being planned for.
  bound_log <- safeguard_shrink(log_hr, se, conf_level, one_sided)
  list(
    hr_published = hr_published, se_log_hr = se,
    conf_level = conf_level, one_sided = one_sided,
    hr_safeguard = exp(bound_log)
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
    "estimates. Powering a new study on the published point estimate can",
    "therefore under-power it. Safeguard power",
    "uses the lower bound of a one-sided confidence interval around the",
    "published effect as a more conservative, defensible input. It accounts",
    "for sampling uncertainty in that estimate; it does not itself correct",
    "publication bias or other selection mechanisms."
  )
}
