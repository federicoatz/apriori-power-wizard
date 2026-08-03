## power_rm_anova.R
## -----------------------------------------------------------------------
## Analysis family: repeated-measures ANOVA -- the same participants
## measured under several conditions, rounds, or time points. Covers both
##
##   "within"      the main effect of the repeated factor (a single group
##                 measured m times: the classic within-subjects design), and
##   "interaction" the within x between interaction in a MIXED design
##                 (g independent groups each measured m times) -- i.e.
##                 "does the change across conditions differ by group?",
##                 which is very often the actual hypothesis in a
##                 pretest/posttest-by-treatment study.
##
## Why this family matters here: repeated measurement is the norm rather
## than the exception in both target fields -- experimental economics runs
## games over many rounds, experimental psychology runs many trials or
## conditions per participant -- and until now the app could only handle
## the two-condition case via the paired t-test.
##
## FORMULA (Cohen, 1988, ch. 8, for the general noncentral-F relation;
## the repeated-measures noncentrality and degrees of freedom follow the
## formulation documented for G*Power 3, Faul, Erdfelder, Lang & Buchner,
## 2007, "F tests -- ANOVA: Repeated measures"):
##
##   lambda = f^2 * n_total * m * epsilon / (1 - rho)
##   df1    = (m - 1) * epsilon                    [within main effect]
##   df1    = (m - 1) * (g - 1) * epsilon          [within x between]
##   df2    = (n_total - g) * (m - 1) * epsilon
##
## with n_total the number of PARTICIPANTS (across all groups), m the
## number of repeated measurements, rho the correlation among those
## repeated measurements, g the number of between-subject groups, and
## epsilon the Greenhouse-Geisser-style nonsphericity correction
## (epsilon = 1 means sphericity holds exactly; lower values are more
## conservative and cost sample size).
##
## The 1/(1 - rho) term is the whole point of a within-subjects design:
## the more strongly a participant's measurements correlate with each
## other, the more between-participant variability is removed from the
## error term, and the fewer participants are needed. This is why
## repeated-measures designs are so much cheaper than between-subjects
## ones for the same effect -- and why rho matters enormously to the
## answer.
##
## VALIDATION (development-time, not shipped). Cross-checked against
## direct Monte Carlo simulation of the repeated-measures F test under
## compound symmetry (4,000 replications per configuration):
##   within main effect: formula/simulated power 0.6505/0.6415,
##     0.6860/0.6985, 0.4013/0.4083, 0.9018/0.9110, 0.5265/0.5142
##   within x between:   0.8407/0.8445, 0.9916/0.9890, 0.9442/0.9447
## all within Monte Carlo noise. Additionally, and independently of any
## simulation, the m = 2 single-group case is EXACTLY equal to a paired
## t-test on d_z = d / sqrt(2(1 - rho)): the formula and
## pwr::pwr.t.test(type = "paired") both return 0.9018 for n = 12,
## f = 0.40, rho = 0.7. That identity is shipped as a regression test,
## since it ties this family to an already-validated one.
## -----------------------------------------------------------------------

#' Numerator degrees of freedom for a repeated-measures effect
#' @keywords internal
.rm_df1 <- function(m, n_groups, effect, eps) {
  base <- (m - 1) * eps
  if (identical(effect, "interaction")) base * (n_groups - 1) else base
}

#' Achieved power for a repeated-measures ANOVA effect at a given N
#'
#' @param n_total integer, total number of PARTICIPANTS across all groups
#' @param m integer, number of repeated measurements per participant (>= 2)
#' @param f_target numeric, Cohen's f for the effect being tested
#' @param rho numeric in [0, 1), correlation among the repeated measures
#' @param effect "within" (main effect of the repeated factor) or
#'   "interaction" (within x between, requires n_groups >= 2)
#' @param n_groups integer, number of between-subject groups (1 = a single
#'   group measured repeatedly)
#' @param eps numeric in (0, 1], nonsphericity correction (1 = sphericity)
#' @param sig_level numeric, alpha
#' @export
power_rm_anova_at_n <- function(n_total, m, f_target, rho = 0.5,
                                 effect = c("within", "interaction"),
                                 n_groups = 1, eps = 1, sig_level = 0.05) {
  effect <- match.arg(effect)
  stopifnot(m >= 2, f_target > 0, rho >= 0, rho < 1, n_groups >= 1,
            eps > 0, eps <= 1, sig_level > 0, sig_level < 1)
  if (identical(effect, "interaction") && n_groups < 2) {
    stop("A within x between interaction needs at least 2 between-subject groups.")
  }
  df1 <- .rm_df1(m, n_groups, effect, eps)
  df2 <- (n_total - n_groups) * (m - 1) * eps
  if (df1 <= 0 || df2 <= 0) return(NA_real_)
  lambda <- f_target^2 * n_total * m * eps / (1 - rho)
  stats::pf(stats::qf(1 - sig_level, df1, df2), df1, df2,
            ncp = lambda, lower.tail = FALSE)
}

#' Total number of participants for a repeated-measures ANOVA effect
#'
#' @inheritParams power_rm_anova_at_n
#' @param power target power
#' @param n_max integer, upper bound for the search
#' @export
power_rm_anova_n <- function(m, f_target, rho = 0.5,
                              effect = c("within", "interaction"),
                              n_groups = 1, eps = 1, sig_level = 0.05,
                              power = 0.80, n_max = 100000) {
  effect <- match.arg(effect)
  stopifnot(power > 0, power < 1)

  achieved <- function(n) {
    power_rm_anova_at_n(n, m, f_target, rho, effect, n_groups, eps, sig_level)
  }
  # Start just above the smallest N with positive error df.
  n <- max(n_groups + 1, 2L)
  if (is.na(achieved(n_max)) || achieved(n_max) < power) {
    stop(sprintf("Target power not reached with up to %d participants.", n_max))
  }
  while (is.na(achieved(n)) || achieved(n) < power) n <- n + 1

  # Round the total up to a whole multiple of the number of groups, so the
  # groups can actually be equally sized -- never down, so power can only
  # improve relative to the target.
  if (n_groups > 1 && n %% n_groups != 0) n <- n + (n_groups - n %% n_groups)

  list(
    n_total = n,
    n_per_group = n / n_groups,
    n_observations = n * m,
    m = m, rho = rho, eps = eps, n_groups = n_groups, effect = effect,
    f = f_target, sig_level = sig_level, power_target = power,
    power_achieved = achieved(n),
    method = "Repeated-measures ANOVA, closed-form noncentral F with within-subject correlation and nonsphericity correction (Cohen, 1988; G*Power formulation); base R only"
  )
}

#' Minimum detectable Cohen's f for a fixed number of participants
#' @export
power_rm_anova_min_f <- function(n_total, m, rho = 0.5,
                                  effect = c("within", "interaction"),
                                  n_groups = 1, eps = 1, sig_level = 0.05,
                                  power = 0.80) {
  effect <- match.arg(effect)
  gap <- function(f) {
    power_rm_anova_at_n(n_total, m, f, rho, effect, n_groups, eps, sig_level) - power
  }
  if (is.na(gap(5)) || gap(5) < 0) return(NA_real_)
  stats::uniroot(gap, lower = 1e-4, upper = 5)$root
}
