## power_clustered_cat.R
## -----------------------------------------------------------------------
## Analysis family: clustered design with a BINARY or CATEGORICAL outcome
## -- the counterpart to power_clustered.R, which covers the continuous
## case. Participants are grouped (matching groups / lab sessions, or
## classrooms / clinics / stores), so their observations are not
## independent, and the outcome is a yes/no or a choice among 3+
## categories.
##
## Why this exists: this combination was, until now, explicitly declared
## unsupported by the app's decision helper -- yet it is one of the most
## common designs in experimental economics, where the headline outcome is
## routinely a RATE measured on participants who interacted in fixed
## matching groups (cooperation rate, contribution vs. free-ride, which of
## three strategies was chosen).
##
## METHOD. Identical in structure to the continuous case: solve the
## individually-randomized sample size with the already-validated
## unclustered family, then inflate it by the design effect
##   DE = 1 + (m - 1) * ICC
## (Donner & Klar, 2000). The design effect is a property of the sampling
## variance of a cluster mean, not of the outcome's distribution, which is
## why the SAME inflation applies to a proportion as to a mean; Donner &
## Klar treat the binary case explicitly.
##
##   binary       -> Cohen's h, via power_proportions_n()  (pwr::pwr.2p.test)
##   categorical  -> Cohen's w, via power_chisq_n()        (pwr::pwr.chisq.test)
##
## SCOPE NOTE, stated plainly because it matters for the categorical case:
## applying a single design effect to a chi-square test is the standard
## first-order adjustment, and is the same quantity the more refined
## Rao-Scott (1984) correction estimates from data. It is an approximation
## -- appropriate for planning, where the ICC is itself an estimate, but
## not a substitute for a design-based analysis of the resulting data. The
## app says so in the module's own text rather than leaving it implicit.
## -----------------------------------------------------------------------

#' Sample size for a clustered two-arm design with a BINARY outcome
#'
#' @param h numeric, individual-level Cohen's h (the arcsine-transformed
#'   difference between the two proportions that would apply if
#'   individuals, not clusters, were the unit -- inflated internally)
#' @param icc numeric, intraclass correlation in [0, 1)
#' @param cluster_size numeric, average participants per cluster
#' @param sig_level,power,alternative,allocation_ratio as usual
#' @export
power_clustered_binary_n <- function(h, icc, cluster_size, sig_level = 0.05,
                                      power = 0.80,
                                      alternative = c("two.sided", "greater", "less"),
                                      allocation_ratio = 1) {
  alternative <- match.arg(alternative)
  stopifnot(h > 0, icc >= 0, icc < 1, cluster_size >= 1,
            sig_level > 0, sig_level < 1, power > 0, power < 1)

  de <- cluster_design_effect(cluster_size, icc)
  naive <- power_proportions_n(h = h, sig_level = sig_level, power = power,
                                alternative = alternative,
                                allocation_ratio = allocation_ratio)

  k1 <- round_up_n((naive$n1 * de) / cluster_size)
  k2 <- round_up_n((naive$n2 * de) / cluster_size)
  n1 <- k1 * cluster_size
  n2 <- k2 * cluster_size

  achieved <- power_clustered_binary_at_n(n1 = n1, n2 = n2, h = h, icc = icc,
                                           cluster_size = cluster_size,
                                           sig_level = sig_level,
                                           alternative = alternative)

  list(
    n1 = n1, n2 = n2, n_total = n1 + n2,
    k1 = k1, k2 = k2, n_clusters_total = k1 + k2, cluster_size = cluster_size,
    icc = icc, design_effect = de,
    h = h, sig_level = sig_level, power_target = power, power_achieved = achieved,
    alternative = alternative, allocation_ratio = allocation_ratio,
    method = "Clustered two-arm design, binary outcome: individually-randomized N (pwr::pwr.2p.test, Cohen's h) inflated by the design effect 1 + (m-1)*ICC (Donner & Klar, 2000)"
  )
}

#' Achieved power for a clustered binary design at given per-arm totals
#' @param n1,n2 numeric, TOTAL participants per arm (clusters x cluster_size)
#' @export
power_clustered_binary_at_n <- function(n1, n2 = n1, h, icc, cluster_size,
                                         sig_level = 0.05,
                                         alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  de <- cluster_design_effect(cluster_size, icc)
  power_proportions_at_n(n1 = n1 / de, n2 = n2 / de, h = h,
                          sig_level = sig_level, alternative = alternative)
}

#' Minimum detectable individual-level h for fixed per-arm totals
#' @export
power_clustered_binary_min_h <- function(n1, n2 = n1, icc, cluster_size,
                                          sig_level = 0.05, power = 0.80,
                                          alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  de <- cluster_design_effect(cluster_size, icc)
  power_proportions_min_h(n1 = n1 / de, n2 = n2 / de, sig_level = sig_level,
                           power = power, alternative = alternative)
}

#' Sample size for a clustered design with a CATEGORICAL outcome (chi-square)
#'
#' @param w numeric, individual-level Cohen's w
#' @param df integer, degrees of freedom of the chi-square test
#' @param icc,cluster_size,sig_level,power as above
#' @export
power_clustered_cat_n <- function(w, df, icc, cluster_size, sig_level = 0.05,
                                   power = 0.80) {
  stopifnot(w > 0, df >= 1, icc >= 0, icc < 1, cluster_size >= 1,
            sig_level > 0, sig_level < 1, power > 0, power < 1)

  de <- cluster_design_effect(cluster_size, icc)
  naive <- power_chisq_n(w = w, df = df, sig_level = sig_level, power = power)

  n_clusters_total <- round_up_n((naive$n_total * de) / cluster_size)
  n_total <- n_clusters_total * cluster_size

  achieved <- power_clustered_cat_at_n(n_total = n_total, w = w, df = df,
                                        icc = icc, cluster_size = cluster_size,
                                        sig_level = sig_level)

  list(
    n_total = n_total,
    n_clusters_total = n_clusters_total, cluster_size = cluster_size,
    icc = icc, design_effect = de,
    w = w, df = df, sig_level = sig_level, power_target = power,
    power_achieved = achieved,
    method = "Clustered design, categorical outcome: individually-randomized N (pwr::pwr.chisq.test, Cohen's w) inflated by the design effect 1 + (m-1)*ICC (Donner & Klar, 2000; a first-order approximation to the Rao-Scott correction)"
  )
}

#' Achieved power for a clustered categorical design at a given total N
#' @export
power_clustered_cat_at_n <- function(n_total, w, df, icc, cluster_size,
                                      sig_level = 0.05) {
  de <- cluster_design_effect(cluster_size, icc)
  power_chisq_at_n(n_total = n_total / de, w = w, df = df, sig_level = sig_level)
}

#' Minimum detectable individual-level w for a fixed total N
#' @export
power_clustered_cat_min_w <- function(n_total, df, icc, cluster_size,
                                       sig_level = 0.05, power = 0.80) {
  de <- cluster_design_effect(cluster_size, icc)
  power_chisq_min_w(n_total = n_total / de, df = df, sig_level = sig_level,
                     power = power)
}
