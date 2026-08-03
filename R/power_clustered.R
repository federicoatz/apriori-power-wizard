## power_clustered.R
## -----------------------------------------------------------------------
## Analysis family: two-arm cluster-randomized (or multi-site) trial with
## a continuous outcome -- whole clusters (classrooms, clinics, stores,
## villages), not individuals, are assigned to condition.
##
## Method: the standard closed-form design-effect approach (Donner & Klar,
## 2000; Hayes & Moulton, 2017, "Cluster Randomised Trials"), NOT
## simulation-based. An individually-randomized sample size is first
## solved with pwr::pwr.t.test() (same machinery as power_two_means.R),
## then inflated by the design effect
##   DE = 1 + (m - 1) * ICC
## where m = average cluster size and ICC = intraclass correlation. The
## inflated per-arm N is then rounded up to a whole number of clusters.
## This mirrors the app-wide "closed-form only, no Monte Carlo" design
## constraint (see app.R footer / README).
## -----------------------------------------------------------------------

#' Design effect (variance inflation factor) for a cluster-randomized design
#' @export
cluster_design_effect <- function(cluster_size, icc) {
  1 + (cluster_size - 1) * icc
}

#' Sample size (individuals and clusters, per arm) for a two-arm CRT
#'
#' @param d numeric, individual-level Cohen's d (the standardized mean
#'   difference that would apply if individuals -- not clusters -- were
#'   randomized; this is inflated internally by the design effect)
#' @param icc numeric, intraclass correlation, in [0, 1)
#' @param cluster_size numeric, average number of individuals per cluster
#' @param sig_level numeric, alpha
#' @param power numeric, target power (1 - beta)
#' @param alternative "two.sided", "greater", or "less"
#' @param allocation_ratio numeric, n2/n1 (individual-level, applied before
#'   cluster rounding). 1 = balanced design (default).
#' @return list with n1, n2, n_total (individuals), k1, k2,
#'   n_clusters_total, cluster_size, icc, design_effect, and the usual
#'   power/alpha/method fields
#' @export
power_clustered_n <- function(d, icc, cluster_size, sig_level = 0.05, power = 0.80,
                               alternative = c("two.sided", "greater", "less"),
                               allocation_ratio = 1) {
  alternative <- match.arg(alternative)
  stopifnot(d > 0, icc >= 0, icc < 1, cluster_size >= 1,
            sig_level > 0, sig_level < 1, power > 0, power < 1)

  de <- cluster_design_effect(cluster_size, icc)

  naive <- power_two_means_n(d = d, sig_level = sig_level, power = power,
                              alternative = alternative,
                              allocation_ratio = allocation_ratio)

  k1 <- round_up_n((naive$n1 * de) / cluster_size)
  k2 <- round_up_n((naive$n2 * de) / cluster_size)
  n1 <- k1 * cluster_size
  n2 <- k2 * cluster_size

  achieved <- power_clustered_at_n(n1 = n1, n2 = n2, d = d, icc = icc,
                                    cluster_size = cluster_size,
                                    sig_level = sig_level, alternative = alternative)

  list(
    n1 = n1, n2 = n2, n_total = n1 + n2,
    k1 = k1, k2 = k2, n_clusters_total = k1 + k2, cluster_size = cluster_size,
    icc = icc, design_effect = de,
    d = d, sig_level = sig_level, power_target = power, power_achieved = achieved,
    alternative = alternative, allocation_ratio = allocation_ratio,
    method = "Cluster-randomized two-arm trial: individually-randomized N (pwr::pwr.t.test) inflated by the design effect 1 + (m-1)*ICC (Donner & Klar, 2000)"
  )
}

#' Achieved power for a CRT at given per-arm individual totals (curve helper)
#'
#' @param n1,n2 numeric, TOTAL individuals in each arm (i.e. clusters x
#'   cluster_size, not a count of clusters)
#' @export
power_clustered_at_n <- function(n1, n2 = n1, d, icc, cluster_size,
                                  sig_level = 0.05,
                                  alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  de <- cluster_design_effect(cluster_size, icc)
  power_two_means_at_n(n1 = n1 / de, n2 = n2 / de, d = d,
                        sig_level = sig_level, alternative = alternative)
}

#' Minimum detectable individual-level d for fixed per-arm totals (inverse)
#' @export
power_clustered_min_d <- function(n1, n2 = n1, icc, cluster_size,
                                   sig_level = 0.05, power = 0.80,
                                   alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  de <- cluster_design_effect(cluster_size, icc)
  power_two_means_min_d(n1 = n1 / de, n2 = n2 / de, sig_level = sig_level,
                         power = power, alternative = alternative)
}
