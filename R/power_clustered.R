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
##
## DEGREES OF FREEDOM -- why this file does not simply hand n/DE to
## pwr::pwr.t.test() and report what comes back.
##
## Deflating the per-arm N by the design effect and passing the result to
## a two-sample t-test gets the NONCENTRALITY exactly right: with k
## clusters of size m per arm, a t-test on cluster means has
##   d_cluster = d * sqrt(m / DE),  ncp = d_cluster * sqrt(k/2)
##             = d * sqrt(n_arm / (2 * DE))
## which is identical to what pwr computes from n_eff = n_arm / DE. What
## it gets WRONG is the reference distribution: pwr derives its degrees of
## freedom from whatever n it is handed, so it reports 2*(n_arm/DE) - 2,
## while the analysis the researcher will actually run -- a t-test on
## cluster means, or a mixed model -- has df = k1 + k2 - 2, one per
## cluster, not one per effective individual.
##
## The two agree closely when there are many clusters and diverge sharply
## when there are few large ones, which is precisely the regime this app's
## audience works in (matching groups in a public-goods game, lab
## sessions). At m = 30, ICC = .02, 4 clusters per arm, the deflated-n df
## gives .865 where the cluster-level df gives .729. The error is in the
## OPTIMISTIC direction -- the app would promise power the design does not
## deliver -- which is the one direction nothing else in this app rounds
## toward. So power_clustered_at_n() below builds the noncentral t itself
## on df = k1 + k2 - 2, and power_clustered_n() refines the design-effect
## starting point upward until the target is genuinely met at that df.
##
## WHAT THIS COSTS, stated because it is not free. Cluster-level df is a
## property of the DESIGN, not of the ICC: when whole clusters are assigned
## to condition, the treatment effect is replicated across clusters, so
## df = k1 + k2 - 2 holds even as the ICC goes to zero. (This is the same
## reason a mixed model with Kenward-Roger or Satterthwaite df returns
## roughly the between-cluster df for a cluster-level predictor no matter
## how small the estimated random-intercept variance is.) The visible
## consequence is that ICC = 0 no longer collapses to the
## individually-randomized answer: at ICC = 0 with m = 20 the app used to
## report 4 clusters per arm, which in fact delivers .750, and now reports
## 5. A user who genuinely knows the ICC is zero -- and therefore intends
## an individual-level test -- is being asked for more than they strictly
## need. That is the direction this app already errs in everywhere else
## (see the module's own "when in doubt use a LARGER ICC" guidance), and
## it is the direction to err in when the alternative is promising power
## the design cannot deliver.
##
## What still holds exactly is the m = 1 case: one participant per cluster
## makes k = n, df = n1 + n2 - 2, and the result is identical to
## power_two_means_at_n() to machine precision. That identity is the
## anchor tying this family to an already-pinned reference, and it is
## shipped as a regression test.
##
## The binary/categorical counterparts in power_clustered_cat.R do NOT
## get the same treatment, for a reason documented there: they rest on
## normal and chi-square approximations that have no n-derived df to
## correct in the first place.
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

  # Donner & Klar starting point: the individually-randomized N inflated by
  # the design effect, rounded up to whole clusters. This lands close to the
  # answer but is systematically one to a few clusters short once power is
  # evaluated on cluster-level df (see the header), so it is a starting
  # value to refine from, not the answer.
  k2_from_k1 <- function(k) max(round_up_n(k * allocation_ratio), 1L)
  k1 <- max(round_up_n((naive$n1 * de) / cluster_size), 1L)

  # df = k1 + k2 - 2 must be at least 1 for the reference t distribution to
  # exist at all, so two clusters in the reference arm is the floor.
  min_k1 <- 2L
  k1 <- max(k1, min_k1)

  reached <- function(k) {
    pw <- power_clustered_at_n(n1 = k * cluster_size,
                                n2 = k2_from_k1(k) * cluster_size,
                                d = d, icc = icc, cluster_size = cluster_size,
                                sig_level = sig_level, alternative = alternative)
    is.finite(pw) && pw >= power
  }
  # Refine in whichever direction the starting point missed by, so the result
  # is still the SMALLEST cluster count meeting the target -- the same
  # pattern power_two_means_n() and power_proportions_n() use for
  # individuals. assert_solvable_n() bounds the walk on a near-null effect.
  while (!reached(k1)) {
    k1 <- k1 + 1L
    assert_solvable_n(k1 * cluster_size, "effect size d")
  }
  while (k1 > min_k1 && reached(k1 - 1L)) k1 <- k1 - 1L

  k2 <- k2_from_k1(k1)
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
    method = "Cluster-randomized two-arm trial: individually-randomized N (pwr::pwr.t.test) inflated by the design effect 1 + (m-1)*ICC (Donner & Klar, 2000), with power evaluated on cluster-level degrees of freedom (k1 + k2 - 2)"
  )
}

#' Achieved power for a CRT at given per-arm individual totals (curve helper)
#'
#' Noncentrality comes from the design-effect-deflated N exactly as in
#' Donner & Klar; the reference distribution is a t on CLUSTER-level
#' degrees of freedom, k1 + k2 - 2, matching the analysis the design
#' actually supports rather than the effective individual count. See the
#' file header for why the two differ and by how much.
#'
#' The tail arithmetic deliberately mirrors pwr::pwr.t.test()'s own
#' convention, including its treatment of `alternative = "less"` (which,
#' with a positive d, describes an effect in the direction opposite to the
#' one being tested and so returns a power near zero) -- this family should
#' not disagree with the other fifteen about what a one-sided test means.
#'
#' @param n1,n2 numeric, TOTAL individuals in each arm (i.e. clusters x
#'   cluster_size, not a count of clusters)
#' @export
power_clustered_at_n <- function(n1, n2 = n1, d, icc, cluster_size,
                                  sig_level = 0.05,
                                  alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  de <- cluster_design_effect(cluster_size, icc)

  # Effective (design-effect-deflated) per-arm sizes drive the noncentrality;
  # the raw cluster counts drive the degrees of freedom.
  ne1 <- n1 / de
  ne2 <- n2 / de
  df <- n1 / cluster_size + n2 / cluster_size - 2
  if (!is.finite(df) || df < 1) return(NA_real_)

  ncp <- d * sqrt(ne1 * ne2 / (ne1 + ne2))

  if (identical(alternative, "two.sided")) {
    qu <- stats::qt(sig_level / 2, df, lower.tail = FALSE)
    stats::pt(qu, df, ncp = abs(ncp), lower.tail = FALSE) +
      stats::pt(-qu, df, ncp = abs(ncp), lower.tail = TRUE)
  } else if (identical(alternative, "greater")) {
    stats::pt(stats::qt(sig_level, df, lower.tail = FALSE), df,
               ncp = ncp, lower.tail = FALSE)
  } else {
    stats::pt(stats::qt(sig_level, df, lower.tail = TRUE), df,
               ncp = ncp, lower.tail = TRUE)
  }
}

#' Minimum detectable individual-level d for fixed per-arm totals (inverse)
#'
#' Solved numerically against power_clustered_at_n() rather than delegated
#' to pwr, so the inverse inverts the SAME function the forward direction
#' reports -- otherwise the sensitivity analysis would quietly answer on
#' effective-individual df while the headline number used cluster df.
#' @export
power_clustered_min_d <- function(n1, n2 = n1, icc, cluster_size,
                                   sig_level = 0.05, power = 0.80,
                                   alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  pw <- function(d) {
    power_clustered_at_n(n1 = n1, n2 = n2, d = d, icc = icc,
                          cluster_size = cluster_size, sig_level = sig_level,
                          alternative = alternative)
  }
  if (!is.finite(pw(1e-6))) return(NA_real_)
  # Upper bracket: 10 is the same ceiling pwr uses for a standardized mean
  # difference. A design too small to reach the target power even at d = 10
  # has no minimum detectable effect to report.
  if (pw(10) < power) return(NA_real_)
  stats::uniroot(function(d) pw(d) - power, interval = c(1e-6, 10),
                  tol = 1e-8)$root
}
