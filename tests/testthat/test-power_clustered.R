## This family is anchored to an already-pinned reference two ways.
##
## EXACTLY, at cluster_size = 1: one participant per cluster makes the
## cluster count equal the participant count and the design effect equal 1,
## so the cluster-level t reduces to the ordinary two-sample t and must
## agree with power_two_means_at_n() (pinned in test-power_two_means.R) to
## machine precision. This replaces the older ICC = 0 anchor, which stopped
## holding when power moved onto cluster-level degrees of freedom: df =
## k1 + k2 - 2 is a property of assigning whole clusters to condition, not
## of the ICC, so it applies at ICC = 0 too and the clustered answer is
## legitimately larger there. See the header of R/power_clustered.R.
##
## STRUCTURALLY, everywhere else: the design-effect method (Donner & Klar,
## 2000; Hayes & Moulton, 2017) is standard and well-documented, but no
## second package (e.g. `clusterPower`) was available to cross-check an
## exact worked value at a general (m, ICC). Those tests pin the properties
## the formula must satisfy -- strong enough to catch sign errors, swapped
## arguments, or a broken inflation step -- plus a direct check that the
## reported power really is the noncentral t on cluster df, computed here
## from the definition rather than from the implementation. Please verify
## against `clusterPower::cps.nct()` or a G*Power/Optimal Design comparison
## locally before relying on this for a real study.

test_that("clustered: cluster_size = 1 reduces EXACTLY to the two-sample t", {
  skip_if_not_installed("pwr")
  for (icc in c(0, 0.3)) {
    # With m = 1 the design effect is 1 + 0 * icc = 1 for any icc, so both
    # the noncentrality and the df must match the unclustered family.
    expect_equal(
      power_clustered_at_n(n1 = 100, n2 = 100, d = 0.5, icc = icc, cluster_size = 1),
      power_two_means_at_n(n1 = 100, n2 = 100, d = 0.5),
      tolerance = 1e-12
    )
  }
  expect_equal(
    power_clustered_at_n(n1 = 60, n2 = 90, d = 0.4, icc = 0, cluster_size = 1),
    power_two_means_at_n(n1 = 60, n2 = 90, d = 0.4),
    tolerance = 1e-12
  )
  expect_equal(power_clustered_n(d = 0.5, icc = 0, cluster_size = 1)$n1,
                power_two_means_n(d = 0.5)$n1)
})

test_that("clustered: power is the noncentral t on CLUSTER-level df", {
  # Recomputed here from the definition, independently of the
  # implementation: with k clusters of size m per arm, a test on cluster
  # means has d_cluster = d * sqrt(m / DE) and df = k1 + k2 - 2.
  m <- 8; icc <- 0.15; k <- 17; d <- 0.5; alpha <- 0.05
  de <- 1 + (m - 1) * icc
  d_cluster <- d * sqrt(m / de)
  ncp <- d_cluster * sqrt(k / 2)
  df <- 2 * k - 2
  qu <- qt(alpha / 2, df, lower.tail = FALSE)
  expected <- pt(qu, df, ncp = ncp, lower.tail = FALSE) +
    pt(-qu, df, ncp = ncp, lower.tail = TRUE)

  expect_equal(
    power_clustered_at_n(n1 = k * m, n2 = k * m, d = d, icc = icc, cluster_size = m),
    expected,
    tolerance = 1e-10
  )
})

test_that("clustered: cluster-level df is never more optimistic than the deflated-n df", {
  # The defect this replaced: taking df from n/DE claims more degrees of
  # freedom than the number of clusters supports, and always in the
  # optimistic direction. The gap must be non-negative everywhere and
  # material in the few-large-clusters regime the app's audience works in.
  grid <- list(c(4, 0.30, 31), c(8, 0.15, 17), c(20, 0.05, 7), c(30, 0.02, 4))
  for (g in grid) {
    m <- g[1]; icc <- g[2]; k <- g[3]
    de <- 1 + (m - 1) * icc
    honest <- power_clustered_at_n(n1 = k * m, n2 = k * m, d = 0.5,
                                    icc = icc, cluster_size = m)
    optimistic <- power_two_means_at_n(n1 = k * m / de, n2 = k * m / de, d = 0.5)
    expect_lte(honest, optimistic)
  }
  # At 4 clusters per arm the two differ by more than a tenth of the scale,
  # which is why this was worth fixing rather than documenting.
  gap <- power_two_means_at_n(n1 = 4 * 30 / 1.58, n2 = 4 * 30 / 1.58, d = 0.5) -
    power_clustered_at_n(n1 = 4 * 30, n2 = 4 * 30, d = 0.5, icc = 0.02, cluster_size = 30)
  expect_gt(gap, 0.10)
})

test_that("clustered: the solver returns the SMALLEST cluster count meeting the target", {
  for (g in list(c(4, 0.30), c(8, 0.15), c(20, 0.05), c(30, 0.02))) {
    m <- g[1]; icc <- g[2]
    res <- power_clustered_n(d = 0.5, icc = icc, cluster_size = m, power = 0.80)
    expect_gte(res$power_achieved, 0.80)
    # One fewer cluster per arm must fall short, or the answer wasn't minimal.
    expect_lt(
      power_clustered_at_n(n1 = (res$k1 - 1) * m, n2 = (res$k2 - 1) * m,
                            d = 0.5, icc = icc, cluster_size = m),
      0.80
    )
  }
})

test_that("clustered: reported power_achieved is the power at the reported N", {
  # The invariant test-properties.R now asserts across every family; kept
  # here too because this family solves in clusters, not participants.
  for (g in list(c(5, 0.10, 1), c(12, 0.05, 2), c(20, 0.02, 0.5))) {
    res <- power_clustered_n(d = 0.5, icc = g[2], cluster_size = g[1],
                              power = 0.80, allocation_ratio = g[3])
    expect_equal(
      res$power_achieved,
      power_clustered_at_n(n1 = res$n1, n2 = res$n2, d = 0.5, icc = g[2],
                            cluster_size = g[1]),
      tolerance = 1e-10
    )
    expect_equal(res$n1, res$k1 * g[1])
    expect_equal(res$n2, res$k2 * g[1])
  }
})

test_that("clustered: higher ICC never decreases the required total N", {
  n_low_icc  <- power_clustered_n(d = 0.5, icc = 0.01, cluster_size = 20, power = 0.80)$n_total
  n_high_icc <- power_clustered_n(d = 0.5, icc = 0.20, cluster_size = 20, power = 0.80)$n_total
  expect_gte(n_high_icc, n_low_icc)
})

test_that("clustered: larger clusters (fixed ICC > 0) never decrease the required total N", {
  n_small_cluster <- power_clustered_n(d = 0.5, icc = 0.05, cluster_size = 5, power = 0.80)$n_total
  n_large_cluster <- power_clustered_n(d = 0.5, icc = 0.05, cluster_size = 50, power = 0.80)$n_total
  expect_gte(n_large_cluster, n_small_cluster)
})

test_that("clustered: design effect formula matches 1 + (m-1)*ICC", {
  expect_equal(cluster_design_effect(cluster_size = 20, icc = 0.05), 1 + 19 * 0.05)
  expect_equal(cluster_design_effect(cluster_size = 1, icc = 0.3), 1)
})

test_that("power_clustered_at_n and power_clustered_n are mutually consistent", {
  res <- power_clustered_n(d = 0.5, icc = 0.05, cluster_size = 20, power = 0.80)
  p_at_n <- power_clustered_at_n(n1 = res$n1, n2 = res$n2, d = 0.5, icc = 0.05, cluster_size = 20)
  expect_equal(p_at_n, res$power_achieved, tolerance = 1e-8)
})

test_that("power_clustered_min_d is the inverse of power_clustered_n", {
  res <- power_clustered_n(d = 0.5, icc = 0.05, cluster_size = 20, power = 0.80)
  min_d <- power_clustered_min_d(n1 = res$n1, n2 = res$n2, icc = 0.05, cluster_size = 20, power = 0.80)
  # Looser tolerance than the single-rounding families (two_means, paired_t):
  # n1/n2 here went through TWO rounding-up steps (naive individual N, then
  # up to a whole number of clusters), so the recovered d is further below
  # the original 0.5 than a single round-up would produce.
  expect_equal(min_d, 0.5, tolerance = 0.15)
  # ... but it must be an exact inverse of the function actually reported:
  # feeding the recovered d back in has to return the target power.
  expect_equal(
    power_clustered_at_n(n1 = res$n1, n2 = res$n2, d = min_d, icc = 0.05, cluster_size = 20),
    0.80,
    tolerance = 1e-6
  )
})

test_that("power_clustered_min_d declines to answer a design that cannot reach the target", {
  # df = k1 + k2 - 2 = 2 clusters per arm is the floor; a design with too
  # few clusters to reach 80% at any effect size must return NA rather than
  # a number the sensitivity panel would display as if it were meaningful.
  expect_true(is.na(power_clustered_min_d(n1 = 20, n2 = 20, icc = 0.5,
                                           cluster_size = 20, power = 0.80)))
})
