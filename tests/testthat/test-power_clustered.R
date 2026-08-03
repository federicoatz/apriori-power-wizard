## No externally pinned numeric reference is used for this family: the
## design-effect method (Donner & Klar, 2000; Hayes & Moulton, 2017) is
## standard and well-documented, but this file was written without a
## local R session or a second package (e.g. `clusterPower`) available to
## cross-check an exact worked value. Instead these tests pin down the
## STRUCTURAL properties the formula must satisfy -- which are strong
## enough to catch sign errors, swapped arguments, or a broken inflation
## step -- and reduce the clustered case to the already-pinned
## power_two_means_n() reference (test-power_two_means.R) whenever
## ICC = 0, where clustering has no effect by construction. Please verify
## against `clusterPower::cps.nct()` or a G*Power/Optimal Design
## comparison locally before relying on this for a real study.

test_that("clustered: ICC = 0 reduces exactly to the individually-randomized case (up to cluster rounding)", {
  skip_if_not_installed("pwr")
  naive <- power_two_means_n(d = 0.5, sig_level = 0.05, power = 0.80)
  res <- power_clustered_n(d = 0.5, icc = 0, cluster_size = 20,
                            sig_level = 0.05, power = 0.80)
  expect_equal(res$design_effect, 1)
  # With no clustering effect, the per-arm N should just be the naive N
  # rounded UP to a whole number of clusters (never below it).
  expect_gte(res$n1, naive$n1)
  expect_lt(res$n1 - naive$n1, 20)  # rounding never adds more than one cluster
  expect_gte(res$power_achieved, 0.80)
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
  expect_equal(min_d, 0.5, tolerance = 0.06)
})
