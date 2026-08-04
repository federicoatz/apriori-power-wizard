## power_clustered_cat.R inflates an already-validated unclustered sample
## size by the design effect. That structure makes it checkable without any
## simulation: with ICC = 0 (or cluster size 1) the design effect is exactly
## 1, so the clustered result must collapse onto the unclustered family it
## wraps -- and with ICC > 0 it must exceed it by exactly the design effect,
## up to the cluster-rounding step.

test_that("clustered binary: ICC = 0 collapses onto the plain two-proportions family", {
  h <- 0.4
  plain <- power_proportions_n(h = h, power = 0.80)
  # cluster_size 1 also forces DE = 1, independently of the ICC.
  cl <- power_clustered_binary_n(h = h, icc = 0, cluster_size = 1, power = 0.80)
  expect_equal(cl$design_effect, 1)
  expect_equal(cl$n1, plain$n1)
  expect_equal(cl$n2, plain$n2)
})

test_that("clustered categorical: ICC = 0 collapses onto the plain chi-square family", {
  plain <- power_chisq_n(w = 0.3, df = 3, power = 0.80)
  cl <- power_clustered_cat_n(w = 0.3, df = 3, icc = 0, cluster_size = 1, power = 0.80)
  expect_equal(cl$design_effect, 1)
  expect_equal(cl$n_total, plain$n_total)
})

test_that("clustered binary: N is inflated by the design effect (up to cluster rounding)", {
  h <- 0.4; m <- 10; icc <- 0.05
  de <- 1 + (m - 1) * icc                    # = 1.45
  plain <- power_proportions_n(h = h, power = 0.80)
  cl <- power_clustered_binary_n(h = h, icc = icc, cluster_size = m, power = 0.80)
  expect_equal(cl$design_effect, de)
  # n1 is the naive n1 * DE rounded UP to a whole cluster, so it lands in
  # [naive*DE, naive*DE + m).
  expect_gte(cl$n1, plain$n1 * de)
  expect_lt(cl$n1, plain$n1 * de + m)
  expect_equal(cl$n1, cl$k1 * m)
})

test_that("clustered categorical: N is inflated by the design effect (up to cluster rounding)", {
  w <- 0.3; df <- 3; m <- 8; icc <- 0.10
  de <- 1 + (m - 1) * icc                    # = 1.70
  plain <- power_chisq_n(w = w, df = df, power = 0.80)
  cl <- power_clustered_cat_n(w = w, df = df, icc = icc, cluster_size = m, power = 0.80)
  expect_equal(cl$design_effect, de)
  expect_gte(cl$n_total, plain$n_total * de)
  expect_lt(cl$n_total, plain$n_total * de + m)
  expect_equal(cl$n_total, cl$n_clusters_total * m)
})

test_that("clustered binary/categorical: both reach at least the target power", {
  expect_gte(power_clustered_binary_n(0.4, icc = 0.05, cluster_size = 10, power = 0.80)$power_achieved, 0.80)
  expect_gte(power_clustered_cat_n(0.3, df = 3, icc = 0.05, cluster_size = 10, power = 0.80)$power_achieved, 0.80)
  expect_gte(power_clustered_binary_n(0.3, icc = 0.20, cluster_size = 4, power = 0.90)$power_achieved, 0.90)
})

test_that("clustered: a larger ICC or cluster size always requires more participants", {
  base <- power_clustered_binary_n(0.4, icc = 0.02, cluster_size = 10, power = 0.80)$n_total
  hi_icc <- power_clustered_binary_n(0.4, icc = 0.20, cluster_size = 10, power = 0.80)$n_total
  hi_m <- power_clustered_binary_n(0.4, icc = 0.02, cluster_size = 40, power = 0.80)$n_total
  expect_gt(hi_icc, base)
  expect_gt(hi_m, base)

  b2 <- power_clustered_cat_n(0.3, df = 3, icc = 0.02, cluster_size = 10, power = 0.80)$n_total
  expect_gt(power_clustered_cat_n(0.3, df = 3, icc = 0.20, cluster_size = 10, power = 0.80)$n_total, b2)
})

test_that("clustered: power increases with N, for both outcome types", {
  expect_gt(power_clustered_binary_at_n(600, h = 0.4, icc = 0.05, cluster_size = 10),
            power_clustered_binary_at_n(100, h = 0.4, icc = 0.05, cluster_size = 10))
  expect_gt(power_clustered_cat_at_n(900, w = 0.3, df = 3, icc = 0.05, cluster_size = 10),
            power_clustered_cat_at_n(150, w = 0.3, df = 3, icc = 0.05, cluster_size = 10))
})

test_that("clustered: the min-effect inverses round-trip against the solvers", {
  res_b <- power_clustered_binary_n(0.4, icc = 0.05, cluster_size = 10, power = 0.80)
  expect_equal(power_clustered_binary_min_h(res_b$n1, res_b$n2, icc = 0.05,
                                             cluster_size = 10, power = 0.80),
               0.4, tolerance = 0.03)
  res_c <- power_clustered_cat_n(0.3, df = 3, icc = 0.05, cluster_size = 10, power = 0.80)
  expect_equal(power_clustered_cat_min_w(res_c$n_total, df = 3, icc = 0.05,
                                          cluster_size = 10, power = 0.80),
               0.3, tolerance = 0.03)
})

test_that("clustered: ill-posed inputs are rejected", {
  expect_error(power_clustered_binary_n(0, icc = 0.05, cluster_size = 10))
  expect_error(power_clustered_binary_n(0.4, icc = 1, cluster_size = 10))
  expect_error(power_clustered_cat_n(0.3, df = 0, icc = 0.05, cluster_size = 10))
})
