test_that("safeguard_ci_d lower bound is below the published estimate", {
  sg <- safeguard_ci_d(d_published = 0.6, n1 = 30, n2 = 30, conf_level = 0.80, one_sided = TRUE)
  expect_lt(sg$lower, 0.6)
  expect_equal(sg$d_safeguard, sg$lower)
})

test_that("safeguard_ci_d uses qnorm(0.80) for the default one-sided level", {
  sg <- safeguard_ci_d(d_published = 0.6, n1 = 30, n2 = 30, conf_level = 0.80, one_sided = TRUE)
  se <- sqrt(60 / (30 * 30) + 0.6^2 / (2 * 60))
  expect_equal(sg$se, se, tolerance = 1e-10)
  expect_equal(sg$lower, 0.6 - qnorm(0.80) * se, tolerance = 1e-10)
})

test_that("safeguard correction increases required N relative to the naive published estimate", {
  d_published <- 0.6
  n_pub <- 40
  sg <- safeguard_ci_d(d_published, n1 = n_pub / 2, n2 = n_pub / 2, conf_level = 0.80)
  n_naive <- power_two_means_n(d_published, power = 0.80)$n_total
  n_safeguard <- power_two_means_n(sg$d_safeguard, power = 0.80)$n_total
  expect_gt(n_safeguard, n_naive)
})

test_that("safeguard_ci_d with a larger original study N yields a tighter (higher) lower bound", {
  sg_small_study <- safeguard_ci_d(0.6, n1 = 10, n2 = 10)
  sg_large_study <- safeguard_ci_d(0.6, n1 = 500, n2 = 500)
  expect_gt(sg_large_study$lower, sg_small_study$lower)
})

test_that("safeguard_ci_r (Fisher z) recovers a known transformation", {
  r <- 0.3
  n <- 100
  sg <- safeguard_ci_r(r, n, conf_level = 0.80, one_sided = TRUE)
  z <- atanh(r)
  se <- 1 / sqrt(n - 3)
  expect_equal(sg$lower, tanh(z - qnorm(0.80) * se), tolerance = 1e-10)
})

test_that("safeguard_ci_w lower bound is below the published estimate", {
  sg <- safeguard_ci_w(w_published = 0.3, n_published = 300, df = 3, conf_level = 0.80)
  expect_lt(sg$lower, 0.3)
  expect_equal(sg$w_safeguard, sg$lower)
})

test_that("safeguard_ci_w with a larger original study N yields a tighter (higher) lower bound", {
  sg_small_study <- safeguard_ci_w(0.3, n_published = 60, df = 3)
  sg_large_study <- safeguard_ci_w(0.3, n_published = 2000, df = 3)
  expect_gt(sg_large_study$lower, sg_small_study$lower)
})

test_that("safeguard correction increases required N for chi-square relative to the naive published estimate", {
  w_published <- 0.3
  n_pub <- 100
  sg <- safeguard_ci_w(w_published, n_published = n_pub, df = 3, conf_level = 0.80)
  n_naive <- power_chisq_n(w_published, df = 3, power = 0.80)$n_total
  n_safeguard <- power_chisq_n(sg$w_safeguard, df = 3, power = 0.80)$n_total
  expect_gt(n_safeguard, n_naive)
})

test_that("d_to_or / or_to_d round-trip and match Chinn (2000) at Cohen's medium d", {
  d_medium <- 0.5
  or <- d_to_or(d_medium)
  expect_equal(or, exp(d_medium * pi / sqrt(3)), tolerance = 1e-10)
  expect_equal(or_to_d(or), d_medium, tolerance = 1e-8)
})
