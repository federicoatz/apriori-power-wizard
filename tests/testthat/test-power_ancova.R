## power_ancova.R wraps pwr::pwr.f2.test() with a covariate-adjusted f2, so
## every achieved-power case can be cross-checked EXACTLY (not just
## approximately) against a direct pwr::pwr.f2.test() call using the same
## u/v/f2 -- and, in the r_cov = 0 limit, against power_anova_factorial_at_n()
## with one extra df spent on the (uninformative) covariate.

test_that("ancova matches pwr::pwr.f2.test exactly for the same u/v/f2 (independent cross-check)", {
  skip_if_not_installed("pwr")
  k <- 3; n_per_group <- 20; f <- 0.25; r_cov <- 0.4
  f2_adj <- (f^2) / (1 - r_cov^2)
  v <- n_per_group * k - k - 1
  ref <- pwr::pwr.f2.test(u = k - 1, v = v, f2 = f2_adj, sig.level = 0.05)$power
  ours <- power_ancova_at_n(n_per_group, k, f, r_cov, sig_level = 0.05)
  expect_equal(ours, ref, tolerance = 1e-8)
})

test_that("ancova: an uninformative covariate (r_cov = 0) costs power relative to plain ANOVA at the same N", {
  k <- 3; n_per_group <- 20; f <- 0.25
  p_ancova <- power_ancova_at_n(n_per_group, k, f, r_cov = 0, sig_level = 0.05)
  p_anova  <- power_anova_factorial_at_n(n_per_group, levels_a = k, levels_b = NULL,
                                          focal = "A", f_target = f, sig_level = 0.05)
  expect_lt(p_ancova, p_anova)
  expect_equal(p_ancova, p_anova, tolerance = 0.02)
})

test_that("ancova: a covariate correlated with the outcome increases power at the same N", {
  k <- 2; n_per_group <- 30; f <- 0.25
  p_no_cov  <- power_ancova_at_n(n_per_group, k, f, r_cov = 0, sig_level = 0.05)
  p_cov     <- power_ancova_at_n(n_per_group, k, f, r_cov = 0.5, sig_level = 0.05)
  expect_gt(p_cov, p_no_cov)
})

test_that("ancova: power increases with n_per_group", {
  p_low  <- power_ancova_at_n(10, k = 3, f_target = 0.25, r_cov = 0.3)
  p_high <- power_ancova_at_n(100, k = 3, f_target = 0.25, r_cov = 0.3)
  expect_gt(p_high, p_low)
})

test_that("power_ancova_n reaches at least the target power, and power_ancova_at_n agrees at that N", {
  res <- power_ancova_n(k = 3, f_target = 0.25, r_cov = 0.3, power = 0.80)
  expect_gte(res$power_achieved, 0.80)
  p_at_n <- power_ancova_at_n(res$n_per_group, k = 3, f_target = 0.25, r_cov = 0.3)
  expect_equal(p_at_n, res$power_achieved, tolerance = 1e-8)
})

test_that("power_ancova_min_f is the inverse of power_ancova_n", {
  res <- power_ancova_n(k = 3, f_target = 0.25, r_cov = 0.3, power = 0.80)
  min_f <- power_ancova_min_f(res$n_per_group, k = 3, r_cov = 0.3, power = 0.80)
  expect_equal(min_f, 0.25, tolerance = 0.01)
})
