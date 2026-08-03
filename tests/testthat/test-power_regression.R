test_that("regression: matches textbook pwr.f2.test reference (u=3, f2=0.15)", {
  skip_if_not_installed("pwr")
  # Cohen (1988), ch. 9 / G*Power reference example: u = 3, f2 = 0.15
  # (medium), alpha = .05, power = .80 -> v = 72.71 -> N = v + u + 1 =
  # 76.71, rounded up to 77. (An earlier version of this test claimed
  # N = 118 for the same inputs -- that number was wrong: running
  # pwr::pwr.f2.test(u=3, f2=0.15, sig.level=0.05, power=0.80) here gives
  # v = 72.70583, not ~113.6, and this app's power_regression_n() is a
  # direct, unmodified wrapper around it (n = v + u + 1), so the earlier
  # expected value was a transcription/recall error in the test itself,
  # not a bug in power_regression.R.)
  res <- power_regression_n(f2 = 0.15, n_predictors_tested = 3, sig_level = 0.05, power = 0.80)
  expect_equal(res$n_total, 77)
})

test_that("r2_to_f2 matches the standard Cohen formula", {
  expect_equal(r2_to_f2(0.30, 0.10), (0.30 - 0.10) / (1 - 0.30))
  expect_equal(r2_to_f2(0.13, 0), 0.13 / 0.87, tolerance = 1e-10)
})

test_that("regression: power_regression_at_n / power_regression_n are consistent", {
  res <- power_regression_n(f2 = 0.15, n_predictors_tested = 1, power = 0.80)
  p_at_n <- power_regression_at_n(res$n_total, f2 = 0.15, n_predictors_tested = 1)
  expect_gte(p_at_n, 0.80 - 0.01)
})

test_that("regression: min f2 for a fixed N is the inverse of power_regression_n", {
  res <- power_regression_n(f2 = 0.15, n_predictors_tested = 1, power = 0.80)
  min_f2 <- power_regression_min_f2(res$n_total, n_predictors_tested = 1, power = 0.80)
  expect_equal(min_f2, 0.15, tolerance = 0.01)
})
