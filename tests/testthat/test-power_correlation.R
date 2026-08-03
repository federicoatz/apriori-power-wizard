test_that("correlation: matches pwr::pwr.r.test reference (r=0.3)", {
  skip_if_not_installed("pwr")
  # Cohen (1988), ch. 3 medium effect r=0.30, alpha=.05, power=.80 ->
  # n = 84.07 -> 85 after rounding up (matches pwr::pwr.r.test()).
  res <- power_correlation_n(r = 0.3, sig_level = 0.05, power = 0.80)
  expect_equal(res$n_total, 85)
  expect_gte(res$power_achieved, 0.80)
})

test_that("correlation: power increases with n", {
  p_low <- power_correlation_at_n(n_total = 20, r = 0.3)
  p_high <- power_correlation_at_n(n_total = 200, r = 0.3)
  expect_gt(p_high, p_low)
})

test_that("correlation: power_correlation_min_r is the inverse of power_correlation_n", {
  res <- power_correlation_n(r = 0.25, sig_level = 0.05, power = 0.80)
  r_back <- power_correlation_min_r(n_total = res$n_total, sig_level = 0.05, power = 0.80)
  expect_lt(r_back, 0.25 + 0.01)
})
