test_that("correlation: matches pwr::pwr.r.test reference (r=0.3)", {
  skip_if_not_installed("pwr")
  # Cohen (1988), ch. 3 medium effect r=0.30, alpha=.05, power=.80 ->
  # n = 84.07 -> 85 after rounding up (matches pwr::pwr.r.test()).
  res <- power_correlation_n(r = 0.3, sig_level = 0.05, power = 0.80)
  expect_equal(res$n_total, 85)
  expect_gte(res$power_achieved, 0.80)
})

test_that("correlation: agrees with pwr::pwr.r.test across a parameter grid", {
  skip_if_not_installed("pwr")
  for (r in c(0.10, 0.21, 0.30, 0.44, 0.60)) {
    for (alpha in c(0.01, 0.05)) {
      for (pw in c(0.80, 0.90, 0.95)) {
        ours <- power_correlation_n(r = r, sig_level = alpha, power = pw)$n_total
        ref <- ceiling(pwr::pwr.r.test(r = r, sig.level = alpha, power = pw)$n)
        expect_equal(ours, ref,
                     info = sprintf("r=%.2f alpha=%.2f power=%.2f", r, alpha, pw))
      }
    }
  }
})

test_that("correlation: one-sided test agrees with pwr and needs fewer participants", {
  skip_if_not_installed("pwr")
  two <- power_correlation_n(r = 0.3, sig_level = 0.05, power = 0.80,
                              alternative = "two.sided")$n_total
  one <- power_correlation_n(r = 0.3, sig_level = 0.05, power = 0.80,
                              alternative = "greater")$n_total
  ref <- ceiling(pwr::pwr.r.test(r = 0.3, sig.level = 0.05, power = 0.80,
                                  alternative = "greater")$n)
  expect_equal(one, ref)
  expect_lt(one, two)
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
