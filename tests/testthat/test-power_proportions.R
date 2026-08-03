test_that("proportions: matches textbook pwr.2p.test reference (h=0.5)", {
  skip_if_not_installed("pwr")
  # Cohen (1988), ch. 6 reference: h = 0.5, alpha = .05, power = .80 ->
  # n = 62.79 per group -> 63 after rounding up (matches pwr::pwr.2p.test
  # and G*Power's "Proportions: Difference between two independent
  # proportions" for the same z-test parameterization).
  res <- power_proportions_n(h = 0.5, sig_level = 0.05, power = 0.80)
  expect_equal(res$n1, 63)
  expect_equal(res$n_total, 126)
})

test_that("proportions_to_h reproduces pwr::ES.h", {
  skip_if_not_installed("pwr")
  expect_equal(proportions_to_h(0.5, 0.3), pwr::ES.h(0.5, 0.3))
})

test_that("proportions: power increases with n", {
  p_low <- power_proportions_at_n(n1 = 20, h = 0.4)
  p_high <- power_proportions_at_n(n1 = 200, h = 0.4)
  expect_gt(p_high, p_low)
})
