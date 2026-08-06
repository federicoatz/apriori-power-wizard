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

test_that("proportions: agrees with pwr::pwr.2p.test across a parameter grid", {
  skip_if_not_installed("pwr")
  for (h in c(0.15, 0.20, 0.35, 0.50, 0.80)) {
    for (alpha in c(0.01, 0.05)) {
      for (pw in c(0.80, 0.90, 0.95)) {
        ours <- power_proportions_n(h = h, sig_level = alpha, power = pw)$n1
        ref <- ceiling(pwr::pwr.2p.test(h = h, sig.level = alpha, power = pw)$n)
        expect_equal(ours, ref,
                     info = sprintf("h=%.2f alpha=%.2f power=%.2f", h, alpha, pw))
      }
    }
  }
})

test_that("proportions: unbalanced design agrees with pwr::pwr.2p2n.test's achieved power", {
  skip_if_not_installed("pwr")
  res <- power_proportions_n(h = 0.4, sig_level = 0.05, power = 0.80,
                              allocation_ratio = 2)
  # the achieved power our solver reports at (n1, n2) must match what pwr
  # computes for those same two group sizes
  ref_power <- pwr::pwr.2p2n.test(h = 0.4, n1 = res$n1, n2 = res$n2,
                                   sig.level = 0.05)$power
  expect_equal(res$power_achieved, ref_power, tolerance = 1e-6)
  expect_gte(res$power_achieved, 0.80)
})

test_that("proportions: power increases with n", {
  p_low <- power_proportions_at_n(n1 = 20, h = 0.4)
  p_high <- power_proportions_at_n(n1 = 200, h = 0.4)
  expect_gt(p_high, p_low)
})
