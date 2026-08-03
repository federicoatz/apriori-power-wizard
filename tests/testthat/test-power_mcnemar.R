test_that("mcnemar: matches direct Monte Carlo simulation within noise", {
  # Cross-checked during development against a direct simulation of the
  # McNemar chi-square statistic (8000-20000 sims); these three pinned
  # values reproduce that same cross-check as a regression test.
  expect_equal(power_mcnemar_at_n(100, p10 = 0.25, p01 = 0.10), 0.7241, tolerance = 0.001)
  expect_equal(power_mcnemar_at_n(200, p10 = 0.30, p01 = 0.15), 0.8913, tolerance = 0.001)
  expect_equal(power_mcnemar_at_n(60, p10 = 0.35, p01 = 0.10), 0.8410, tolerance = 0.001)
})

test_that("mcnemar: power_mcnemar_n solves for the N that achieves target power", {
  res <- power_mcnemar_n(p10 = 0.25, p01 = 0.10, power = 0.80)
  expect_gte(res$power_achieved, 0.80)
  # One fewer pair should no longer reach the target.
  expect_lt(power_mcnemar_at_n(res$n_total - 1, p10 = 0.25, p01 = 0.10), 0.80)
})

test_that("mcnemar: power increases with n", {
  p_low <- power_mcnemar_at_n(30, p10 = 0.25, p01 = 0.10)
  p_high <- power_mcnemar_at_n(300, p10 = 0.25, p01 = 0.10)
  expect_gt(p_high, p_low)
})

test_that("mcnemar: symmetric proportions (delta = 0) is rejected as ill-posed", {
  expect_error(power_mcnemar_n(p10 = 0.2, p01 = 0.2, power = 0.80))
})

test_that("mcnemar: power_mcnemar_min_delta is the approximate inverse of power_mcnemar_n", {
  res <- power_mcnemar_n(p10 = 0.28, p01 = 0.12, power = 0.80)
  delta_back <- power_mcnemar_min_delta(res$n_total, p_discordant = res$p_discordant, power = 0.80)
  expect_lt(delta_back, res$delta + 0.01)
})
