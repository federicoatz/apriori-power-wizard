test_that("tost: matches direct Monte Carlo simulation within noise", {
  # Cross-checked during development against a direct simulation of the
  # two one-sided t-tests (8000-20000 sims); these three pinned values
  # reproduce that same cross-check as a regression test.
  expect_equal(power_tost_at_n(60, delta_eq = 0.5, theta = 0), 0.7106, tolerance = 0.001)
  expect_equal(power_tost_at_n(150, delta_eq = 0.3, theta = 0), 0.6534, tolerance = 0.001)
  expect_equal(power_tost_at_n(80, delta_eq = 0.4, theta = 0.1), 0.5259, tolerance = 0.001)
})

test_that("tost: power_tost_n solves for the N that achieves target power", {
  res <- power_tost_n(delta_eq = 0.4, theta = 0, power = 0.80)
  expect_gte(res$power_achieved, 0.80)
  expect_lt(power_tost_at_n(res$n - 1, delta_eq = 0.4, theta = 0), 0.80)
})

test_that("tost: power increases with n", {
  p_low <- power_tost_at_n(20, delta_eq = 0.4, theta = 0)
  p_high <- power_tost_at_n(300, delta_eq = 0.4, theta = 0)
  expect_gt(p_high, p_low)
})

test_that("tost: assumed true difference at or beyond the margin is rejected as ill-posed", {
  expect_error(power_tost_n(delta_eq = 0.3, theta = 0.3, power = 0.80))
  expect_error(power_tost_n(delta_eq = 0.3, theta = 0.5, power = 0.80))
})

test_that("tost: power_tost_min_delta_eq is the approximate inverse of power_tost_n", {
  res <- power_tost_n(delta_eq = 0.4, theta = 0, power = 0.80)
  delta_back <- power_tost_min_delta_eq(res$n, theta = 0, power = 0.80)
  expect_equal(delta_back, 0.4, tolerance = 0.01)
})
