test_that("two means: matches textbook pwr.t.test reference (d=0.5, standard settings)", {
  skip_if_not_installed("pwr")
  # Cohen (1988) / G*Power reference value: for a two-sample t-test,
  # d = 0.5, alpha = .05 (two-tailed), power = .80 -> n = 63.77 per group,
  # i.e. 64 after rounding up. This is one of the most widely reproduced
  # reference numbers in the power-analysis literature (matches G*Power
  # 3.1's "t tests - Means: Difference between two independent means"
  # output for the same inputs).
  res <- power_two_means_n(d = 0.5, sig_level = 0.05, power = 0.80,
                            alternative = "two.sided")
  expect_equal(res$n1, 64)
  expect_equal(res$n2, 64)
  expect_equal(res$n_total, 128)
  expect_gte(res$power_achieved, 0.80)
})

test_that("two means: large effect requires fewer participants than small effect", {
  n_small <- power_two_means_n(d = 0.2, power = 0.80)$n_total
  n_large <- power_two_means_n(d = 0.8, power = 0.80)$n_total
  expect_gt(n_small, n_large)
})

test_that("two means: one-tailed test requires <= participants than two-tailed", {
  n_two <- power_two_means_n(d = 0.5, alternative = "two.sided")$n_total
  n_one <- power_two_means_n(d = 0.5, alternative = "greater")$n_total
  expect_lte(n_one, n_two)
})

test_that("two means: unequal allocation ratio changes group sizes but keeps target power", {
  res <- power_two_means_n(d = 0.5, allocation_ratio = 2)
  expect_equal(res$n2, round_up_n(res$n1 * 2))
  expect_gte(res$power_achieved, 0.80)
})

test_that("power_two_means_at_n and power_two_means_n are mutually consistent", {
  res <- power_two_means_n(d = 0.5, power = 0.80)
  p_at_n <- power_two_means_at_n(n1 = res$n1, n2 = res$n2, d = 0.5)
  expect_equal(p_at_n, res$power_achieved, tolerance = 1e-8)
})

test_that("power_two_means_min_d is the inverse of power_two_means_n", {
  res <- power_two_means_n(d = 0.5, power = 0.80)
  min_d <- power_two_means_min_d(n1 = res$n1, power = 0.80)
  expect_equal(min_d, 0.5, tolerance = 0.02)
})
