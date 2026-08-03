## power_anova_factorial.R computes power via the standard closed-form
## noncentral-F relationship (Cohen, 1988): lambda = N_total * f^2, with
## df1/df2 from the design. This is exactly the same relationship
## pwr::pwr.anova.test() uses for one-way designs, so the one-way case is
## cross-checked directly against that independent, widely-used
## implementation rather than against a remembered textbook constant.

test_that("one-way ANOVA matches pwr::pwr.anova.test exactly (independent cross-check)", {
  skip_if_not_installed("pwr")
  ref1 <- pwr::pwr.anova.test(k = 3, n = 15, f = 0.25, sig.level = 0.05)$power
  ours1 <- power_anova_factorial_at_n(15, levels_a = 3, levels_b = NULL, focal = "A", f_target = 0.25)
  expect_equal(ours1, ref1, tolerance = 1e-8)

  ref2 <- pwr::pwr.anova.test(k = 4, n = 10, f = 0.40, sig.level = 0.05)$power
  ours2 <- power_anova_factorial_at_n(10, levels_a = 4, levels_b = NULL, focal = "A", f_target = 0.40)
  expect_equal(ours2, ref2, tolerance = 1e-8)
})

test_that("one-way ANOVA: matches pwr::pwr.anova.test's own sample-size solution", {
  skip_if_not_installed("pwr")
  # Cohen (1988) / G*Power reference example: k = 4 groups, f = 0.25
  # (medium), alpha = .05, power = .80 -> n = 44.6 per group -> 45.
  ref_n <- ceiling(pwr::pwr.anova.test(k = 4, f = 0.25, sig.level = 0.05, power = 0.80)$n)
  ours <- power_anova_factorial_n(levels_a = 4, levels_b = NULL, focal = "A", f_target = 0.25, power = 0.80)
  expect_equal(ours$n_per_cell, ref_n)
  expect_gte(ours$power_achieved, 0.80)
})

test_that("factorial ANOVA: power increases with n_per_cell", {
  p_low <- power_anova_factorial_at_n(10, levels_a = 2, levels_b = 2, focal = "A", f_target = 0.25)
  p_high <- power_anova_factorial_at_n(100, levels_a = 2, levels_b = 2, focal = "A", f_target = 0.25)
  expect_gt(p_high, p_low)
})

test_that("factorial ANOVA: detecting an interaction needs a larger N than a main effect with fewer degrees of freedom", {
  # A 2x2 design gives A (df=1) and A:B (df=1) the SAME df, so at equal
  # Cohen's f they require the IDENTICAL sample size -- that's correct
  # ANOVA behavior, not something this test should assert against. A 2x3
  # design gives the interaction 2 df against the main effect's 1 df,
  # which is where the (real, but df-driven, not automatically 4x)
  # inflation shows up.
  n_main <- power_anova_factorial_n(2, 3, focal = "A", f_target = 0.25, power = 0.80)$n_total
  n_interaction <- power_anova_factorial_n(2, 3, focal = "A:B", f_target = 0.25, power = 0.80)$n_total
  expect_gt(n_interaction, n_main)
})

test_that("factorial ANOVA: main effect and interaction of equal df need equal N (2x2 case)", {
  n_main <- power_anova_factorial_n(2, 2, focal = "A", f_target = 0.25, power = 0.80)$n_total
  n_interaction <- power_anova_factorial_n(2, 2, focal = "A:B", f_target = 0.25, power = 0.80)$n_total
  expect_equal(n_interaction, n_main)
})

test_that("one-way ANOVA (levels_b = NULL) runs without error", {
  res <- power_anova_factorial_n(levels_a = 3, levels_b = NULL, f_target = 0.25, power = 0.80)
  expect_true(res$n_total > 0)
})

test_that("power_anova_factorial_at_n and power_anova_factorial_n are mutually consistent", {
  res <- power_anova_factorial_n(2, 2, focal = "A", f_target = 0.25, power = 0.80)
  p_at_n <- power_anova_factorial_at_n(res$n_per_cell, 2, 2, focal = "A", f_target = 0.25)
  expect_equal(p_at_n, res$power_achieved, tolerance = 1e-8)
})

test_that("power_anova_factorial_min_f is the inverse of power_anova_factorial_n", {
  res <- power_anova_factorial_n(2, 2, focal = "A", f_target = 0.25, power = 0.80)
  min_f <- power_anova_factorial_min_f(res$n_per_cell, 2, 2, focal = "A", power = 0.80)
  expect_equal(min_f, 0.25, tolerance = 0.01)
})

test_that("degrees of freedom follow the standard factorial-ANOVA rule", {
  expect_equal(.anova_df1(2, 2, "A"), 1)
  expect_equal(.anova_df1(2, 2, "B"), 1)
  expect_equal(.anova_df1(2, 2, "A:B"), 1)
  expect_equal(.anova_df1(3, 4, "A"), 2)
  expect_equal(.anova_df1(3, 4, "B"), 3)
  expect_equal(.anova_df1(3, 4, "A:B"), 6)
  expect_equal(.anova_df1(5, NULL, "A"), 4)
})
