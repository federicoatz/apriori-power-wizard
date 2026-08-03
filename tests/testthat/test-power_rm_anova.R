## power_rm_anova.R implements the repeated-measures noncentral-F relation.
## The strongest check here needs no simulation at all: with m = 2 and a
## single group, a within-subjects ANOVA is algebraically the same test as a
## paired t-test on d_z = d / sqrt(2(1 - rho)), so it can be checked exactly
## against pwr -- an already-validated, independent implementation. The
## pinned values in the first block reproduce a development-time Monte Carlo
## cross-check (4,000 replications per configuration) of the general case.

test_that("rm-anova: m = 2, one group is EXACTLY a paired t-test (independent cross-check)", {
  skip_if_not_installed("pwr")
  n <- 12; f <- 0.40; rho <- 0.7
  d <- 2 * f                          # f = d/2 for a two-level factor
  dz <- d / sqrt(2 * (1 - rho))       # within-subject standardization
  ours <- power_rm_anova_at_n(n, m = 2, f_target = f, rho = rho)
  ref <- pwr::pwr.t.test(n = n, d = dz, type = "paired")$power
  expect_equal(ours, ref, tolerance = 1e-8)

  # A second, differently-shaped configuration.
  n2 <- 25; f2 <- 0.2; rho2 <- 0.4
  dz2 <- (2 * f2) / sqrt(2 * (1 - rho2))
  expect_equal(power_rm_anova_at_n(n2, m = 2, f_target = f2, rho = rho2),
               pwr::pwr.t.test(n = n2, d = dz2, type = "paired")$power,
               tolerance = 1e-8)
})

test_that("rm-anova: within main effect matches Monte Carlo simulation within noise", {
  expect_equal(power_rm_anova_at_n(20, m = 3, f_target = 0.25, rho = 0.5), 0.6415, tolerance = 0.03)
  expect_equal(power_rm_anova_at_n(15, m = 4, f_target = 0.25, rho = 0.6), 0.6985, tolerance = 0.03)
  expect_equal(power_rm_anova_at_n(30, m = 3, f_target = 0.15, rho = 0.5), 0.4083, tolerance = 0.03)
})

test_that("rm-anova: within x between interaction matches Monte Carlo simulation within noise", {
  expect_equal(
    power_rm_anova_at_n(30, m = 3, f_target = 0.25, rho = 0.5,
                        effect = "interaction", n_groups = 2),
    0.8445, tolerance = 0.03)
  expect_equal(
    power_rm_anova_at_n(36, m = 3, f_target = 0.30, rho = 0.5,
                        effect = "interaction", n_groups = 3),
    0.9447, tolerance = 0.03)
})

test_that("rm-anova: a stronger within-subject correlation needs fewer participants", {
  n_low  <- power_rm_anova_n(m = 3, f_target = 0.25, rho = 0.2, power = 0.80)$n_total
  n_high <- power_rm_anova_n(m = 3, f_target = 0.25, rho = 0.7, power = 0.80)$n_total
  expect_lt(n_high, n_low)
})

test_that("rm-anova: a nonsphericity correction below 1 costs power", {
  full <- power_rm_anova_at_n(20, m = 4, f_target = 0.25, rho = 0.5, eps = 1)
  corrected <- power_rm_anova_at_n(20, m = 4, f_target = 0.25, rho = 0.5, eps = 0.6)
  expect_lt(corrected, full)
})

test_that("rm-anova: power increases with N, and the solver clears the target", {
  expect_gt(power_rm_anova_at_n(80, m = 3, f_target = 0.25, rho = 0.5),
            power_rm_anova_at_n(10, m = 3, f_target = 0.25, rho = 0.5))
  res <- power_rm_anova_n(m = 3, f_target = 0.25, rho = 0.5, power = 0.80)
  expect_gte(res$power_achieved, 0.80)
})

test_that("rm-anova: the total N is divisible by the number of groups in a mixed design", {
  res <- power_rm_anova_n(m = 3, f_target = 0.25, rho = 0.5,
                           effect = "interaction", n_groups = 3, power = 0.80)
  expect_equal(res$n_total %% 3, 0)
  expect_equal(res$n_per_group * 3, res$n_total)
})

test_that("rm-anova: an interaction with fewer than 2 groups is rejected", {
  expect_error(power_rm_anova_at_n(30, m = 3, f_target = 0.25,
                                    effect = "interaction", n_groups = 1))
})

test_that("power_rm_anova_min_f is the inverse of power_rm_anova_n", {
  res <- power_rm_anova_n(m = 3, f_target = 0.25, rho = 0.5, power = 0.80)
  expect_equal(power_rm_anova_min_f(res$n_total, m = 3, rho = 0.5, power = 0.80),
               0.25, tolerance = 0.02)
})
