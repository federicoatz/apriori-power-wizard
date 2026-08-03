## power_wilcoxon.R implements Noether's (1987) closed form for the
## Wilcoxon-Mann-Whitney rank-sum test. Cross-checked during development
## against direct Monte Carlo simulation of stats::wilcox.test() (4,000-6,000
## replications per configuration, normal parents); the pinned values below
## reproduce that cross-check as a regression test. A wider tolerance than
## the exact-closed-form families use is deliberate: Noether's formula is a
## large-sample normal approximation, and the simulation showed it to be
## mildly conservative rather than exact.

test_that("wilcoxon: matches Monte Carlo simulation within noise", {
  # Simulated power was 0.8068, 0.8125, 0.8357 respectively.
  expect_equal(power_wilcoxon_at_n(69, d_to_p_superiority(0.5)), 0.8068, tolerance = 0.02)
  expect_equal(power_wilcoxon_at_n(186, d_to_p_superiority(0.3)), 0.8125, tolerance = 0.02)
  expect_equal(power_wilcoxon_at_n(29, d_to_p_superiority(0.8)), 0.8357, tolerance = 0.05)
})

test_that("wilcoxon: the balanced case reduces to Noether's n = (za+zb)^2 / (6 (p-.5)^2)", {
  p <- d_to_p_superiority(0.5)
  expected <- (qnorm(0.975) + qnorm(0.80))^2 / (6 * (p - 0.5)^2)
  res <- power_wilcoxon_n(p, sig_level = 0.05, power = 0.80)
  # n1 is the ceiling of the closed form (possibly +1 to clear the target).
  expect_gte(res$n1, ceiling(expected))
  expect_lte(res$n1, ceiling(expected) + 1)
})

test_that("wilcoxon: power_wilcoxon_n reaches at least the target power, one fewer does not", {
  p <- d_to_p_superiority(0.5)
  res <- power_wilcoxon_n(p, power = 0.80)
  expect_gte(res$power_achieved, 0.80)
  expect_lt(power_wilcoxon_at_n(res$n1 - 1, p), 0.80)
})

test_that("wilcoxon: power increases with n and with distance of p from 0.5", {
  p <- d_to_p_superiority(0.5)
  expect_gt(power_wilcoxon_at_n(200, p), power_wilcoxon_at_n(20, p))
  expect_gt(power_wilcoxon_at_n(50, 0.75), power_wilcoxon_at_n(50, 0.60))
})

test_that("wilcoxon: p = 0.5 (no effect at all) is rejected as ill-posed", {
  expect_error(power_wilcoxon_n(0.5, power = 0.80))
  expect_true(is.na(power_wilcoxon_at_n(50, 0.5)))
})

test_that("wilcoxon: requires a larger N than the equivalent t-test under normality (the ARE cost)", {
  skip_if_not_installed("pwr")
  d <- 0.5
  n_t <- ceiling(pwr::pwr.t.test(d = d, sig.level = 0.05, power = 0.80)$n)
  n_w <- power_wilcoxon_n(d_to_p_superiority(d), sig_level = 0.05, power = 0.80)$n1
  expect_gt(n_w, n_t)
  # The asymptotic relative efficiency of WMW vs t under normality is
  # 3/pi ~= 0.955, so the penalty should be only a few percent, not a
  # different order of magnitude.
  expect_lt(n_w / n_t, 1.15)
})

test_that("d_to_p_superiority and p_superiority_to_d are inverses, with d = 0 -> p = 0.5", {
  expect_equal(d_to_p_superiority(0), 0.5)
  expect_equal(p_superiority_to_d(d_to_p_superiority(0.42)), 0.42, tolerance = 1e-10)
  expect_gt(d_to_p_superiority(0.8), d_to_p_superiority(0.2))
})

test_that("power_wilcoxon_min_p is the inverse of power_wilcoxon_n", {
  p <- d_to_p_superiority(0.5)
  res <- power_wilcoxon_n(p, power = 0.80)
  expect_equal(power_wilcoxon_min_p(res$n1, power = 0.80), p, tolerance = 0.01)
})

test_that("wilcoxon: unequal allocation requires more total N than balanced", {
  p <- d_to_p_superiority(0.5)
  n_bal <- power_wilcoxon_n(p, allocation_ratio = 1, power = 0.80)$n_total
  n_unb <- power_wilcoxon_n(p, allocation_ratio = 3, power = 0.80)$n_total
  expect_gt(n_unb, n_bal)
})
