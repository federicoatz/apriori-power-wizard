test_that("round_up_n always rounds up, never to nearest", {
  expect_equal(round_up_n(63.01), 64)
  expect_equal(round_up_n(63.99), 64)
  expect_equal(round_up_n(64.00), 64)
})

test_that("cohen_benchmarks returns Cohen's (1988) canonical values", {
  expect_equal(unname(cohen_benchmarks("d")), c(0.20, 0.50, 0.80))
  expect_equal(unname(cohen_benchmarks("f")), c(0.10, 0.25, 0.40))
  expect_equal(unname(cohen_benchmarks("f2")), c(0.02, 0.15, 0.35))
  expect_equal(unname(cohen_benchmarks("h")), c(0.20, 0.50, 0.80))
  expect_equal(unname(cohen_benchmarks("w")), c(0.10, 0.30, 0.50))
  expect_equal(unname(cohen_benchmarks("r")), c(0.10, 0.30, 0.50))
})

test_that("read_params_step Bonferroni-corrects alpha when n_comparisons > 1", {
  p1 <- read_params_step(list(alpha = 0.05, power = 0.80, tails = "two.sided",
                               balanced = TRUE, n_comparisons = 1))
  expect_equal(p1$alpha, 0.05)
  expect_equal(p1$alpha_nominal, 0.05)
  expect_equal(p1$n_comparisons, 1)

  p4 <- read_params_step(list(alpha = 0.05, power = 0.80, tails = "two.sided",
                               balanced = TRUE, n_comparisons = 4))
  expect_equal(p4$alpha, 0.0125)
  expect_equal(p4$alpha_nominal, 0.05)
  expect_equal(p4$n_comparisons, 4)
})

test_that("read_params_step defaults n_comparisons to 1 when absent", {
  p <- read_params_step(list(alpha = 0.05, power = 0.80, tails = "two.sided", balanced = TRUE))
  expect_equal(p$alpha, 0.05)
  expect_equal(p$n_comparisons, 1)
})

test_that("eta2_to_f / f_to_eta2 are inverses and match Cohen's (1988) formula", {
  eta2 <- 0.06
  f <- eta2_to_f(eta2)
  expect_equal(f, sqrt(eta2 / (1 - eta2)), tolerance = 1e-10)
  expect_equal(f_to_eta2(f), eta2, tolerance = 1e-10)
})

test_that("sesoi_raw_to_d matches the definition d = mean difference / pooled SD", {
  expect_equal(sesoi_raw_to_d(5, 10), 0.5)
})

test_that("sesoi_raw_to_f_twolevel equals d/2 (exact identity for a 2-level factor)", {
  d <- sesoi_raw_to_d(5, 10)
  expect_equal(sesoi_raw_to_f_twolevel(5, 10), d / 2)
})

test_that("sesoi_proportions_to_h matches Cohen's arcsine-difference definition", {
  expect_equal(sesoi_proportions_to_h(0.5, 0.3), 2 * asin(sqrt(0.5)) - 2 * asin(sqrt(0.3)))
})

test_that("apply_allocation_ratio produces the requested ratio after rounding", {
  out <- apply_allocation_ratio(50, ratio = 1.5)
  expect_equal(out$n1, 50)
  expect_equal(out$n2, 75)
})
