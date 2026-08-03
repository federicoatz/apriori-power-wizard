## NOTE ON THESE TESTS
## ---------------------------------------------------------------------
## R/power_logistic.R has no external dependency (base R only: integrate(),
## uniroot(), pnorm()) -- it is a direct reimplementation of the Demidenko
## (2007) Wald-test power formula, following the same algorithm as
## WebPower::wp.logistic(). The exact-value tests below check against the
## reference numbers published in WebPower's own documentation
## (https://rdrr.io/cran/WebPower/man/wp.logistic.html, "Examples" section),
## for family = "normal", parameter = c(0, 1):
##   p0 = 0.15, p1 = 0.1, n = 200, alpha = 0.05 (two-sided)
##     -> beta1 = -0.4626235, power = 0.6299315
##   p0 = 0.15, p1 = 0.1, power = 0.8, alpha = 0.05 (two-sided)
##     -> n = 298.9207 (i.e., n_total = 299 after rounding up)
## ---------------------------------------------------------------------

test_that("probs_to_or computes the correct odds ratio", {
  # p0 = 0.2, p1 = 0.35 -> odds0 = 0.25, odds1 = 0.5385 -> OR = 2.1538
  or <- probs_to_or(0.2, 0.35)
  expect_equal(or, (0.35 / 0.65) / (0.2 / 0.8), tolerance = 1e-10)
})

test_that("power_logistic_at_n matches WebPower's published reference value (normal predictor)", {
  power_at_200 <- power_logistic_at_n(
    n_total = 200, p0 = 0.15, p1 = 0.1,
    predictor_dist = "normal", sig_level = 0.05, alternative = "two.sided"
  )
  expect_equal(power_at_200, 0.6299315, tolerance = 1e-5)
})

test_that("power_logistic_n matches WebPower's published reference value (normal predictor)", {
  res <- power_logistic_n(
    p0 = 0.15, p1 = 0.1, predictor_dist = "normal",
    sig_level = 0.05, power = 0.80, alternative = "two.sided"
  )
  # WebPower reports the exact (non-rounded) solution as n = 298.9207;
  # this app always rounds sample sizes up to the next integer.
  expect_equal(res$n_total, 299)
  expect_gte(res$power_achieved, 0.80)
})

test_that("logistic: sample size increases as p1 approaches p0 (effect shrinks)", {
  res_big_effect <- power_logistic_n(p0 = 0.2, p1 = 0.45, power = 0.80)
  res_small_effect <- power_logistic_n(p0 = 0.2, p1 = 0.28, power = 0.80)
  expect_gt(res_small_effect$n_total, res_big_effect$n_total)
})

test_that("logistic: power_logistic_at_n / power_logistic_n are consistent", {
  res <- power_logistic_n(p0 = 0.2, p1 = 0.4, power = 0.80)
  p_at_n <- power_logistic_at_n(res$n_total, p0 = 0.2, p1 = 0.4)
  expect_gte(p_at_n, 0.80 - 0.02)
})

test_that("logistic: binary predictor family behaves consistently too", {
  res <- power_logistic_n(p0 = 0.2, p1 = 0.4, predictor_dist = "binary",
                           prop_predictor = 0.5, power = 0.80)
  p_at_n <- power_logistic_at_n(res$n_total, p0 = 0.2, p1 = 0.4,
                                 predictor_dist = "binary", prop_predictor = 0.5)
  expect_gte(p_at_n, 0.80 - 0.02)
})

test_that("d_to_or and or_to_d are inverses", {
  d <- 0.5
  expect_equal(or_to_d(d_to_or(d)), d, tolerance = 1e-8)
})
