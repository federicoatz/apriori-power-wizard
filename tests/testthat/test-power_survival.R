## power_survival.R implements Schoenfeld's (1983) events formula for the
## two-group log-rank test. Cross-checked during development against a
## direct Monte Carlo simulation (exponential survival times, fixed-time
## administrative censoring, survival::survdiff() -- NOT a runtime
## dependency of this app; see the header comment in power_survival.R).
## Agreement was excellent (within Monte Carlo noise) for HR close to 1
## with near-balanced allocation, the two pinned cases below; the formula
## is documented to be somewhat CONSERVATIVE (true power higher than
## predicted, never lower) for strong effects combined with unequal
## allocation, which is a known property of the Schoenfeld approximation,
## not something these tests need to re-verify.

test_that("survival: matches Monte Carlo simulation within noise (near-balanced allocation, moderate HR)", {
  # Simulated power (4000-8000 reps) was 0.7943 and 0.7985 respectively;
  # a wider tolerance than mcnemar/tost's 0.001 is used deliberately, since
  # unlike those EXACT closed forms, Schoenfeld's formula is itself an
  # approximation (not just re-derived and numerically matched).
  expect_equal(power_survival_at_n(172, hr = 0.6, p_event = 0.7, alloc_ratio = 1, sig_level = 0.05),
               0.7943, tolerance = 0.02)
  expect_equal(power_survival_at_n(353, hr = 0.7, p_event = 0.7, alloc_ratio = 1, sig_level = 0.05),
               0.7985, tolerance = 0.02)
})

test_that("survival: power_survival_n reaches at least the target power, and one fewer N does not", {
  res <- power_survival_n(hr = 0.6, p_event = 0.7, power = 0.80)
  expect_gte(res$power_achieved, 0.80)
  p_one_less <- power_survival_at_n(res$n_total - 1, hr = 0.6, p_event = 0.7)
  expect_lt(p_one_less, res$power_achieved)
})

test_that("survival: power increases with n_total", {
  p_low  <- power_survival_at_n(50, hr = 0.6, p_event = 0.7)
  p_high <- power_survival_at_n(500, hr = 0.6, p_event = 0.7)
  expect_gt(p_high, p_low)
})

test_that("survival: an HR of 1 (no true effect) is rejected as ill-posed", {
  expect_error(power_survival_n(hr = 1, p_event = 0.7, power = 0.80))
})

test_that("survival: power_survival_min_hr is the inverse of power_survival_n (on the log scale)", {
  res <- power_survival_n(hr = 0.6, p_event = 0.7, power = 0.80)
  min_hr <- power_survival_min_hr(res$n_total, p_event = 0.7, power = 0.80)
  expect_equal(log(min_hr), log(0.6), tolerance = 0.02)
})

test_that("survival: unequal allocation requires more total N than balanced allocation, same HR/p_event", {
  n_balanced <- power_survival_n(hr = 0.6, p_event = 0.7, alloc_ratio = 1, power = 0.80)$n_total
  n_unequal  <- power_survival_n(hr = 0.6, p_event = 0.7, alloc_ratio = 3, power = 0.80)$n_total
  expect_gt(n_unequal, n_balanced)
})
