test_that("paired t: matches textbook pwr.t.test reference (d=0.5, standard settings)", {
  skip_if_not_installed("pwr")
  # Cohen (1988) / G*Power reference value: for a paired (matched-pairs)
  # t-test, dz = 0.5, alpha = .05 (two-tailed), power = .80 -> n = 33.37
  # pairs, i.e. 34 after rounding up. This is the same formula pwr uses
  # for type = "one.sample" (a paired test reduces to a one-sample test on
  # the difference scores), and matches G*Power 3.1's "t tests - Means:
  # Difference between two dependent means (matched pairs)" output for the
  # same inputs.
  res <- power_paired_t_n(d = 0.5, sig_level = 0.05, power = 0.80,
                           alternative = "two.sided")
  expect_equal(res$n_pairs, 34)
  expect_equal(res$n_total, 34)
  expect_gte(res$power_achieved, 0.80)
})

test_that("paired t: large effect requires fewer pairs than small effect", {
  n_small <- power_paired_t_n(d = 0.2, power = 0.80)$n_pairs
  n_large <- power_paired_t_n(d = 0.8, power = 0.80)$n_pairs
  expect_gt(n_small, n_large)
})

test_that("paired t: one-tailed test requires <= pairs than two-tailed", {
  n_two <- power_paired_t_n(d = 0.5, alternative = "two.sided")$n_pairs
  n_one <- power_paired_t_n(d = 0.5, alternative = "greater")$n_pairs
  expect_lte(n_one, n_two)
})

test_that("paired t requires fewer pairs than an independent two-means comparison for the same d", {
  # A paired design is always at least as efficient (never needs more
  # participants) as an independent-samples design with the same nominal
  # d, because the paired formula is mathematically the one-sample-test
  # formula (df = n-1) rather than the two-sample formula (df = 2n-2).
  n_paired <- power_paired_t_n(d = 0.5, power = 0.80)$n_pairs
  n_indep_per_group <- power_two_means_n(d = 0.5, power = 0.80)$n1
  expect_lt(n_paired, n_indep_per_group)
})

test_that("power_paired_t_at_n and power_paired_t_n are mutually consistent", {
  res <- power_paired_t_n(d = 0.5, power = 0.80)
  p_at_n <- power_paired_t_at_n(n = res$n_pairs, d = 0.5)
  expect_equal(p_at_n, res$power_achieved, tolerance = 1e-8)
})

test_that("power_paired_t_min_d is the inverse of power_paired_t_n", {
  res <- power_paired_t_n(d = 0.5, power = 0.80)
  min_d <- power_paired_t_min_d(n = res$n_pairs, power = 0.80)
  expect_equal(min_d, 0.5, tolerance = 0.02)
})
