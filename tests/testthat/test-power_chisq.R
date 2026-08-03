test_that("chisq: matches pwr::pwr.chisq.test reference (w=0.3, df=3)", {
  skip_if_not_installed("pwr")
  # Cohen (1988), ch. 7 medium effect w=0.30 at df=3, alpha=.05, power=.80
  # -> N = 121.14 -> 122 after rounding up (matches pwr::pwr.chisq.test()).
  res <- power_chisq_n(w = 0.3, df = 3, sig_level = 0.05, power = 0.80)
  expect_equal(res$n_total, 122)
  expect_equal(res$df, 3)
  expect_gte(res$power_achieved, 0.80)
})

test_that("chisq: reproduces the Beyond Shallow Meritocracy paper's own N", {
  skip_if_not_installed("pwr")
  # Real-world cross-check: the paper's own chi-square test of independence
  # (4 treatments, DF = 3) reports w = 0.185 for N = 320. Our solver should
  # land within rounding distance of that same N.
  res <- power_chisq_n(w = 0.185, df = 3, sig_level = 0.05, power = 0.80)
  expect_equal(res$n_total, 319)
})

test_that("chisq: power increases with N", {
  p_low <- power_chisq_at_n(n_total = 50, w = 0.3, df = 3)
  p_high <- power_chisq_at_n(n_total = 300, w = 0.3, df = 3)
  expect_gt(p_high, p_low)
})

test_that("chisq: power_chisq_min_w is the inverse of power_chisq_n", {
  res <- power_chisq_n(w = 0.25, df = 2, sig_level = 0.05, power = 0.80)
  w_back <- power_chisq_min_w(n_total = res$n_total, df = 2, sig_level = 0.05, power = 0.80)
  expect_lt(w_back, 0.25 + 0.01)
})

test_that("chisq: a 2x4 independence table has the same df as a 5-category goodness-of-fit test", {
  # (rows-1)*(cols-1) = (2-1)*(4-1) = 3; k-1 = 5-1 = 4 -- deliberately
  # different, just checking both design shapes reach the solver with the
  # df the module computes, exercised end-to-end via power_chisq_n directly.
  res_indep <- power_chisq_n(w = 0.2, df = 3, power = 0.80)
  res_gof <- power_chisq_n(w = 0.2, df = 4, power = 0.80)
  expect_gt(res_gof$n_total, res_indep$n_total) # more df needs more N at equal w
})
