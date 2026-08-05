test_that("safeguard_ci_d lower bound is below the published estimate", {
  sg <- safeguard_ci_d(d_published = 0.6, n1 = 30, n2 = 30, conf_level = 0.80, one_sided = TRUE)
  expect_lt(sg$lower, 0.6)
  # `$lower` and `$d_safeguard` coincide ONLY in the positive half-plane,
  # which is the sole reason this assertion held before the sign fix. It is
  # kept, scoped to that half-plane explicitly, and paired with the negative
  # case below -- otherwise it silently certifies the raw endpoint as if it
  # were the safeguard bound.
  expect_equal(sg$d_safeguard, sg$lower)
})

test_that("for a negative estimate the safeguard bound is NOT the raw lower endpoint", {
  sg <- safeguard_ci_d(d_published = -0.6, n1 = 30, n2 = 30, conf_level = 0.80, one_sided = TRUE)
  # the raw lower endpoint runs away from the null...
  expect_lt(sg$lower, -0.6)
  # ...while the safeguard bound moves toward it, and the two must differ
  expect_gt(sg$d_safeguard, -0.6)
  expect_lt(sg$d_safeguard, 0)
  expect_false(isTRUE(all.equal(sg$d_safeguard, sg$lower)))
})

test_that("safeguard_ci_d uses qnorm(0.80) for the default one-sided level", {
  sg <- safeguard_ci_d(d_published = 0.6, n1 = 30, n2 = 30, conf_level = 0.80, one_sided = TRUE)
  se <- sqrt(60 / (30 * 30) + 0.6^2 / (2 * 60))
  expect_equal(sg$se, se, tolerance = 1e-10)
  expect_equal(sg$lower, 0.6 - qnorm(0.80) * se, tolerance = 1e-10)
})

test_that("safeguard correction increases required N relative to the naive published estimate", {
  d_published <- 0.6
  n_pub <- 40
  sg <- safeguard_ci_d(d_published, n1 = n_pub / 2, n2 = n_pub / 2, conf_level = 0.80)
  n_naive <- power_two_means_n(d_published, power = 0.80)$n_total
  n_safeguard <- power_two_means_n(sg$d_safeguard, power = 0.80)$n_total
  expect_gt(n_safeguard, n_naive)
})

test_that("safeguard_ci_d with a larger original study N yields a tighter (higher) lower bound", {
  sg_small_study <- safeguard_ci_d(0.6, n1 = 10, n2 = 10)
  sg_large_study <- safeguard_ci_d(0.6, n1 = 500, n2 = 500)
  expect_gt(sg_large_study$lower, sg_small_study$lower)
})

test_that("safeguard_ci_r (Fisher z) recovers a known transformation", {
  r <- 0.3
  n <- 100
  sg <- safeguard_ci_r(r, n, conf_level = 0.80, one_sided = TRUE)
  z <- atanh(r)
  se <- 1 / sqrt(n - 3)
  expect_equal(sg$lower, tanh(z - qnorm(0.80) * se), tolerance = 1e-10)
})

test_that("safeguard_ci_w lower bound is below the published estimate", {
  sg <- safeguard_ci_w(w_published = 0.3, n_published = 300, df = 3, conf_level = 0.80)
  expect_lt(sg$lower, 0.3)
  expect_equal(sg$w_safeguard, sg$lower)
})

test_that("safeguard_ci_w with a larger original study N yields a tighter (higher) lower bound", {
  sg_small_study <- safeguard_ci_w(0.3, n_published = 60, df = 3)
  sg_large_study <- safeguard_ci_w(0.3, n_published = 2000, df = 3)
  expect_gt(sg_large_study$lower, sg_small_study$lower)
})

test_that("safeguard correction increases required N for chi-square relative to the naive published estimate", {
  w_published <- 0.3
  n_pub <- 100
  sg <- safeguard_ci_w(w_published, n_published = n_pub, df = 3, conf_level = 0.80)
  n_naive <- power_chisq_n(w_published, df = 3, power = 0.80)$n_total
  n_safeguard <- power_chisq_n(sg$w_safeguard, df = 3, power = 0.80)$n_total
  expect_gt(n_safeguard, n_naive)
})

test_that("d_to_or / or_to_d round-trip and match Chinn (2000) at Cohen's medium d", {
  d_medium <- 0.5
  or <- d_to_or(d_medium)
  expect_equal(or, exp(d_medium * pi / sqrt(3)), tolerance = 1e-10)
  expect_equal(or_to_d(or), d_medium, tolerance = 1e-8)
})

# --- Sign handling -------------------------------------------------------
# Both bugs below shipped in v1.0.3 and produced wrong answers silently
# rather than failing, which is why they are pinned here.

test_that("safeguard_shrink moves toward the null from either side and never crosses it", {
  # positive and negative estimates of equal magnitude must shrink
  # symmetrically, never flip sign, never grow
  pos <- safeguard_shrink( 0.50, se = 0.10, conf_level = 0.80)
  neg <- safeguard_shrink(-0.50, se = 0.10, conf_level = 0.80)
  expect_gt(pos, 0);  expect_lt(pos, 0.50)
  expect_lt(neg, 0);  expect_gt(neg, -0.50)
  expect_equal(pos, -neg, tolerance = 1e-12)

  # a bound that would cross the null is clamped just short of it,
  # on the correct side
  expect_gt(safeguard_shrink( 0.01, se = 5, conf_level = 0.95), 0)
  expect_lt(safeguard_shrink(-0.01, se = 5, conf_level = 0.95), 0)
})

test_that("a protective hazard ratio never comes back as a harmful one", {
  # HR = 0.85 from 40 events has |log HR| / SE < z even at the 80% default,
  # so the naive lower bound crosses HR = 1 and would return a plausible
  # sample size for an effect in the opposite direction.
  for (cl in c(0.80, 0.95)) {
    hr <- safeguard_ci_logHR(0.85, events_published = 40, conf_level = cl)$hr_safeguard
    expect_lt(hr, 1)
  }
  # and a harmful one stays harmful
  expect_gt(safeguard_ci_logHR(1.80, events_published = 60)$hr_safeguard, 1)
})

test_that("a negative published correlation is shrunk toward zero, not away from it", {
  pos <- safeguard_ci_r( 0.30, n = 60)$r_safeguard
  neg <- safeguard_ci_r(-0.30, n = 60)$r_safeguard
  expect_gt(pos, 0); expect_lt(pos, 0.30)
  expect_lt(neg, 0); expect_gt(neg, -0.30)
  expect_equal(pos, -neg, tolerance = 1e-12)
})

test_that("a diverged sample size raises a clean error instead of hanging", {
  # near-null effects reach the unbounded refinement loops in these
  # families; each must fail fast rather than walk upward forever
  expect_error(power_survival_n(hr = 0.9999, p_event = 0.5), "indistinguishable from no effect")
  expect_error(power_wilcoxon_n(p_sup = 0.50001), "indistinguishable from no effect")
  expect_error(power_ancova_n(k = 2, f_target = 1e-6, r_cov = 0.5), "indistinguishable from no effect")
  # and normal cases are untouched
  expect_gt(power_survival_n(hr = 0.60, p_event = 0.5)$n_total, 0)
})

test_that("published Table 3 safeguard values are unchanged by the sign fix", {
  expect_equal(safeguard_ci_d(0.40, 50, 50)$d_safeguard, 0.230, tolerance = 1e-3)
  expect_equal(safeguard_ci_r(0.516, 55)$r_safeguard, 0.425, tolerance = 1e-3)
  expect_equal(safeguard_ci_h(1.049984, 152, 155)$h_safeguard, 0.954, tolerance = 1e-3)
  expect_equal(safeguard_ci_d(1.344628, 72, 72)$d_safeguard, 1.189, tolerance = 1e-3)
})
