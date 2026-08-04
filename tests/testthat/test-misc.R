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

test_that("effect_comparison_values (magnitude) scales by simple multiplication and excludes the current value", {
  out <- effect_comparison_values(0.5, kind = "magnitude")
  expect_equal(unname(sort(out)), sort(0.5 * c(0.5, 0.8, 1.2, 1.5)))
  expect_false(any(abs(out - 0.5) < 1e-9))
  expect_true(all(grepl("weaker|stronger", names(out))))
})

test_that("effect_comparison_values (ratio) scales in log space around 1", {
  out <- effect_comparison_values(0.6, kind = "ratio")
  expect_equal(unname(sort(out)), sort(exp(log(0.6) * c(0.5, 0.8, 1.2, 1.5))))
  # A ratio > 1 (harmful direction) should scale symmetrically the same way.
  out2 <- effect_comparison_values(1.8, kind = "ratio")
  expect_equal(unname(sort(out2)), sort(exp(log(1.8) * c(0.5, 0.8, 1.2, 1.5))))
})

test_that("effect_comparison_values returns an empty vector for an invalid current effect", {
  expect_length(effect_comparison_values(NA_real_), 0)
  expect_length(effect_comparison_values(0), 0)
  expect_length(effect_comparison_values(-1), 0)
})

test_that("APP_VERSION (global.R) matches the version: field in CITATION.cff", {
  # Every saved project file and generated report is stamped with
  # APP_VERSION (see R/report_text.R, R/project_state.R), and CITATION.cff
  # is what "please cite this software" points people to -- if the two
  # drift apart, a result traced back to "v0.16.0" and a citation claiming
  # "v0.17.0" would disagree about which version actually produced it.
  cand <- c("../../CITATION.cff", "../CITATION.cff", "CITATION.cff")
  path <- cand[vapply(cand, file.exists, logical(1))]
  skip_if(length(path) == 0, "CITATION.cff not found from this working directory")
  cff <- readLines(path[[1]], warn = FALSE)
  version_line <- grep("^version:", cff, value = TRUE)
  skip_if(length(version_line) == 0, "no version: field in CITATION.cff")
  cff_version <- gsub('"', "", trimws(sub("^version:", "", version_line[[1]])))
  expect_equal(APP_VERSION, cff_version)
})
