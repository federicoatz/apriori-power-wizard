## Regression-test version of validation/scenario_validation.R: fast,
## deterministic, runs in CI on every push (see .github/workflows/test.yml).
## The standalone script exists separately because it is meant to be read
## as evidence in its own right (see VALIDATION.md), not just as a test.

test_that("recommend_family covers every branch of the individual/continuous question", {
  expect_equal(recommend_family("individual", "continuous", "two_groups"), "two_means")
  expect_equal(recommend_family("individual", "continuous", "paired"), "paired_t")
  expect_equal(recommend_family("individual", "continuous", "repeated"), "rm_anova")
  expect_equal(recommend_family("individual", "continuous", "factorial"), "anova_factorial")
  expect_equal(recommend_family("individual", "continuous", "predictor"), "regression")
  expect_equal(recommend_family("individual", "continuous", "ancova"), "ancova")
  expect_equal(recommend_family("individual", "continuous", "nonparametric"), "wilcoxon")
})

test_that("recommend_family covers every branch of the individual/binary question", {
  expect_equal(recommend_family("individual", "binary", dh_binary = "two_groups"), "proportions")
  expect_equal(recommend_family("individual", "binary", dh_binary = "paired"), "mcnemar")
  expect_equal(recommend_family("individual", "binary", dh_binary = "predictor"), "logistic")
})

test_that("recommend_family handles categorical and time-to-event directly (no sub-question)", {
  expect_equal(recommend_family("individual", "categorical"), "chisq")
  expect_equal(recommend_family("individual", "time_to_event"), "survival")
})

test_that("recommend_family covers the cluster branch", {
  expect_equal(recommend_family("cluster", "continuous"), "clustered_rct")
  expect_equal(recommend_family("cluster", "binary"), "clustered_cat")
  expect_equal(recommend_family("cluster", "categorical"), "clustered_cat")
})

test_that("recommend_family returns NA for the one declined combination", {
  expect_true(is.na(recommend_family("cluster", "time_to_event")))
  expect_true(decision_helper_unsupported("cluster", "time_to_event"))
  expect_false(decision_helper_unsupported("individual", "time_to_event"))
  expect_false(decision_helper_unsupported("cluster", "continuous"))
})

test_that("recommend_family returns NA (never errors) for incomplete or unanswered questions", {
  expect_true(is.na(recommend_family()))
  expect_true(is.na(recommend_family("individual")))
  expect_true(is.na(recommend_family("individual", "continuous")))
  expect_true(is.na(recommend_family("individual", "binary")))
  expect_true(is.na(recommend_family("nonsense", "continuous", "two_groups")))
})

test_that("recommend_family agrees with every scenario in the published benchmark", {
  cand <- c("../../validation/scenario_benchmark.csv",
            "../validation/scenario_benchmark.csv", "validation/scenario_benchmark.csv")
  path <- cand[vapply(cand, file.exists, logical(1))]
  skip_if(length(path) == 0, "scenario_benchmark.csv not found from this working directory")
  bench <- read.csv(path[[1]], stringsAsFactors = FALSE, na.strings = "")

  for (i in seq_len(nrow(bench))) {
    row <- bench[i, ]
    got <- recommend_family(row$dh_unit, row$dh_outcome, row$dh_continuous, row$dh_binary)
    expected <- if (identical(row$expected_family, "unsupported")) NA_character_ else row$expected_family
    if (is.na(expected)) {
      expect_true(is.na(got), info = sprintf("scenario '%s': expected NA/unsupported, got '%s'", row$scenario, got))
    } else {
      expect_equal(got, expected, info = sprintf("scenario '%s'", row$scenario))
    }
  }
})
