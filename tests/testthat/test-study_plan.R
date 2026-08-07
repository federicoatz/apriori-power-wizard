## Tests for the cross-family study plan (R/study_plan.R).
##
## The property that matters most here is that the MAX/SUM distinction is
## never guessed. Getting it backwards is not a cosmetic error: treating
## two separate experiments as one sample under-recruits by the whole of
## the smaller requirement, and treating one sample as two over-recruits by
## the same amount. So the tests below pin both directions explicitly,
## rather than only checking the case the panel was designed around.

mk <- function(family, n, group = "Sample A", title = family) {
  list(family = family, title = title, n_total = n, group = group)
}

test_that("analyses on the same participants take the MAXIMUM, not the sum", {
  plan <- study_plan_requirement(list(
    mk("clustered_rct", 360), mk("clustered_cat", 552)
  ))
  expect_equal(plan$total, 552)
  expect_equal(plan$n_groups, 1)
  expect_equal(plan$n_entries, 2)
  expect_equal(plan$groups[[1]]$binding$family, "clustered_cat")
  # Explicitly NOT the sum -- the failure mode this whole file exists for.
  expect_false(plan$total == 360 + 552)
})

test_that("analyses on different participants ADD, and are not collapsed to a max", {
  plan <- study_plan_requirement(list(
    mk("two_means", 128, group = "Sample A"),
    mk("chisq", 300, group = "Sample B")
  ))
  expect_equal(plan$total, 428)
  expect_equal(plan$n_groups, 2)
  # The opposite error: taking the max here would under-recruit by 128.
  expect_false(plan$total == 300)
})

test_that("the two rules compose: max within each sample, sum across samples", {
  plan <- study_plan_requirement(list(
    mk("clustered_rct", 360, "Sample A"),
    mk("clustered_cat", 552, "Sample A"),
    mk("two_means", 128, "Sample B")
  ))
  expect_equal(plan$total, 552 + 128)
  expect_equal(plan$n_groups, 2)
  ns <- vapply(plan$groups, function(g) g$n, numeric(1))
  expect_setequal(ns, c(552, 128))
})

test_that("a group's entries are ordered by requirement, binding one first", {
  plan <- study_plan_requirement(list(
    mk("a", 100), mk("b", 900), mk("c", 400)
  ))
  expect_equal(vapply(plan$groups[[1]]$entries, function(e) e$n_total, numeric(1)),
               c(900, 400, 100))
  expect_equal(plan$groups[[1]]$binding$family, "b")
})

test_that("groups appear in the order they were first added, not alphabetically", {
  plan <- study_plan_requirement(list(
    mk("a", 100, "Sample C"), mk("b", 200, "Sample A")
  ))
  expect_equal(vapply(plan$groups, function(g) g$group, character(1)),
               c("Sample C", "Sample A"))
})

test_that("study_plan_shortfall reports what powering on one analysis would miss", {
  plan <- study_plan_requirement(list(
    mk("clustered_rct", 360), mk("clustered_cat", 552), mk("two_means", 128, "Sample B")
  ))
  # Total is 680; powering on the continuous family alone recruits 360.
  expect_equal(study_plan_shortfall(plan, "clustered_rct"), 320)
  expect_equal(study_plan_shortfall(plan, "clustered_cat"), 128)
  # A family not in the plan has no shortfall to report.
  expect_equal(study_plan_shortfall(plan, "regression"), 0)
})

test_that("a single-sample plan's binding analysis has zero shortfall", {
  plan <- study_plan_requirement(list(mk("a", 100), mk("b", 900)))
  expect_equal(study_plan_shortfall(plan, "b"), 0)
  expect_equal(study_plan_shortfall(plan, "a"), 800)
})

test_that("entries without a usable N are dropped, not propagated as NA", {
  plan <- study_plan_requirement(list(
    mk("a", 100), mk("b", NA), mk("c", NULL), mk("d", 0), mk("e", Inf)
  ))
  expect_equal(plan$n_entries, 1)
  expect_equal(plan$total, 100)
  expect_false(is.na(plan$total))
})

test_that("a missing or blank group label falls back to one shared sample", {
  plan <- study_plan_requirement(list(
    list(family = "a", title = "a", n_total = 100),
    list(family = "b", title = "b", n_total = 300, group = "")
  ))
  expect_equal(plan$n_groups, 1)
  expect_equal(plan$total, 300)
})

test_that("an empty or absent plan is a well-formed zero, not an error", {
  for (x in list(list(), NULL)) {
    plan <- study_plan_requirement(x)
    expect_equal(plan$total, 0)
    expect_equal(plan$n_entries, 0)
    expect_length(plan$groups, 0)
  }
})

test_that("the report sentence appears only once there are two analyses", {
  expect_equal(study_plan_report_sentence(study_plan_requirement(list())), "")
  expect_equal(study_plan_report_sentence(study_plan_requirement(list(mk("a", 100)))), "")
  s <- study_plan_report_sentence(study_plan_requirement(list(mk("a", 100), mk("b", 900))))
  expect_true(nzchar(s))
  expect_match(s, "largest of them rather than their sum")
  expect_match(s, "900")
})

test_that("the report sentence states the total explicitly for a multi-sample plan", {
  s <- study_plan_report_sentence(study_plan_requirement(list(
    mk("a", 552, "Sample A"), mk("b", 128, "Sample B")
  )))
  expect_match(s, "2 independent samples")
  expect_match(s, "N = 680")
})

test_that("the consumers degrade quietly on a malformed plan rather than erroring", {
  # Both are called from renderUI / report-building code, where raising
  # would blank the whole Results step over an additive panel.
  for (bad in list(NULL, list(), "not a plan", list(groups = list()))) {
    expect_equal(study_plan_report_sentence(bad), "")
    expect_equal(study_plan_shortfall(bad, "two_means"), 0)
  }
})

test_that("the plan reproduces the walkthrough in the paper", {
  # Two confirmatory tests on one sample of interacting matching groups:
  # a continuous primary outcome and a categorical secondary one, both
  # Bonferroni-corrected to alpha = .025 for two planned comparisons.
  cont <- power_clustered_n(d = 0.40, icc = 0.15, cluster_size = 4, sig_level = 0.025)
  cat_ <- power_clustered_cat_n(w = 0.185, df = 3, icc = 0.15, cluster_size = 4,
                                 sig_level = 0.025)
  plan <- study_plan_requirement(list(
    mk("clustered_rct", cont$n_total, title = "Clustered continuous outcome"),
    mk("clustered_cat", cat_$n_total, title = "Clustered categorical outcome")
  ))
  # The categorical secondary measure binds, not the primary hypothesis.
  expect_equal(plan$groups[[1]]$binding$family, "clustered_cat")
  expect_equal(plan$total, cat_$n_total)
  expect_gt(study_plan_shortfall(plan, "clustered_rct"), 0)
})
