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
  # Bonferroni is the backward-compatible default method: saved projects
  # from versions before the mc_method input existed restore without it.
  expect_equal(p$mc_method, "bonferroni")
})

test_that("read_params_step Sidak-corrects alpha when selected", {
  p4 <- read_params_step(list(alpha = 0.05, power = 0.80, tails = "two.sided",
                               balanced = TRUE, n_comparisons = 4,
                               mc_method = "sidak"))
  expect_equal(p4$alpha, 1 - (1 - 0.05)^(1 / 4), tolerance = 1e-12)
  expect_equal(p4$mc_method, "sidak")
  expect_equal(p4$alpha_nominal, 0.05)

  # Sidak is never MORE conservative than Bonferroni (its per-test alpha
  # is always at least as large), so it can never demand a larger N.
  for (k in c(2, 5, 20, 100)) {
    sidak <- read_params_step(list(alpha = 0.05, n_comparisons = k, mc_method = "sidak"))$alpha
    bonf <- read_params_step(list(alpha = 0.05, n_comparisons = k, mc_method = "bonferroni"))$alpha
    expect_gte(sidak, bonf)
    # and both control the family-wise rate at or below nominal under
    # independence: 1 - (1-alpha_per)^k <= alpha_nominal (equality for Sidak)
    expect_lte(1 - (1 - sidak)^k, 0.05 + 1e-12)
    expect_lte(1 - (1 - bonf)^k, 0.05 + 1e-12)
  }

  # with a single comparison the method is irrelevant and alpha untouched
  p1 <- read_params_step(list(alpha = 0.05, n_comparisons = 1, mc_method = "sidak"))
  expect_equal(p1$alpha, 0.05)
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

test_that("recruitment_target is the identity when there is no attrition", {
  for (res in list(power_two_means_n(d = 0.5),
                   power_clustered_n(d = 0.5, icc = 0.05, cluster_size = 8),
                   power_anova_factorial_n(levels_a = 2, levels_b = 3, focal = "A", f_target = 0.3),
                   power_regression_n(f2 = 0.15))) {
    expect_equal(recruitment_target(res, 0)$n_recruit, res$n_total)
  }
})

test_that("recruitment_target inflates the DESIGN UNIT, not the grand total", {
  # The defect this closes: attrition was applied as n_total / (1 - a) for
  # every family, which for a clustered design produces a recruitment
  # target that is not a whole number of participants per cluster, and for
  # a factorial design one that cannot be split evenly across cells. Both
  # break the structure the power calculation assumed.
  a <- 0.10

  cl <- power_clustered_n(d = 0.5, icc = 0.05, cluster_size = 8)
  rt <- recruitment_target(cl, a)
  expect_equal(rt$unit, "cluster")
  # Cluster COUNT is preserved (it is what carries the power); cluster SIZE
  # absorbs the losses, as a whole number of participants.
  expect_equal(rt$n_units, cl$n_clusters_total)
  expect_equal(rt$per_unit_recruit, ceiling(cl$cluster_size / (1 - a)))
  expect_equal(rt$n_recruit, cl$n_clusters_total * rt$per_unit_recruit)
  # Divisible by the cluster count, which the naive total need not be.
  expect_equal(rt$n_recruit %% rt$n_units, 0)
  expect_false(round_up_n(cl$n_total / (1 - a)) %% cl$n_clusters_total == 0)

  fac <- power_anova_factorial_n(levels_a = 2, levels_b = 3, focal = "A", f_target = 0.3)
  rt <- recruitment_target(fac, a)
  expect_equal(rt$unit, "cell")
  expect_equal(rt$n_units, fac$n_cells)
  expect_equal(rt$per_unit_recruit, ceiling(fac$n_per_cell / (1 - a)))
  expect_equal(rt$n_recruit %% fac$n_cells, 0)

  anc <- power_ancova_n(k = 3, f_target = 0.3, r_cov = 0.4)
  rt <- recruitment_target(anc, a)
  expect_equal(rt$unit, "group")
  expect_equal(rt$n_recruit %% anc$k, 0)
})

test_that("recruitment_target preserves an unbalanced allocation across arms", {
  res <- power_two_means_n(d = 0.5, allocation_ratio = 2)
  rt <- recruitment_target(res, 0.10)
  expect_equal(rt$unit, "arm")
  expect_equal(rt$per_unit_recruit, ceiling(res$n1 / (1 - 0.10)))
  expect_equal(rt$n_recruit, ceiling(res$n1 / 0.9) + ceiling(res$n2 / 0.9))
})

test_that("recruitment_target never asks for fewer participants than the analytic N", {
  designs <- list(power_two_means_n(d = 0.5),
                  power_two_means_n(d = 0.5, allocation_ratio = 0.5),
                  power_clustered_n(d = 0.5, icc = 0.05, cluster_size = 8),
                  power_clustered_cat_n(w = 0.3, df = 3, icc = 0.05, cluster_size = 8),
                  power_anova_factorial_n(levels_a = 2, levels_b = 2, focal = "A", f_target = 0.3),
                  power_ancova_n(k = 3, f_target = 0.3, r_cov = 0.4),
                  power_rm_anova_n(m = 3, f_target = 0.3, rho = 0.5),
                  power_regression_n(f2 = 0.15),
                  power_correlation_n(r = 0.3))
  for (res in designs) {
    for (a in c(0, 0.05, 0.2, 0.5)) {
      rt <- recruitment_target(res, a)
      expect_gte(rt$n_recruit, res$n_total)
      # Rounding up per unit can exceed the naive total, but never by more
      # than one extra participant per design unit.
      expect_lte(rt$n_recruit, ceiling(res$n_total / (1 - a)) + rt$n_units)
    }
  }
})

test_that("recruitment_target refuses nonsense rather than returning a wrong number", {
  res <- power_two_means_n(d = 0.5)
  expect_true(is.na(recruitment_target(res, 1)$n_recruit))
  expect_true(is.na(recruitment_target(res, -0.1)$n_recruit))
  expect_true(is.na(recruitment_target(NULL, 0.1)$n_recruit))
  expect_equal(recruitment_target(res, NA)$n_recruit, res$n_total)
})

test_that("the params step tells users WHEN to choose Bonferroni vs Sidak", {
  # Guidance text, pinned because it makes a substantive claim a reader
  # could act on and because prose is exactly what a UI refactor drops
  # silently. The claim: Sidak's exactness assumes independent
  # comparisons, several outcomes on the same participants are correlated
  # so that assumption fails, and the choice barely moves N either way
  # (mean 0.4%, max ~2% across families, effect sizes and k = 2..20 --
  # reproduce with the loop in the v1.4.1 changelog entry).
  #
  # Unlike the rest of this file, this one RENDERS UI, so it needs shiny
  # and bslib actually attached -- params_step_ui() calls h3()/accordion()
  # unqualified. tests/testthat.R deliberately sources common_ui.R without
  # attaching either (see its header), so they are attached here rather
  # than skipped: a silent skip under the standard entry point would hide
  # exactly the regression this test is for.
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
  })

  html <- as.character(params_step_ui(shiny::NS("two_means")))
  squashed <- gsub("\\s+", " ", html)

  # Always visible once Advanced options is open, not only on hover.
  expect_match(squashed, "Keep Bonferroni unless your comparisons are independent")
  expect_match(squashed, "measured on the same people are correlated")
  expect_match(squashed, "choice about assumptions, not about sample size")

  # And the fuller rule in the tooltip.
  expect_match(squashed, "WHICH TO PICK")
  expect_match(squashed, "pre-specified orthogonal contrasts")
})

test_that("the Bonferroni/Sidak difference really is as small as the interface claims", {
  # The interface tells users the two corrections rarely differ by more
  # than 1-2% of N. That is a checkable claim about this app's own
  # solvers, so it is checked rather than asserted -- if a solver changes
  # and the gap widens, the guidance becomes wrong and this fails.
  diffs <- c()
  for (k in 2:20) {
    a_bonf <- 0.05 / k
    a_sidak <- 1 - (1 - 0.05)^(1 / k)
    for (d in c(0.2, 0.5, 0.8)) {
      nb <- power_two_means_n(d = d, sig_level = a_bonf)$n_total
      ns <- power_two_means_n(d = d, sig_level = a_sidak)$n_total
      # Sidak is never stricter than Bonferroni: it must never ask for more.
      expect_lte(ns, nb)
      diffs <- c(diffs, 100 * (nb - ns) / nb)
    }
    for (h in c(0.2, 0.5)) {
      nb <- power_proportions_n(h = h, sig_level = a_bonf)$n_total
      ns <- power_proportions_n(h = h, sig_level = a_sidak)$n_total
      expect_lte(ns, nb)
      diffs <- c(diffs, 100 * (nb - ns) / nb)
    }
  }
  expect_lt(mean(diffs), 1)
  expect_lt(max(diffs), 3)
})
