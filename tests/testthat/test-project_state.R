test_that("capture_family_state keeps only the active family's inputs, prefix stripped", {
  all_inputs <- list(
    analysis_choice = "two_means",
    start_wizard = 1,
    `two_means-alpha` = 0.05,
    `two_means-power` = 0.80,
    `two_means-es_branch` = "sesoi",
    `anova_factorial-alpha` = 0.01
  )
  st <- capture_family_state(all_inputs, "two_means")
  expect_equal(st$family, "two_means")
  expect_equal(st$inputs$alpha, 0.05)
  expect_equal(st$inputs$power, 0.80)
  expect_equal(st$inputs$es_branch, "sesoi")
  expect_false("anova_factorial-alpha" %in% names(st$inputs))
  expect_false(any(grepl("^anova_factorial", names(st$inputs))))
})

test_that("capture_family_state excludes click-counter-style inputs", {
  all_inputs <- list(
    `two_means-alpha` = 0.05,
    `two_means-nav_next_design` = 3,
    `two_means-nav_back_params` = 1,
    `two_means-use_90` = 2,
    `two_means-back_to_step1` = 1,
    `two_means-es_helper_use` = 1,
    `two_means-share_link` = 1,
    `two_means-save_project` = 1,
    `two_means-wiz` = "effect_size"
  )
  st <- capture_family_state(all_inputs, "two_means")
  expect_equal(names(st$inputs), c("alpha", "wiz"))
})

test_that("capture_family_state drops NULL/unset inputs", {
  all_inputs <- list(`two_means-alpha` = 0.05, `two_means-sesoi_d` = NULL)
  st <- capture_family_state(all_inputs, "two_means")
  expect_equal(names(st$inputs), "alpha")
})

test_that("state_to_json / json_to_state round-trip exactly", {
  st <- list(family = "two_means", inputs = list(alpha = 0.05, es_branch = "sesoi", power = 0.8))
  back <- json_to_state(state_to_json(st))
  expect_equal(back$family, "two_means")
  expect_equal(back$inputs$alpha, 0.05)
  expect_equal(back$inputs$es_branch, "sesoi")
  expect_equal(back$inputs$power, 0.8)
})

test_that("json_to_query / query_to_json round-trip exactly, including +, /, = characters", {
  json <- state_to_json(list(family = "chisq", inputs = list(note = "a/b+c=d test", n_rows = 4)))
  q <- json_to_query(json)
  expect_false(grepl("[+/=]", q))
  expect_equal(query_to_json(q), json)
})

test_that("the full round-trip (capture -> json -> query -> json -> state) is lossless", {
  all_inputs <- list(`mcnemar-p10` = 0.25, `mcnemar-p01` = 0.10, `mcnemar-alpha` = 0.05)
  st <- capture_family_state(all_inputs, "mcnemar")
  q <- json_to_query(state_to_json(st))
  back <- json_to_state(query_to_json(q))
  expect_equal(back$family, "mcnemar")
  expect_equal(back$inputs$p10, 0.25)
  expect_equal(back$inputs$p01, 0.10)
  expect_equal(back$inputs$alpha, 0.05)
})

test_that("restorable_state_inputs applies the SAME exclusions capture does", {
  # The asymmetry this closes: .state_exclude_re filtered on capture but not
  # on restore, so a hand-edited project file or a crafted ?state= link
  # could name ids the capture side excludes by design.
  crafted <- list(
    d = 0.5, alpha = 0.05, wiz = "results",   # legitimate, must survive
    nav_next_design = 7, nav_back_params = 3, # click counters, must not
    use_90 = 1, back_to_step1 = 2, es_helper_use = 4,
    share_link = 1, save_project = 1, load_project_file = "x"
  )
  keep <- restorable_state_inputs(crafted)
  expect_setequal(keep, c("d", "alpha", "wiz"))

  # The invariant, stated directly: whatever capture would drop, restore
  # drops too. Anything capture keeps, restore keeps.
  all_inputs <- stats::setNames(as.list(seq_along(crafted)),
                                 paste0("two_means-", names(crafted)))
  captured <- capture_family_state(all_inputs, "two_means")
  expect_setequal(names(captured$inputs), restorable_state_inputs(names(crafted)))
})

test_that("restorable_state_inputs is a no-op for a state this app wrote", {
  all_inputs <- list(
    `two_means-d` = 0.5, `two_means-alpha` = 0.05, `two_means-wiz` = "effect_size",
    `two_means-nav_next_design` = 1
  )
  captured <- capture_family_state(all_inputs, "two_means")
  round_trip <- json_to_state(state_to_json(captured))
  expect_setequal(restorable_state_inputs(round_trip$inputs),
                  names(round_trip$inputs))
})

test_that("restorable_state_inputs handles an empty or malformed state", {
  expect_length(restorable_state_inputs(list()), 0)
  expect_length(restorable_state_inputs(NULL), 0)
  expect_length(restorable_state_inputs(character(0)), 0)
  # An unnamed list carries nothing restorable, rather than erroring.
  expect_length(restorable_state_inputs(list(1, 2, 3)), 0)
})
