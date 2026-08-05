## decision_helper.R
## -----------------------------------------------------------------------
## Pure decision-tree logic for Step 1's plain-language decision helper
## (see the `decision_helper_acc` accordion in app.R, questions dh_unit /
## dh_outcome / dh_continuous / dh_binary). Deliberately factored out of
## app.R's `recommended_family` reactive into its own file: the reactive
## is now a thin wrapper that reads the four inputs and calls
## `recommend_family()` below, so this mapping -- design principle #2,
## "separate analysis selection from numerical calculation" -- is testable
## directly, with no Shiny session, exactly like every power_*_n()
## function in R/. See validation/scenario_benchmark.csv and
## validation/scenario_validation.R for scenario-level checks against
## real experimental-economics/behavioral-science paradigms, and
## tests/testthat/test-decision_helper.R for the regression-test version
## of the same checks (run on every push, see .github/workflows/test.yml).
##
## Unanswered questions are represented as NULL, matching Shiny's own
## convention (an unset radioButtons() input is NULL, not NA/""), and the
## `%||%` operator used below is the one already defined once, app-wide,
## in modules/common_ui.R (loaded before this file is ever called, since
## every caller -- app.R, the test suite, the validation scripts -- goes
## through global.R first).
## -----------------------------------------------------------------------

#' Map decision-helper answers to a recommended analysis family
#'
#' @param dh_unit "individual" or "cluster" (question 1: do participants
#'   act independently, or in groups?), or NULL if unanswered
#' @param dh_outcome "continuous", "binary", "categorical", or
#'   "time_to_event" (question 2: outcome type), or NULL if unanswered
#' @param dh_continuous one of "two_groups", "paired", "repeated",
#'   "factorial", "predictor", "ancova", "nonparametric" -- only consulted
#'   when dh_unit == "individual" and dh_outcome == "continuous"
#'   (question 3)
#' @param dh_binary one of "two_groups", "paired", "predictor" -- only
#'   consulted when dh_unit == "individual" and dh_outcome == "binary"
#'   (question 3)
#' @return a family key (character, matching the names in
#'   `family_ui_fns` in app.R), or `NA_character_` if the answers given
#'   so far are incomplete or, for the one combination this app declines
#'   (clustered design with a time-to-event outcome -- see
#'   `decision_helper_unsupported()`), unsupported.
#' @export
recommend_family <- function(dh_unit = NULL, dh_outcome = NULL,
                              dh_continuous = NULL, dh_binary = NULL) {
  if (isTRUE(decision_helper_unsupported(dh_unit, dh_outcome))) return(NA_character_)

  fam <- if (identical(dh_unit, "cluster")) {
    switch(dh_outcome %||% "",
      continuous = "clustered_rct",
      binary = "clustered_cat",
      categorical = "clustered_cat",
      NA_character_)
  } else if (identical(dh_unit, "individual")) {
    if (identical(dh_outcome, "continuous")) {
      switch(dh_continuous %||% "",
        two_groups = "two_means", paired = "paired_t", factorial = "anova_factorial",
        predictor = "regression", ancova = "ancova",
        nonparametric = "wilcoxon", repeated = "rm_anova", NA_character_)
    } else if (identical(dh_outcome, "binary")) {
      switch(dh_binary %||% "",
        two_groups = "proportions", paired = "mcnemar", predictor = "logistic",
        NA_character_)
    } else if (identical(dh_outcome, "categorical")) {
      "chisq"
    } else if (identical(dh_outcome, "time_to_event")) {
      "survival"
    } else {
      NA_character_
    }
  } else {
    NA_character_
  }
  if (is.null(fam)) NA_character_ else fam
}

#' Is this combination of answers one the decision helper declines?
#'
#' The only case: a clustered/interacting design with a time-to-event
#' outcome. Combining the design-effect correction (R/power_clustered.R)
#' with an events-based sample size (R/power_survival.R) is meaningfully
#' more involved than either piece alone -- see the manuscript's
#' Discussion/Limitations for why this is a deliberate scope boundary
#' rather than an oversight.
#'
#' @inheritParams recommend_family
#' @export
decision_helper_unsupported <- function(dh_unit = NULL, dh_outcome = NULL) {
  identical(dh_unit, "cluster") && identical(dh_outcome %||% "", "time_to_event")
}
