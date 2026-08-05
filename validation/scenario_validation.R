## validation/scenario_validation.R
## -----------------------------------------------------------------------
## Validates the DECISION HELPER, not the closed-form formulas -- the
## companion to monte_carlo_validation.R (which validates that the
## formulas compute correctly) and the piece the manuscript's actual
## claimed contribution rests on: not "the formulas work" (G*Power and
## `pwr` already establish that), but "a first-time user can be routed
## from a plain-language description of their design to the correct
## analysis family."
##
## Checks every scenario in scenario_benchmark.csv: given the answers a
## researcher would give to the three decision-helper questions (see
## R/decision_helper.R and the `decision_helper_acc` accordion in app.R),
## does recommend_family() return the expected family?
##
## Nine of the sixteen scenarios are the SAME worked examples shipped in
## the app itself (R/example_library.R, reachable from Step 1's "Try a
## worked example" entry point) -- reusing them here, rather than writing
## a separate set of made-up cases, is what makes this a real bridge
## between the repository and the manuscript: the same scenario, the same
## expected family, checked two different ways (as a runnable example in
## the app, and as an automated assertion here). The other seven extend
## coverage to every family and edge case the decision helper can reach
## that the shipped examples don't already cover (see "source" column).
##
## This is NOT a user study. It establishes that the decision helper's
## MAPPING from design description to analysis family is correct and
## regression-tested, not that real first-time users navigate it
## successfully -- that remains the untested design claim disclosed in
## the manuscript's Limitations section (see VALIDATION.md's "Scope"
## note). What it rules out is the specific, checkable failure mode of a
## first-time user answering honestly and being routed to the wrong
## family.
##
## Usage (from the project root):
##   Rscript validation/scenario_validation.R
## Exits 1 if any scenario mismatches (this one IS a real gate, unlike
## accessibility_check.R -- a decision-helper mapping is either right or
## wrong, there is no equivalent of "known issue, deliberately deferred").
## See tests/testthat/test-decision_helper.R for the same checks wired
## into the regular test suite.
## -----------------------------------------------------------------------

if (!file.exists("app.R")) {
  stop("Run this from the project root (the folder containing app.R), e.g.:\n",
       "  Rscript validation/scenario_validation.R")
}
source("global.R")

bench <- read.csv("validation/scenario_benchmark.csv", stringsAsFactors = FALSE, na.strings = "")

cat(sprintf("Scenario validation: decision helper vs. %d benchmark scenarios\n", nrow(bench)))
cat(sprintf("App version: %s\n\n", APP_VERSION))

n_pass <- 0L
n_fail <- 0L
for (i in seq_len(nrow(bench))) {
  row <- bench[i, ]
  got <- recommend_family(row$dh_unit, row$dh_outcome, row$dh_continuous, row$dh_binary)
  expected <- if (identical(row$expected_family, "unsupported")) NA_character_ else row$expected_family
  ok <- if (is.na(expected)) is.na(got) else identical(got, expected)

  status <- if (ok) "PASS" else "FAIL"
  if (ok) n_pass <- n_pass + 1L else n_fail <- n_fail + 1L

  answers <- paste(c(
    sprintf("unit=%s", row$dh_unit %||% "?"),
    sprintf("outcome=%s", row$dh_outcome %||% "?"),
    if (!is.na(row$dh_continuous)) sprintf("continuous=%s", row$dh_continuous),
    if (!is.na(row$dh_binary)) sprintf("binary=%s", row$dh_binary)
  ), collapse = ", ")

  cat(sprintf("[%s] %-32s (%s) [%s]\n", status, row$scenario, answers, row$source))
  cat(sprintf("       %s\n", row$title))
  cat(sprintf("       expected=%s  got=%s\n", row$expected_family, if (is.na(got)) "NA/unsupported" else got))
  if (!ok) cat("       *** MISMATCH ***\n")
  cat("\n")
}

cat(sprintf("Total: %d/%d scenarios routed to the expected family.\n", n_pass, n_pass + n_fail))
if (n_fail > 0) quit(status = 1)
