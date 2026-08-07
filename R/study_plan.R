## study_plan.R
## -----------------------------------------------------------------------
## Aggregating several analyses from DIFFERENT families into one
## recruitment requirement.
##
## Why this exists. Every family in this app is a self-contained wizard,
## and until now the app never looked at two of them together. That was a
## real gap rather than a tidy scope boundary: the shared "number of
## planned comparisons" control asks for the TOTAL number of planned
## comparisons and corrects alpha accordingly, but the N it then reports
## covers only the family the user is standing in. A researcher planning
## two confirmatory tests on one sample -- a continuous primary outcome
## and a categorical secondary one, say -- could enter 2 in the first
## family, read the corrected N, and under-recruit substantially, because
## the second test's requirement is larger and nothing said so. The
## multiple-comparisons control made that worse rather than better: its
## presence is reassuring precisely where it does not reach.
##
## THE ARITHMETIC, which is the whole reason this is not a one-liner.
## Combining two required sample sizes depends on whether the two analyses
## are run on the SAME PARTICIPANTS:
##
##   same participants   -> take the MAXIMUM. Both tests are run on one
##                          sample, so that sample must be large enough
##                          for the more demanding of them. Adding them
##                          would recruit people twice.
##   different participants -> take the SUM. Two experiments in one paper
##                          need both samples; taking the maximum would
##                          under-recruit by the whole of the smaller.
##
## Getting this backwards is dangerous in one direction, so this file never
## infers the relation. The caller supplies a sample-group label per entry,
## the user supplies that label explicitly in the interface, and the rule
## is simply: maximum within a group, sum across groups.
##
## Note what is deliberately NOT attempted. This does not adjust alpha
## across families -- that is what the existing per-family multiplicity
## control does, and the user is expected to set the same total in each
## family (the interface says so). Nor does it model partial overlap
## between samples, which has no closed-form answer and would need a
## design the user has not been asked to describe.
## -----------------------------------------------------------------------

#' Is this a plan object produced by [study_plan_requirement()]?
#'
#' The two consumers below are called from renderUI/report code where a
#' malformed or half-built argument must degrade to "say nothing" rather
#' than raise -- an error here would blank the whole Results step over a
#' panel that is meant to be additive.
#' @keywords internal
.is_plan <- function(plan) {
  is.list(plan) && !is.null(plan$n_entries) && length(plan$n_entries) == 1 &&
    !is.na(plan$n_entries) && is.numeric(plan$n_entries)
}

#' Combine several families' requirements into one recruitment total
#'
#' @param entries list of entries, each a list with at least `family`
#'   (character key), `title` (character, human-readable), `n_total`
#'   (numeric), and `group` (character, the sample-group label the user
#'   assigned). Optional: `n_units` and `unit_label` for designs counted
#'   in clusters/cells, carried through for display only.
#' @return list with `total` (participants to recruit across all groups),
#'   `groups` (list, one per sample group, each with `group`, `n` = that
#'   group's requirement, `binding` = the entry that sets it, and
#'   `entries` sorted by requirement descending), `n_entries`, and
#'   `n_groups`. Returns a zero-entry structure for empty input.
#' @export
study_plan_requirement <- function(entries) {
  empty <- list(total = 0L, groups = list(), n_entries = 0L, n_groups = 0L)
  if (is.null(entries) || length(entries) == 0) return(empty)

  # Drop anything without a usable requirement rather than letting an NA
  # propagate into the headline total. A family whose inputs are mid-edit
  # legitimately has no number yet.
  ok <- vapply(entries, function(e) {
    !is.null(e$n_total) && length(e$n_total) == 1 &&
      !is.na(e$n_total) && is.finite(e$n_total) && e$n_total > 0
  }, logical(1))
  entries <- entries[ok]
  if (length(entries) == 0) return(empty)

  labels <- vapply(entries, function(e) {
    g <- e$group
    if (is.null(g) || is.na(g) || !nzchar(g)) "Sample A" else as.character(g)
  }, character(1))

  # Group order follows first appearance, not alphabetical: the panel reads
  # as "the analyses you added, in the order you added them".
  out_groups <- lapply(unique(labels), function(g) {
    grp <- entries[labels == g]
    ns <- vapply(grp, function(e) as.numeric(e$n_total), numeric(1))
    ord <- order(ns, decreasing = TRUE)
    grp <- grp[ord]
    list(group = g, n = round_up_n(max(ns)), binding = grp[[1]], entries = grp)
  })

  list(
    total = round_up_n(sum(vapply(out_groups, function(g) g$n, numeric(1)))),
    groups = out_groups,
    n_entries = length(entries),
    n_groups = length(out_groups)
  )
}

#' How much a single analysis would under-recruit if planned alone
#'
#' The number the panel exists to show. Powering a study on one of its
#' analyses is the default failure mode this whole file is about, so the
#' shortfall is computed explicitly rather than left for the reader to
#' subtract.
#'
#' @param plan output of [study_plan_requirement()]
#' @param family character, the family key to evaluate
#' @return integer shortfall (0 if that family already sets the total, or
#'   if it is not in the plan)
#' @export
study_plan_shortfall <- function(plan, family) {
  if (!.is_plan(plan) || plan$n_entries == 0 || is.null(family)) return(0L)
  own <- NA_real_
  for (g in plan$groups) {
    for (e in g$entries) {
      if (identical(e$family, family)) own <- as.numeric(e$n_total)
    }
  }
  if (is.na(own)) return(0L)
  max(0L, round_up_n(plan$total - own))
}

#' One-line summary of a plan, for the generated report text
#'
#' Kept here rather than in report_text.R so the sentence and the
#' arithmetic that produces it cannot drift apart.
#'
#' @param plan output of [study_plan_requirement()]
#' @return character(1), or "" when there is nothing worth saying (fewer
#'   than two analyses in the plan)
#' @export
study_plan_report_sentence <- function(plan) {
  if (!.is_plan(plan) || plan$n_entries < 2) return("")

  fmt <- function(x) format(x, big.mark = ",")
  per_group <- vapply(plan$groups, function(g) {
    sprintf("%s participants for %s (set by %s, the most demanding of %s %s in that sample)",
            fmt(g$n), g$group, g$binding$title, length(g$entries),
            if (length(g$entries) == 1) "analysis" else "analyses")
  }, character(1))

  if (plan$n_groups == 1) {
    sprintf(paste0(" This study plans %d analyses on one sample, so the requirement",
                   " is the largest of them rather than their sum: %s."),
            plan$n_entries, per_group[[1]])
  } else {
    sprintf(paste0(" This study plans %d analyses across %d independent samples.",
                   " Within a sample the requirement is the largest of the",
                   " analyses run on it; across samples the requirements add:",
                   " %s. Total to recruit: N = %s."),
            plan$n_entries, plan$n_groups,
            paste(per_group, collapse = "; "), fmt(plan$total))
  }
}
