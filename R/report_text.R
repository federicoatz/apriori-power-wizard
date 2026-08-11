## report_text.R
## -----------------------------------------------------------------------
## Builds the paste-ready English report/pre-registration text block. Pure
## string templating: given a structured `spec` list (produced by the
## wizard's reactive state), returns a single character string.
## -----------------------------------------------------------------------

#' Citation strings for each method, used in the generated report
#' @export
method_citations <- function() {
  list(
    two_means = "Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Routledge. Computed with the R package 'pwr' (Champely, 2020).",
    anova_factorial = "Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Routledge. Computed via the standard closed-form noncentral-F relationship for fixed-effects factorial ANOVA.",
    regression = "Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.), ch. 9. Computed with the R package 'pwr' (Champely, 2020).",
    logistic = "Demidenko, E. (2007). Sample size determination for logistic regression revisited. Statistics in Medicine, 26(18), 3385-3397.",
    proportions = "Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.), ch. 6. Computed with the R package 'pwr' (Champely, 2020).",
    paired_t = "Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Routledge. Computed with the R package 'pwr' (Champely, 2020), pwr.t.test(type = \"paired\").",
    clustered_rct = "Donner, A., & Klar, N. (2000). Design and Analysis of Cluster Randomization Trials in Health Research. Arnold. Individually-randomized N (Cohen, 1988; R package 'pwr') inflated by the design effect 1 + (m-1)xICC.",
    chisq = "Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.), ch. 7. Computed with the R package 'pwr' (Champely, 2020), pwr.chisq.test().",
    correlation = "Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.), ch. 3. Computed with the R package 'pwr' (Champely, 2020), pwr.r.test().",
    mcnemar = "Connor, R. J. (1987). Sample size for testing differences in proportions for the paired-sample design. Biometrics, 43(1), 207-211. Large-sample closed-form approximation implemented directly in base R (no external dependency).",
    tost = "Schuirmann, D. J. (1987). A comparison of the two one-sided tests procedure and the power approach for assessing the equivalence of average bioavailability. Journal of Pharmacokinetics and Biopharmaceutics, 15(6), 657-680. Exact noncentral-t power (Phillips, 1990) implemented directly in base R (no external dependency).",
    ancova = "Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.), ch. 9. Computed with the R package 'pwr' (Champely, 2020), pwr.f2.test(), with f2 adjusted for the covariate following Borm, Fransen, & Lemmens (2007).",
    clustered_binary = "Donner, A., & Klar, N. (2000). Design and Analysis of Cluster Randomization Trials in Health Research. Arnold. Individually-randomized N for two proportions (Cohen, 1988, ch. 6; R package 'pwr', pwr.2p.test) inflated by the design effect 1 + (m-1)xICC.",
    clustered_cat = "Donner, A., & Klar, N. (2000). Design and Analysis of Cluster Randomization Trials in Health Research. Arnold. Individually-randomized chi-square N (Cohen, 1988, ch. 7; R package 'pwr', pwr.chisq.test) inflated by the design effect 1 + (m-1)xICC -- a first-order approximation to the Rao-Scott (1984) correction for clustered categorical data.",
    rm_anova = "Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.), ch. 8. Repeated-measures noncentral-F power with within-subject correlation and Greenhouse-Geisser nonsphericity correction, following the formulation documented for G*Power 3 (Faul, Erdfelder, Lang, & Buchner, 2007); implemented directly in base R (no external dependency).",
    wilcoxon = "Noether, G. E. (1987). Sample size determination for some common nonparametric tests. Journal of the American Statistical Association, 82(398), 645-647. Closed-form large-sample formula for the Wilcoxon-Mann-Whitney rank-sum test, implemented directly in base R (no external dependency).",
    survival = "Schoenfeld, D. A. (1983). Sample-size formula for the proportional-hazards regression model. Biometrics, 39(2), 499-503. Events-based closed-form power for the two-group log-rank test, implemented directly in base R (no external dependency)."
  )
}

#' Effect-size branch justification text
#' @keywords internal
.effect_size_branch_text <- function(branch, details) {
  switch(branch,
    cohen = sprintf(
      paste("The effect size was set using Cohen's (1988) conventional",
            "benchmark for a '%s' effect (%s). This benchmark is a",
            "field-agnostic heuristic rather than a domain-calibrated",
            "estimate, and readers should interpret the resulting sample",
            "size accordingly."),
      details$label, details$value
    ),
    safeguard = sprintf(
      paste("The effect size was derived from a previously published",
            "estimate (%s, from a study with N = %s) using the",
            "'safeguard power' procedure of Perugini, Gallucci, and",
            "Costantini (2014). The %s%% %s confidence interval around the",
            "published estimate was computed, and its lower bound (%s) was",
            "used as a conservative input that accounts for sampling",
            "uncertainty in the published estimate. This procedure does",
            "not itself correct publication bias or other selection",
            "mechanisms. Where the original effect has two groups, a total",
            "N is treated as equally allocated between them. For comparison, using the raw published",
            "estimate without correction would imply N = %s; the",
            "safeguard-corrected target is N = %s."),
      details$published_value, details$published_n,
      details$conf_level * 100, if (details$one_sided) "one-sided" else "two-sided",
      details$safeguard_value, details$n_naive, details$n_safeguard
    ),
    sesoi = sprintf(
      paste("The effect size represents the Smallest Effect Size Of",
            "Interest (SESOI) as directly specified by the researcher:",
            "%s. This value reflects the smallest effect considered",
            "theoretically or practically meaningful for this research",
            "question, independent of any prior study's precision."),
      details$description
    ),
    mcnemar_raw = sprintf(
      paste("The effect was specified directly via the two discordant-pair",
            "probabilities: P(positive under condition 1, negative under",
            "condition 2) = %s and P(negative under condition 1, positive",
            "under condition 2) = %s, an absolute difference of %s. Only",
            "these discordant pairs are informative for McNemar's test;",
            "pairs with the same outcome under both conditions do not",
            "contribute information about a within-pair change."),
      details$p10, details$p01, details$delta
    ),
    equivalence_margin = sprintf(
      paste("The equivalence margin was set directly by the researcher at",
            "%s (Cohen's d units): the largest difference between",
            "conditions that would still be considered practically",
            "equivalent. The design assumed a true difference of %s for",
            "planning purposes%s."),
      details$delta_eq, details$assumed_theta,
      if (isTRUE(details$theta_is_zero)) " (the conservative default: no true difference at all)" else ""
    ),
    hr_direct = sprintf(
      paste("The effect was specified directly as a hazard ratio of %s",
            "(arm 2 vs. arm 1)."),
      details$hr
    ),
    hr_safeguard = sprintf(
      paste("The effect size was derived from a previously published",
            "hazard ratio (%s, from a study with %s recorded events) using",
            "the 'safeguard power' procedure of Perugini, Gallucci, and",
            "Costantini (2014), adapted to the log-hazard-ratio scale. The",
            "%s%% %s confidence interval around the published log hazard",
            "ratio was computed, and its bound closer to 1 (%s) was used",
            "as a conservative input that accounts for sampling uncertainty",
            "in the published estimate. It does not itself correct publication",
            "bias or other selection mechanisms."),
      details$published_hr, details$published_events,
      details$conf_level * 100, if (details$one_sided) "one-sided" else "two-sided",
      details$safeguard_hr
    )
  )
}

#' Build the full paste-ready report text
#'
#' @param spec list with fields: analysis (character label), design
#'   (character description), n_total, n_per_group (named list or vector),
#'   alpha (the per-test alpha actually used to solve for N -- already
#'   Bonferroni-corrected if n_comparisons > 1), power_target,
#'   power_achieved, tails ("two.sided"/"one-sided"/"none"), allocation
#'   (character description), effect_branch ("cohen"/"safeguard"/"sesoi"),
#'   effect_branch_details (list, see .effect_size_branch_text),
#'   effect_metric, effect_value, method_key (one of
#'   names(method_citations())), and optionally n_comparisons/alpha_nominal
#'   (see wire_results_server(), which injects both automatically from the
#'   shared "Number of planned comparisons" input)
#' @export
build_report_text <- function(spec) {
  citation <- method_citations()[[spec$method_key]]
  if (identical(spec$effect_branch, "safeguard")) {
    citation <- paste(citation,
      "Effect size derived via the safeguard-power procedure of Perugini,",
      "Gallucci, and Costantini (2014).")
  }

  effect_txt <- .effect_size_branch_text(spec$effect_branch, spec$effect_branch_details)

  # Chi-square tests are inherently non-directional (the rejection region
  # is a single upper tail of the chi-square statistic, not a "two-tailed"
  # or "one-tailed" test in the usual sense), so `tails` is "none" for that
  # family and the parenthetical is dropped from the sentence entirely
  # rather than mislabeling it "two-tailed".
  has_tails <- !identical(spec$tails, "none")
  tails_txt <- if (has_tails) {
    if (spec$tails == "two.sided") "two-tailed" else "one-tailed"
  } else NULL
  tails_paren <- if (has_tails) sprintf(" (%s)", tails_txt) else ""

  # Multiplicity correction (see the "Advanced options" inputs in every
  # family's Parameters step): when the researcher plans more than one
  # comparison, the per-test alpha actually used for the sample-size solve
  # is alpha_nominal / k (Bonferroni, the default) or
  # 1 - (1-alpha_nominal)^(1/k) (Šidák). spec$alpha already IS that
  # corrected value (it flows straight into every power_*_n() solver), so
  # this is purely a matter of disclosing the correction in the text.
  # spec$mc_method may be absent in older saved projects; Bonferroni was
  # the only method then, so it is the backward-compatible default.
  has_mc <- !is.null(spec$n_comparisons) && !is.na(spec$n_comparisons) && spec$n_comparisons > 1
  alpha_clause <- if (has_mc) {
    mc_label <- if (identical(spec$mc_method, "sidak")) "Sidak-corrected" else "Bonferroni-corrected"
    sprintf("We set a family-wise alpha = %s, %s for %d planned comparisons to a per-test alpha = %s%s",
            format_stat(spec$alpha_nominal, 2), mc_label, spec$n_comparisons, format_stat(spec$alpha, 4), tails_paren)
  } else {
    sprintf("We set alpha = %s%s", format_stat(spec$alpha, 2), tails_paren)
  }

  # What the correction cost, not merely that it was applied. Reported for
  # the same reason the safeguard branch reports its naive comparison: a
  # single corrected figure conceals the size of the adjustment, and the
  # adjustment is the part a reader of the pre-registration cannot
  # reconstruct.
  multiplicity_txt <- if (has_mc && !is.null(spec$n_uncorrected) &&
                          !is.na(spec$n_uncorrected)) {
    sprintf(" Without that correction the same design would have required N = %s; the correction therefore accounts for %s of the total reported below.",
            spec$n_uncorrected,
            format(max(0, as.numeric(spec$n_total) - as.numeric(spec$n_uncorrected)),
                   big.mark = ","))
  } else ""

  # Attrition / exclusion inflation (see the shared "Expected attrition"
  # input in params_step_ui()). Purely a recruitment adjustment applied
  # AFTER the statistical solve -- it never changes alpha, power, or the
  # effect size -- so it is disclosed as its own sentence rather than
  # folded into the design description.
  has_attr <- !is.null(spec$attrition) && !is.na(spec$attrition) && spec$attrition > 0
  attrition_txt <- if (has_attr) {
    # For a design with structure to preserve -- clusters, cells, arms --
    # the over-recruitment has to be stated per unit, not only as a grand
    # total: "recruit 216" does not tell a reader (or the author, six
    # months later) that the 24 clusters stay fixed and each one grows.
    # The unstructured families set no unit and get the plain sentence.
    # Guarded on the per-unit figure actually accounting for the whole
    # total: "54 per arm across 2 arms" would be a false statement for an
    # unbalanced design, where the two arms differ by construction. Every
    # uniform unit (cluster, cell, equal group, balanced arm) passes.
    uniform_units <- !is.null(spec$n_recruit_per_unit) && !is.null(spec$n_recruit_units) &&
      isTRUE(spec$n_recruit_per_unit * spec$n_recruit_units == spec$n_recruit)
    unit_txt <- if (!is.null(spec$n_recruit_unit) &&
                      !identical(spec$n_recruit_unit, "participant") &&
                      uniform_units) {
      sprintf(" (%s per %s across %s %ss%s)",
              format(spec$n_recruit_per_unit, big.mark = ","), spec$n_recruit_unit,
              format(spec$n_recruit_units, big.mark = ","), spec$n_recruit_unit,
              # Worth spelling out only where a reader might otherwise
              # assume the extra participants bought extra units: the
              # number of clusters is what the power rests on, so it is
              # exactly the thing that must NOT absorb the inflation.
              if (identical(spec$n_recruit_unit, "cluster")) ", holding the number of clusters fixed" else "")
    } else ""
    sprintf(" Anticipating that %s%% of recruited participants will be lost to attrition or excluded before analysis, we plan to recruit N = %s%s in order to retain the required analytic sample.",
            format_stat(spec$attrition * 100, 0), spec$n_recruit, unit_txt)
  } else ""

  # Study plan (see R/study_plan.R). Present only when the user has
  # explicitly grouped two or more analyses; the sentence itself is built
  # there so the prose and the arithmetic cannot drift apart. It goes into
  # the report rather than living only on screen -- a panel that corrects
  # the researcher's sample size but leaves the manuscript quoting this
  # family's number alone would have fixed nothing that matters.
  study_plan_txt <- study_plan_report_sentence(spec$study_plan)

  lines <- c(
    sprintf("A priori power analysis (%s).", spec$analysis),
    "",
    sprintf(
      "We conducted an a priori power analysis to determine the minimum sample size required to detect %s in %s. %s and aimed for %s%% power (1 - beta = %s). %s",
      spec$effect_label_short, spec$design, alpha_clause,
      format_stat(spec$power_target * 100, 0), format_stat(spec$power_target, 2),
      spec$allocation
    ),
    "",
    effect_txt,
    "",
    sprintf(
      "This yields a required sample size of N = %s (%s), achieving %s%% power (%s).%s%s",
      spec$n_total, spec$n_per_group_txt,
      format_stat(spec$power_achieved * 100, 1), format_stat(spec$power_achieved, 4),
      multiplicity_txt, paste0(attrition_txt, study_plan_txt)
    ),
    "",
    sprintf("Method: %s", citation),
    "",
    sprintf("Computed with A Priori Power Analysis Wizard v%s (%s).",
            APP_VERSION, Sys.Date())
  )

  if (!is.null(spec$interaction_warning_shown) && isTRUE(spec$interaction_warning_shown)) {
    lines <- c(lines, "",
      paste("Note: the focal contrast for this analysis is an interaction",
            "effect. Interaction tests generally require substantially",
            "larger samples than the main effects that compose them; this",
            "was accounted for by computing power directly for the",
            "interaction term rather than for a related main effect."))
  }

  paste(lines, collapse = "\n")
}
