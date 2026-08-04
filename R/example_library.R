## example_library.R
## -----------------------------------------------------------------------
## A library of pre-filled example designs drawn from paradigms this app's
## audience actually runs -- public goods games, dictator and trust games,
## risk elicitation, Stroop-style within-subject tasks, pretest/posttest
## interventions.
##
## WHY: the hardest part of a first power analysis is not the arithmetic,
## it is not knowing what a complete, plausible setup even looks like --
## which family, which effect-size branch, what a defensible ICC or
## within-subject correlation is. An example that loads a whole design in
## one click answers that by demonstration rather than explanation, which
## is the same reason the decision helper and Guided mode exist.
##
## HOW: an example is nothing more than a hard-coded instance of the state
## object the Save/Load and share-link features already serialize --
## `list(family = , inputs = )`, keyed by un-prefixed input id (see
## capture_family_state() in R/project_state.R). Loading one therefore
## reuses apply_saved_state() unchanged, with no new plumbing at all.
## `wiz = "results"` lands the user directly on the finished answer; every
## step behind it is populated, so Back walks through the whole design.
##
## ON THE NUMBERS -- read this before treating any of them as a citation.
## These values are ILLUSTRATIVE and internally plausible, chosen to make
## each example self-consistent and recognizable. They are NOT empirical
## constants, not meta-analytic estimates, and not "typical values" for
## the paradigm in any defensible sense: effect sizes and intraclass
## correlations vary enormously across implementations, populations, and
## payoff structures. Every example's description says so, and the UI
## repeats it. Users are meant to load an example to see the SHAPE of a
## complete analysis, then replace every number with their own.
## -----------------------------------------------------------------------

#' The example library
#'
#' @return a named list of examples, each with `title`, `blurb` (one line
#'   of plain-language framing), `family`, and `state` (the
#'   apply_saved_state()-compatible list)
#' @export
example_library <- function() {
  list(
    public_goods = list(
      title = "Public goods game: does punishment raise contributions?",
      blurb = paste(
        "Contributions compared between a punishment and a no-punishment",
        "treatment, with participants in fixed matching groups of 4 -- so the",
        "matching group, not the participant, is the independent unit."
      ),
      family = "clustered_rct",
      state = list(family = "clustered_rct", inputs = list(
        setting = "lab", cluster_size = 4, icc = 0.10,
        alpha = 0.05, power = 0.80, tails = "two.sided", balanced = TRUE,
        n_comparisons = 1, attrition = 0,
        es_branch = "sesoi", sesoi_mode = "standardized", sesoi_d = 0.40,
        wiz = "results"
      ))
    ),

    cooperation_rate = list(
      title = "Cooperation rate by matching group",
      blurb = paste(
        "A yes/no outcome -- did the participant cooperate -- compared",
        "between two treatments, again with the matching group as the",
        "independent unit. The binary counterpart of the example above."
      ),
      family = "clustered_cat",
      state = list(family = "clustered_cat", inputs = list(
        outcome_type = "binary", setting = "lab", cluster_size = 4, icc = 0.10,
        alpha = 0.05, power = 0.80, tails = "two.sided", balanced = TRUE,
        n_comparisons = 1, attrition = 0,
        es_branch = "sesoi", sesoi_mode = "raw",
        sesoi_p1 = 0.35, sesoi_p2 = 0.55,
        wiz = "results"
      ))
    ),

    dictator = list(
      title = "Dictator game: does framing change the amount given?",
      blurb = paste(
        "Two independent groups, one continuous outcome (the amount sent),",
        "no interaction between participants -- the simplest complete design",
        "in the app, and a good first example to read."
      ),
      family = "two_means",
      state = list(family = "two_means", inputs = list(
        alpha = 0.05, power = 0.80, tails = "two.sided", balanced = TRUE,
        n_comparisons = 1, attrition = 0,
        es_branch = "sesoi", sesoi_mode = "standardized", sesoi_d = 0.35,
        wiz = "results"
      ))
    ),

    trust_game = list(
      title = "Trust game: amount returned (skewed outcome)",
      blurb = paste(
        "Amounts returned pile up at zero and at focal round numbers, so a",
        "rank-based test is the conventional choice -- this example shows",
        "the Mann-Whitney family and the probability-of-superiority effect",
        "size it uses."
      ),
      family = "wilcoxon",
      state = list(family = "wilcoxon", inputs = list(
        alpha = 0.05, power = 0.80, tails = "two.sided", balanced = TRUE,
        n_comparisons = 1, attrition = 0,
        es_branch = "sesoi", sesoi_mode = "psup", sesoi_psup = 0.62,
        wiz = "results"
      ))
    ),

    risk_elicitation = list(
      title = "Risk elicitation: safe choices under two incentive schemes",
      blurb = paste(
        "A bounded count of safe choices (as in a Holt-Laury price list)",
        "compared between two conditions, planned nonparametrically because",
        "the scale is short and lumpy rather than normal."
      ),
      family = "wilcoxon",
      state = list(family = "wilcoxon", inputs = list(
        alpha = 0.05, power = 0.80, tails = "two.sided", balanced = TRUE,
        n_comparisons = 1, attrition = 0.10,
        es_branch = "sesoi", sesoi_mode = "d", sesoi_d = 0.40,
        wiz = "results"
      ))
    ),

    stroop = list(
      title = "Stroop task: congruent vs. incongruent trials",
      blurb = paste(
        "The same participants measured under both conditions, so each",
        "person is their own control -- a within-subject comparison, which",
        "needs far fewer participants than the between-subjects version."
      ),
      family = "paired_t",
      state = list(family = "paired_t", inputs = list(
        alpha = 0.05, power = 0.80, tails = "two.sided",
        n_comparisons = 1, attrition = 0,
        es_branch = "sesoi", sesoi_mode = "standardized", sesoi_d = 0.50,
        wiz = "results"
      ))
    ),

    learning_rounds = list(
      title = "Learning across rounds, and whether it differs by treatment",
      blurb = paste(
        "Every participant plays several rounds, and the question is whether",
        "the change across rounds differs between two treatments -- the",
        "within x between interaction of a mixed design."
      ),
      family = "rm_anova",
      state = list(family = "rm_anova", inputs = list(
        effect_type = "interaction", m_measures = 4, n_groups = 2,
        rho = 0.60, eps = 1,
        alpha = 0.05, power = 0.80, n_comparisons = 1, attrition = 0,
        es_branch = "sesoi", sesoi_mode = "standardized", sesoi_f = 0.25,
        wiz = "results"
      ))
    ),

    pretest_posttest = list(
      title = "Pretest/posttest intervention, adjusting for the baseline",
      blurb = paste(
        "Two groups compared on a post-intervention score while adjusting",
        "for each participant's baseline -- ANCOVA. A well-correlated",
        "baseline is one of the cheapest ways to cut the sample size you need."
      ),
      family = "ancova",
      state = list(family = "ancova", inputs = list(
        k_groups = 2, r_cov = 0.60,
        alpha = 0.05, power = 0.80, n_comparisons = 1, attrition = 0.10,
        es_branch = "sesoi", sesoi_mode = "standardized", sesoi_f = 0.20,
        wiz = "results"
      ))
    ),

    choice_across_arms = list(
      title = "Which of three strategies? Choice distribution across arms",
      blurb = paste(
        "A categorical choice compared across three treatment arms, tested",
        "with a chi-square. Participants act independently here; if they",
        "interacted in groups, use the clustered version instead."
      ),
      family = "chisq",
      state = list(family = "chisq", inputs = list(
        chisq_design = "independence", n_rows = 3, n_cols = 3,
        alpha = 0.05, power = 0.80, n_comparisons = 1, attrition = 0,
        es_branch = "sesoi", sesoi_w = 0.25,
        wiz = "results"
      ))
    )
  )
}

#' Short, prominent disclaimer shown wherever the examples are offered
#'
#' Kept as a function rather than inline UI text so the same wording is
#' reused in every place the library is surfaced, and can never drift.
#' @export
example_library_caveat <- function() {
  paste(
    "The numbers in these examples are illustrative starting points, not",
    "empirical facts: effect sizes and intraclass correlations vary widely",
    "across implementations, populations, and payoff structures. Load one",
    "to see what a complete analysis looks like, then replace every value",
    "with an estimate justified for YOUR study."
  )
}
