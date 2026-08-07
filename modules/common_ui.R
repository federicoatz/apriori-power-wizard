## common_ui.R
## -----------------------------------------------------------------------
## Reusable UI fragments shared by every family module (mod_two_means.R,
## mod_anova_factorial.R, mod_regression.R, mod_logistic.R,
## mod_proportions.R). Kept as plain functions (NOT nested Shiny modules)
## so each family module can read the resulting inputs directly through
## its own `ns()` / `input$` without dealing with nested-namespace
## indirection. This file contains ONLY view code -- no calculation logic
## lives here.
##
## Every input label in this file can optionally carry a `help_tip()` --
## a small "?" icon that reveals a plain-language explanation on hover.
## These are aimed squarely at users who have never run a power analysis
## before and don't yet know terms like "alpha" or "Cohen's d" by heart.
## -----------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

#' A label with an inline "?" tooltip explaining the term in plain language
#'
#' @param label character/tagList, the visible input label
#' @param tip character, plain-language explanation shown on hover
#' @export
help_tip <- function(label, tip) {
  tagList(
    label,
    tooltip(
      icon("circle-question", class = "help-icon"),
      tip,
      placement = "right"
    )
  )
}

#' A "Guided mode" explanation box: always rendered in the page, but only
#' shown when the visitor has switched Guided mode on (the toggle button
#' in app.R sets `data-guided="true"` on the root element; see the
#' `.guided-explain` rule in styles.css). Purely a client-side CSS show/
#' hide -- there is no reactive dependency here, so this works instantly,
#' with no re-render, even for steps and families already visited earlier
#' in the session.
#'
#' @param title character, short heading for the box (e.g. "In plain
#'   language")
#' @param ... paragraphs/tagList content, the actual explanation
#' @export
guided_box <- function(title, ...) {
  div(class = "guided-explain",
    div(class = "guided-explain-title", icon("graduation-cap"), title),
    ...
  )
}

#' Statistical-parameters step UI (alpha, power, tails, allocation)
#'
#' @param ns namespace function from the calling module
#' @export
params_step_ui <- function(ns) {
  tagList(
    h3(icon("sliders"), " Step: Statistical parameters"),
    p(class = "step-intro",
      "These four settings define how strict your test will be and how confident",
      "you want to be in detecting a real effect. If you're unsure, the defaults",
      "below are sensible starting points used across most of the social and",
      "behavioral sciences."),
    guided_box("In plain language",
      p("Every study risks two kinds of mistakes: claiming an effect that",
        "isn't really there (a ", strong("false positive"), "), or missing",
        "one that genuinely is (a ", strong("false negative"), "). ",
        strong("Alpha"), " is how much false-positive risk you're willing",
        "to accept -- 0.05 means you're OK with a 5% chance of that",
        "happening purely by chance. ", strong("Power"), " is the flip",
        "side: it's your chance of actually detecting the effect if it's",
        "really there. The sample size this wizard computes is simply",
        "the smallest N that hits your chosen power at your chosen alpha."),
      p("You very likely don't need to touch anything under",
        strong("Advanced options"), " below -- those exist for specific,",
        "less common situations, and the defaults are the right choice",
        "for the vast majority of studies.")
    ),
    fluidRow(
      column(6,
        numericInput(ns("alpha"),
                     help_tip("Significance level (alpha)",
                       "The chance of concluding there's an effect when there actually isn't one (a 'false positive'). 0.05 means a 5% chance -- the standard convention in most journals."),
                     value = 0.05, min = 0.0001, max = 0.5, step = 0.01)
      ),
      column(6,
        numericInput(ns("power"),
                     help_tip("Desired power (1 - beta)",
                       "The chance of correctly detecting the effect if it truly exists. Higher power means a larger sample, but also a safer study: 0.80 means an 80% chance of finding a real effect; 0.90 is stricter and increasingly expected by reviewers."),
                     value = 0.80, min = 0.01, max = 0.999, step = 0.01),
        div(class = "field-hint",
            "Default is 0.80. Many journals and reviewers now expect 0.90 for confirmatory studies."),
        actionLink(ns("use_90"), tagList(icon("arrow-up"), " Use 0.90 instead"))
      )
    ),
    div(class = "field-hint",
        "The defaults below (two-tailed, equal-sized groups) are the right choice for almost every study. Only open ",
        strong("Advanced options"), " if you have a specific reason to change them."),
    accordion(
      id = ns("params_advanced_acc"), open = FALSE,
      accordion_panel(
        title = tagList(icon("sliders"), " Advanced options: test direction, allocation ratio & multiple comparisons"),
        value = "advanced",
        fluidRow(
          column(6,
            radioButtons(ns("tails"),
                         help_tip("Test direction",
                           "Two-tailed tests for 'is there any difference' (in either direction) and are the safer, more common default. One-tailed tests only for 'is the effect specifically positive/negative' and should only be used when you can fully rule out the opposite direction in advance."),
                         choices = c("Two-tailed (recommended default)" = "two.sided",
                                     "One-tailed: greater" = "greater",
                                     "One-tailed: less" = "less"),
                         selected = "two.sided")
          ),
          column(6,
            checkboxInput(ns("balanced"),
                          help_tip("Balanced allocation (equal group sizes)",
                            "Balanced designs (equal-sized groups) are almost always the most statistically efficient choice. Only uncheck this if a practical constraint forces unequal group sizes (e.g., a rare treatment condition)."),
                          value = TRUE),
            conditionalPanel(
              condition = sprintf("!input['%s']", ns("balanced")),
              numericInput(ns("alloc_ratio"), "Allocation ratio (n2 / n1)",
                           value = 1, min = 0.1, max = 10, step = 0.1)
            )
          )
        ),
        fluidRow(
          column(6,
            numericInput(ns("attrition"),
                         help_tip("Expected attrition / exclusion rate",
                           "The proportion of recruited participants you expect to LOSE before analysis -- dropouts in a multi-session or online study, plus anyone excluded for failing comprehension or attention checks. The app reports both the N your analysis needs and the larger N you should actually recruit to end up with it: recruit N = analysis N / (1 - this rate). Leave at 0 if you expect no loss."),
                         value = 0, min = 0, max = 0.95, step = 0.05)
          ),
          column(6,
            div(class = "field-hint", icon("circle-info"),
                " Attrition does not change the statistical calculation -- it only tells you how many people to recruit so that enough of them survive to analysis.")
          )
        ),
        fluidRow(
          column(6,
            numericInput(ns("n_comparisons"),
                         help_tip("Number of planned comparisons",
                           "If this is one of several comparisons planned in the same study (e.g., testing 4 outcomes, or 4 pairwise contrasts), each additional comparison inflates the chance of a false positive somewhere across the whole set. Set this to the TOTAL number of planned comparisons: the alpha above is treated as the family-wise rate, and the per-test alpha actually used below is reduced accordingly (by the method chosen on the right). Leave at 1 if this is your only planned comparison. Important: this corrects the alpha used here, but the sample size it produces still covers only THIS analysis. If your other planned comparisons are different tests, work each one out in its own family and use the study-plan panel in the Results step to combine them -- the sample you need is the largest of them, not this one."),
                         value = 1, min = 1, max = 100, step = 1)
          ),
          column(6,
            radioButtons(ns("mc_method"),
                         help_tip("Correction method",
                           "Bonferroni (alpha / k) is the simplest and most conservative choice, and the one reviewers recognize on sight. Sidak (1 - (1-alpha)^(1/k)) is exact when the comparisons are independent and slightly less demanding -- it never asks for a larger sample than Bonferroni. Sequential procedures such as Holm cannot be planned for with a single per-test alpha, so for an a priori calculation Bonferroni is their conservative planning bound."),
                         choices = c("Bonferroni (default)" = "bonferroni",
                                     "Šidák (independent comparisons)" = "sidak"),
                         selected = "bonferroni"),
            div(class = "field-hint", icon("circle-info"),
                " With more than one comparison, the alpha entered above is corrected before being used to solve for N, and the correction is disclosed in the report text below.")
          )
        )
      )
    )
  )
}

#' Read the params step inputs into a plain list (call from the parent
#' module's server, e.g. `params <- read_params_step(input)`)
#'
#' `alpha` is already multiplicity-corrected when `n_comparisons` > 1 --
#' Bonferroni (alpha_nominal / k) by default, or Šidák
#' (1 - (1-alpha_nominal)^(1/k)) when the user selects it -- and every
#' family's solver consumes `alpha` directly, so the correction applies
#' automatically with no changes needed in any individual family module.
#' `alpha_nominal`, `n_comparisons`, and `mc_method` are carried alongside
#' it purely so [wire_results_server()] can disclose the correction in the
#' generated report text.
#' @export
read_params_step <- function(input) {
  alpha_nominal <- safe_numeric(input$alpha, 0.0001, 0.5, 0.05)
  n_comparisons <- max(1L, as.integer(safe_numeric(input$n_comparisons, 1, 100, 1)))
  mc_method <- if (identical(input$mc_method, "sidak")) "sidak" else "bonferroni"
  alpha_per <- if (n_comparisons > 1L && mc_method == "sidak") {
    1 - (1 - alpha_nominal)^(1 / n_comparisons)
  } else {
    alpha_nominal / n_comparisons
  }
  list(
    alpha = alpha_per,
    alpha_nominal = alpha_nominal,
    n_comparisons = n_comparisons,
    mc_method = mc_method,
    attrition = safe_numeric(input$attrition, 0, 0.95, 0),
    power = safe_numeric(input$power, 0.01, 0.999, 0.80),
    tails = input$tails %||% "two.sided",
    allocation_ratio = if (isTRUE(input$balanced)) 1 else safe_numeric(input$alloc_ratio, 0.1, 10, 1)
  )
}

#' Effect-size step UI: three selectable branches (Cohen / previous study /
#' SESOI). `metric_label` and `metric_hint` customize wording per family;
#' the actual numeric inputs for each branch are family-specific and
#' passed in as `cohen_ui`, `sesoi_ui` tagLists (safeguard branch UI is
#' generic and built here).
#'
#' @param ns namespace function
#' @param cohen_ui tagList, Cohen's-convention branch inputs (radio choices)
#' @param sesoi_ui tagList, SESOI branch inputs
#' @param safeguard_metric_label character/tagList, label for the published
#'   effect size input (e.g. "Published Cohen's d")
#' @param safeguard_pre_ui tagList or NULL, family-specific inputs rendered
#'   INSIDE the safeguard branch, above the generic published-value/N pair
#'   -- used by families whose published statistic needs interpreting
#'   before the generic inputs make sense (e.g. Wilcoxon's choice of
#'   p / U / z / d as the reported metric)
#' @param safeguard_hint character or NULL, a field-hint rendered under the
#'   safeguard inputs. The default covers the magnitude-scale families
#'   (d, h, w, f), whose input is clamped positive: a negatively-signed
#'   published effect is entered as its absolute value, with direction
#'   carried by the test-direction setting. Families where the SIGN of the
#'   published estimate is itself meaningful and accepted (correlation,
#'   regression) must override this, and families with fully custom
#'   safeguard UI (survival) never see it.
#' @export
effect_size_step_ui <- function(ns, cohen_ui, sesoi_ui,
                                 safeguard_metric_label = "Published effect size",
                                 safeguard_pre_ui = NULL,
                                 safeguard_hint = paste(
                                   "If the published effect was reported with a negative sign,",
                                   "enter its size without the sign: on this magnitude scale the",
                                   "direction of your hypothesis is set by 'Test direction' in the",
                                   "Parameters step, not by the sign of the effect size.")) {
  tagList(
    h3(icon("bullseye"), " Step: Effect size"),
    p(class = "step-intro",
      "This is the single most important choice in the whole calculation --",
      "it's your assumption about how large the effect you're looking for",
      "actually is. Pick whichever of the three options below best matches",
      "what you know. If you genuinely have no information at all, start",
      "with Cohen's conventions, but treat the result as a rough starting point."),
    guided_box("In plain language",
      p("An ", strong("effect size"), " is just a number describing how",
        "big the difference, relationship, or pattern you're looking for",
        "actually is, on a scale that doesn't depend on the specific",
        "units of your outcome. A tiny effect needs a huge sample to",
        "detect reliably; a large, obvious effect needs far fewer",
        "participants. This is the single input that matters most for",
        "your sample size, and it's also the one number in this whole",
        "wizard you can't get from a formula -- it has to come from what",
        "you already know (a previous study) or what you've decided",
        "matters (a SESOI), with Cohen's generic benchmarks only as a",
        "last resort when you truly have neither.")
    ),

    accordion(
      id = ns("es_helper_acc"), open = FALSE,
      accordion_panel(
        title = tagList(icon("compass"), " Not sure which one to pick? Answer 2 quick questions"),
        value = "es_helper",
        radioButtons(ns("es_helper_prev"),
          "1. Is there a previous study (yours, a pilot, or someone else's) that measured a similar effect?",
          choices = c("Yes, and I know its effect size and its N" = "yes",
                      "No" = "no"),
          selected = character(0)),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'no'", ns("es_helper_prev")),
          radioButtons(ns("es_helper_sesoi"),
            "2. Do you know the smallest effect that would still be practically or theoretically meaningful, in the outcome's own real-world units?",
            choices = c("Yes" = "yes", "No, I have no information at all" = "no"),
            selected = character(0))
        ),
        uiOutput(ns("es_helper_recommendation"))
      )
    ),

    h4(class = "es-section-title", "1. How do you want to determine the effect size?"),
    div(class = "choice-card-grid",
      radioButtons(ns("es_branch"), NULL,
        choiceNames = list(
          div(class = "choice-card",
            icon("book", class = "choice-icon"),
            div(strong("Cohen's (1988) conventions"),
                p(class = "choice-desc", "Generic small/medium/large benchmarks -- use only as a last resort."))),
          div(class = "choice-card",
            icon("shield-halved", class = "choice-icon"),
            div(strong("From a previous study"),
                p(class = "choice-desc", "A published estimate you want to build on, corrected for its imprecision (safeguard power)."))),
          div(class = "choice-card",
            icon("crosshairs", class = "choice-icon"),
            div(strong("Smallest effect of interest"),
                p(class = "choice-desc", "Recommended: you state directly the smallest effect that would matter to you (SESOI).")))
        ),
        choiceValues = c("cohen", "safeguard", "sesoi"),
        selected = "sesoi")
    ),

    h4(class = "es-section-title", "2. Enter the values for that choice"),
    div(class = "es-branch-detail",
      conditionalPanel(
        condition = sprintf("input['%s'] == 'cohen'", ns("es_branch")),
        div(class = "well well-warning",
            icon("triangle-exclamation"), strong(" Caution: "), cohen_warning_text()),
        cohen_ui
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'safeguard'", ns("es_branch")),
        div(class = "well well-info", icon("circle-info"), " ", safeguard_power_explainer()),
        safeguard_pre_ui,
        fluidRow(
          column(6, numericInput(ns("sg_published_value"), safeguard_metric_label, value = NA)),
          column(6, numericInput(ns("sg_published_n"),
                   help_tip("Original study's total N", "The total sample size (both groups combined, if applicable) of the study the published effect came from -- check its Methods section."),
                   value = NA, min = 4))
        ),
        if (!is.null(safeguard_hint)) div(class = "field-hint", icon("circle-info"), " ", safeguard_hint),
        fluidRow(
          column(6, sliderInput(ns("sg_conf_level"),
                   help_tip("Confidence level for the safeguard bound", "How conservative the correction should be. 80% is the default recommended by Perugini et al. (2014); higher values are more cautious and require a larger sample."),
                   min = 0.5, max = 0.99, value = 0.80, step = 0.01)),
          column(6, radioButtons(ns("sg_one_sided"), "Interval type",
                                  choices = c("One-sided (recommended, Perugini et al. 2014)" = "one",
                                              "Two-sided" = "two"),
                                  selected = "one"))
        )
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'sesoi'", ns("es_branch")),
        div(class = "well well-info", icon("lightbulb"),
            " Ask yourself: ", em("\"What is the smallest effect that, if true, would still be worth acting on, publishing, or caring about?\""),
            " That number -- not the effect you secretly hope to find -- is the SESOI."),
        sesoi_ui
      )
    )
  )
}

#' Server-side wiring for the effect-size mini decision-helper embedded in
#' [effect_size_step_ui()] (the "Not sure which one to pick?" accordion).
#' Generic across every family, since the three branch values ("cohen",
#' "safeguard", "sesoi") are identical everywhere -- call once from each
#' family module's `moduleServer()` body, e.g.
#' `wire_effect_size_helper(input, output, session)`.
#'
#' @param input,output,session standard moduleServer args of the CALLING
#'   family module
#' @export
wire_effect_size_helper <- function(input, output, session) {
  recommended_branch <- reactive({
    if (identical(input$es_helper_prev, "yes")) {
      "safeguard"
    } else if (identical(input$es_helper_prev, "no")) {
      switch(input$es_helper_sesoi %||% "", yes = "sesoi", no = "cohen", NULL)
    } else {
      NULL
    }
  })

  output$es_helper_recommendation <- renderUI({
    branch <- recommended_branch()
    if (is.null(branch)) {
      return(helpText("Answer the question(s) above to get a recommendation."))
    }
    label <- switch(branch,
      safeguard = "From a previous study (safeguard power)",
      sesoi = "Smallest effect size of interest (SESOI)",
      cohen = "Cohen's (1988) conventions (last resort)")
    div(class = "well well-result",
        p(strong("Recommended: "), label),
        actionButton(session$ns("es_helper_use"), tagList(icon("check"), " Use this"), class = "btn-success"))
  })

  observeEvent(input$es_helper_use, {
    branch <- recommended_branch()
    req(branch)
    updateRadioButtons(session, "es_branch", selected = branch)
  })
}

#' Standard "Back / Next" navigation row for a family module's internal
#' sub-wizard
#'
#' @param ns namespace function
#' @param step character, a short unique key for the CALLING sub-step
#'   (e.g. "design", "params", "effect_size", "results"). Required so
#'   that every step's Back/Next buttons get distinct input IDs --
#'   because every sub-step stays mounted in the DOM simultaneously
#'   (see the hidden tabsetPanel pattern in each mod_*.R), giving two
#'   different steps' buttons the SAME id would create duplicate HTML
#'   ids for genuinely different controls. Pair with
#'   [wizard_nav_observers()] on the server side.
#' @param show_back,show_next logical, whether to render each button
#' @param next_label character, label for the "Next" button
#' @export
wizard_nav_ui <- function(ns, step, show_back = TRUE, show_next = TRUE,
                           next_label = "Next") {
  div(class = "wizard-nav",
    fluidRow(
      column(6, if (show_back) actionButton(ns(paste0("nav_back_", step)), tagList(icon("arrow-left"), " Back"), class = "btn-outline-secondary")),
      column(6, style = "text-align: right;",
             if (show_next) actionButton(ns(paste0("nav_next_", step)), tagList(next_label, " ", icon("arrow-right")), class = "btn-primary"))
    )
  )
}

#' Wire the Back/Next buttons produced by every [wizard_nav_ui()] call in a
#' module to a single `go_step()` stepper function
#'
#' @param input standard moduleServer `input`
#' @param next_steps character vector of step keys that have a visible
#'   "Next" button (i.e., every step except the last)
#' @param back_steps character vector of step keys that have a visible
#'   "Back" button (i.e., every step except the first)
#' @param go_step function(delta) advancing/retreating the module's
#'   internal step tracker (see each mod_*.R's `go_step()`)
#' @export
wizard_nav_observers <- function(input, next_steps, back_steps, go_step) {
  lapply(next_steps, function(s) {
    observeEvent(input[[paste0("nav_next_", s)]], go_step(1), ignoreInit = TRUE)
  })
  lapply(back_steps, function(s) {
    observeEvent(input[[paste0("nav_back_", s)]], go_step(-1), ignoreInit = TRUE)
  })
  invisible(NULL)
}

#' Step progress indicator
#' @export
step_progress_ui <- function(current, labels) {
  icons <- c("pen-ruler", "sliders", "bullseye", "chart-simple")
  n <- length(labels)
  items <- lapply(seq_len(n), function(i) {
    cls <- if (i < current) "step-done" else if (i == current) "step-active" else "step-pending"
    ic <- if (i < current) icon("check") else icon(icons[[min(i, length(icons))]])
    tags$span(class = paste("step-indicator", cls), ic, sprintf(" %d. %s", i, labels[i]))
  })
  div(class = "step-progress", items)
}
