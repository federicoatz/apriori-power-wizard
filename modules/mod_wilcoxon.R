## mod_wilcoxon.R
## -----------------------------------------------------------------------
## Family module: Wilcoxon-Mann-Whitney rank-sum test for two independent
## groups (see R/power_wilcoxon.R for Noether's 1987 formula and its
## Monte Carlo validation).
##
## This family reuses the standard three-branch effect-size step even
## though its native effect size is the probability of superiority
## P(X < Y) rather than Cohen's d: d converts to p exactly under normality
## (d_to_p_superiority()), so Cohen's conventions and the safeguard-power
## correction both remain meaningful, and the SESOI branch additionally
## lets the user state p directly for the case where no normal-based d is
## available or wanted.
## -----------------------------------------------------------------------

mod_wilcoxon_ui <- function(id) {
  ns <- NS(id)
  tagList(
    step_progress_ui(1, c("Design", "Parameters", "Effect size", "Results")),
    tabsetPanel(
      id = ns("wiz"), type = "hidden",
      tabPanelBody("design",
        h3(icon("ranking-star"), " Step: Design structure"),
        p(class = "step-intro",
          "Two independent groups are compared on an outcome that is ordinal,",
          "or continuous but not plausibly normal -- skewed earnings,",
          "bounded contributions, lumpy bids, Likert-type ratings. Instead of",
          "comparing means, the Wilcoxon-Mann-Whitney test compares ranks,",
          "asking whether values from one group tend to be larger than",
          "values from the other."),
        guided_box("In plain language",
          p("A t-test asks \"is the AVERAGE different?\" and leans on the",
            "assumption that the data are roughly normally distributed.",
            "This test instead asks \"if I pick one participant at random",
            "from each group, how often is one of them higher?\" It makes",
            "no assumption about the shape of the distribution, which is",
            "why it is the standard choice in experimental economics for",
            "outcomes like contributions, bids, and earnings that are",
            "typically skewed, bounded, or piled up at zero."),
          p("The trade-off is small: with genuinely normal data this test",
            "needs only about 5% more participants than a t-test. With the",
            "heavy-tailed or skewed data it is designed for, it often needs",
            strong(" fewer"), " -- so the cost of choosing it defensively is",
            "much lower than most people assume.")
        ),
        div(class = "well well-info", icon("lightbulb"),
            strong(" Example: "), "comparing average contributions in a public",
            "goods game between a punishment and a no-punishment treatment,",
            "where contributions cluster at 0 and at the endowment and are",
            "clearly not normal -- a rank test is the conventional choice."),
        div(class = "field-hint", icon("triangle-exclamation"),
            " If your participants interact with each other in fixed groups, the individual is not the independent unit -- use the \"Clustered: sessions / matching groups\" family instead, or apply this test at the matching-group level."),
        wizard_nav_ui(ns, "design", show_back = FALSE)
      ),
      tabPanelBody("params",
        params_step_ui(ns),
        wizard_nav_ui(ns, "params")
      ),
      tabPanelBody("effect_size",
        effect_size_step_ui(
          ns,
          cohen_ui = tagList(
            radioButtons(ns("cohen_size"),
              help_tip("Benchmark", "Cohen's conventions are defined for the mean-difference scale (d); the app converts your choice to the probability of superiority that the rank test actually works with, which is shown below."),
              choices = c("Small (d = 0.20)" = "small",
                          "Medium (d = 0.50)" = "medium",
                          "Large (d = 0.80)" = "large"),
              selected = "medium"),
            div(class = "field-hint", icon("circle-info"),
                " These are converted to P(X < Y) = 0.556, 0.638, and 0.714 respectively.")
          ),
          sesoi_ui = tagList(
            radioButtons(ns("sesoi_mode"), "Specify the SESOI as:",
              choices = c("Probability of superiority, P(X < Y)" = "psup",
                          "Standardized mean difference (Cohen's d), converted" = "d"),
              selected = "psup"),
            conditionalPanel(condition = sprintf("input['%s'] == 'psup'", ns("sesoi_mode")),
              numericInput(ns("sesoi_psup"),
                help_tip("Smallest P(X < Y) of interest",
                  "If you drew one participant at random from each group, how often would the second one have to score higher for the difference to matter to you? 0.5 means no difference at all; 0.56 is a small effect, 0.64 medium, 0.71 large, by analogy with Cohen's conventions."),
                value = 0.64, min = 0.001, max = 0.999, step = 0.01)
            ),
            conditionalPanel(condition = sprintf("input['%s'] == 'd'", ns("sesoi_mode")),
              numericInput(ns("sesoi_d"), "Smallest effect size of interest (Cohen's d)",
                           value = 0.5, min = 0.0001, step = 0.05),
              helpText("Converted to P(X < Y) assuming both groups are normal with equal variance; the conversion is exact under that assumption.")
            )
          ),
          safeguard_metric_label = "Published Cohen's d"
        ),
        uiOutput(ns("psup_summary")),
        wizard_nav_ui(ns, "effect_size", next_label = "Compute")
      ),
      tabPanelBody("results",
        results_panel_ui(ns),
        wizard_nav_ui(ns, "results", show_next = FALSE)
      )
    )
  )
}

mod_wilcoxon_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    steps <- c("design", "params", "effect_size", "results")
    go_step <- function(delta) {
      cur <- which(steps == input$wiz)
      new <- max(1, min(length(steps), cur + delta))
      updateTabsetPanel(session, "wiz", selected = steps[new])
    }
    wizard_nav_observers(input,
      next_steps = c("design", "params", "effect_size"),
      back_steps = c("params", "effect_size", "results"),
      go_step = go_step)
    observeEvent(input$use_90, updateNumericInput(session, "power", value = 0.90))
    wire_effect_size_helper(input, output, session)

    params <- reactive(read_params_step(input))

    # Always returned on the > 0.5 side: the test is symmetric in
    # |p - 0.5|, and the params step's own one-/two-tailed control is what
    # expresses directionality.
    effect_value <- reactive({
      branch <- input$es_branch %||% "sesoi"
      p <- if (branch == "cohen") {
        d_to_p_superiority(unname(cohen_benchmarks("d")[input$cohen_size %||% "medium"]))
      } else if (branch == "safeguard") {
        pub <- safe_numeric(input$sg_published_value, 0.0001, 5)
        n_pub <- safe_numeric(input$sg_published_n, 4, 1e6)
        req(pub, n_pub)
        sg <- safeguard_ci_d(pub, n1 = n_pub / 2, n2 = n_pub / 2,
                              conf_level = input$sg_conf_level %||% 0.80,
                              one_sided = identical(input$sg_one_sided, "one"))
        d_to_p_superiority(sg$d_safeguard)
      } else {
        if (identical(input$sesoi_mode %||% "psup", "d")) {
          d_to_p_superiority(safe_numeric(input$sesoi_d, 0.0001, 5, 0.5))
        } else {
          safe_numeric(input$sesoi_psup, 0.001, 0.999, 0.64)
        }
      }
      if (p < 0.5) 1 - p else p
    })

    valid_effect <- reactive({
      p <- effect_value()
      !is.null(p) && !is.na(p) && p > 0.5 && p < 1
    })

    output$psup_summary <- renderUI({
      p <- tryCatch(effect_value(), error = function(e) NA_real_)
      if (is.na(p) || p <= 0.5) {
        return(div(class = "well well-warning", icon("triangle-exclamation"),
          " The effect must correspond to a probability of superiority different from 0.5 -- exactly 0.5 means the two groups are indistinguishable and can never be detected at any sample size."))
      }
      div(class = "well well-result", icon("circle-check"),
          sprintf(" Planning for P(X < Y) = %.3f (equivalent to Cohen's d = %.3f under normality).",
                  p, p_superiority_to_d(p)))
    })

    effect_branch_details <- reactive({
      branch <- input$es_branch %||% "sesoi"
      p <- params()
      pv <- effect_value()
      if (branch == "cohen") {
        list(label = input$cohen_size,
             value = sprintf("P(X < Y) = %.3f (from d = %.2f)",
                              pv, unname(cohen_benchmarks("d")[input$cohen_size %||% "medium"])))
      } else if (branch == "safeguard") {
        pub <- safe_numeric(input$sg_published_value, 0.0001, 5)
        n_pub <- safe_numeric(input$sg_published_n, 4, 1e6)
        naive <- tryCatch(power_wilcoxon_n(d_to_p_superiority(pub), p$alpha, p$power, p$tails, p$allocation_ratio)$n_total,
                           error = function(e) NA)
        list(
          published_value = sprintf("d = %.3f (P(X < Y) = %.3f)", pub, d_to_p_superiority(pub)),
          published_n = round(n_pub),
          conf_level = input$sg_conf_level %||% 0.80,
          one_sided = identical(input$sg_one_sided, "one"),
          safeguard_value = sprintf("P(X < Y) = %.3f", pv),
          n_naive = naive,
          n_safeguard = tryCatch(power_wilcoxon_n(pv, p$alpha, p$power, p$tails, p$allocation_ratio)$n_total,
                                  error = function(e) NA)
        )
      } else {
        list(description = sprintf("P(X < Y) = %.3f (equivalent to d = %.3f under normality)",
                                    pv, p_superiority_to_d(pv)))
      }
    })

    result_r <- reactive({
      p <- params()
      req(valid_effect())
      power_wilcoxon_n(p_sup = effect_value(), sig_level = p$alpha, power = p$power,
                        alternative = p$tails, allocation_ratio = p$allocation_ratio)
    })

    solve_n_fn <- function(sig_level, power, effect = NULL) {
      p <- params()
      power_wilcoxon_n(p_sup = effect %||% effect_value(), sig_level = sig_level,
                        power = power, alternative = p$tails,
                        allocation_ratio = p$allocation_ratio)
    }
    # Scaled on the DISTANCE from 0.5 (the test's own null), not on p
    # itself -- halving p would be nonsense, halving |p - 0.5| is exactly
    # "half as strong an effect".
    effect_set_r <- reactive({
      p <- effect_value()
      if (is.null(p) || is.na(p) || p <= 0.5) return(stats::setNames(numeric(0), character(0)))
      shifted <- effect_comparison_values(p - 0.5, "magnitude")
      vals <- pmin(0.5 + unname(shifted), 0.999)
      stats::setNames(vals, names(shifted))
    })

    n_summary_r <- reactive({
      res <- result_r()
      tagList(
        tags$p(icon("users"), sprintf(" Group 1: n = %d  |  Group 2: n = %d  (total %d)", res$n1, res$n2, res$n_total)),
        tags$p(icon("ranking-star"), sprintf(" P(X < Y) = %.3f (equivalent to d = %.3f under normality)", res$p_sup, res$d_equivalent)),
        tags$p(icon("bullseye"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target))
      )
    })

    curve_extra_args_r <- reactive({
      p <- params()
      list(p_sup = effect_value(), sig_level = p$alpha, alternative = p$tails,
           allocation_ratio = p$allocation_ratio)
    })
    n_solution_r <- reactive(result_r()$n1)

    sensitivity_fn <- function(n_max) {
      p <- params()
      sensitivity_min_effect("wilcoxon", n_max = round(n_max), sig_level = p$alpha,
                              power = p$power, alternative = p$tails,
                              allocation_ratio = p$allocation_ratio)
    }

    report_spec_r <- reactive({
      res <- result_r(); p <- params()
      list(
        analysis = "Wilcoxon-Mann-Whitney rank-sum test (two independent groups)",
        design = "a between-subjects design with two independent groups, analysed with a rank-based (nonparametric) test rather than a t-test",
        effect_label_short = sprintf("an effect corresponding to P(X < Y) = %.3f (equivalent to Cohen's d = %.3f under normality)",
                                      res$p_sup, res$d_equivalent),
        alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
        tails = p$tails,
        allocation = if (isTRUE(all.equal(p$allocation_ratio, 1))) {
          "Groups were assumed to be equally sized."
        } else {
          sprintf("Groups followed an allocation ratio of %.2f (n2/n1).", p$allocation_ratio)
        },
        n_total = res$n_total,
        n_per_group_txt = sprintf("n1 = %d, n2 = %d", res$n1, res$n2),
        effect_branch = input$es_branch %||% "sesoi",
        effect_branch_details = effect_branch_details(),
        method_key = "wilcoxon"
      )
    })

    wire_results_server(input, output, session, family = "wilcoxon",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn, effect_set_r = effect_set_r)
  })
}
