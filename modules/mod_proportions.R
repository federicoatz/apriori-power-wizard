## mod_proportions.R
## -----------------------------------------------------------------------
## Family module: comparison of two independent proportions (Cohen's h).
## Same internal sub-wizard pattern as mod_two_means.R.
## -----------------------------------------------------------------------

mod_proportions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    step_progress_ui(1, c("Design", "Parameters", "Effect size", "Results")),
    tabsetPanel(
      id = ns("wiz"), type = "hidden",
      tabPanelBody("design",
        h3(icon("percent"), " Step: Design structure"),
        p(class = "step-intro",
          "Two independent groups, each with a binary outcome (e.g.,",
          "conversion vs. no conversion), compared with a two-proportions z-test."),
        guided_box("In plain language",
          p("Two groups of DIFFERENT people, and a yes/no outcome on each",
            "person -- you're comparing what fraction of each group said",
            "'yes'. If instead the SAME people are measured twice (e.g.,",
            "before/after), you want 'McNemar's test' instead, which is",
            "built for that paired case.")
        ),
        div(class = "well well-info", icon("lightbulb"),
            strong(" Example: "), "comparing the click-through rate of two",
            "ad designs, or the completion rate of two versions of a survey."),
        wizard_nav_ui(ns, "design", show_back = FALSE)
      ),
      tabPanelBody("params",
        params_step_ui(ns),
        wizard_nav_ui(ns, "params")
      ),
      tabPanelBody("effect_size",
        effect_size_step_ui(
          ns,
          cohen_ui = radioButtons(ns("cohen_size"),
            help_tip("Benchmark", "h is Cohen's arcsine-based effect size for two proportions -- it doesn't have as intuitive a scale as a raw percentage-point gap, which is exactly why the raw-proportions SESOI option below is usually easier to reason about."),
            choices = c("Small (h = 0.20)" = "small",
                        "Medium (h = 0.50)" = "medium",
                        "Large (h = 0.80)" = "large"),
            selected = "medium"),
          sesoi_ui = tagList(
            radioButtons(ns("sesoi_mode"), "Specify the SESOI as:",
              choices = c("Two raw proportions" = "raw",
                          "Already standardized (Cohen's h)" = "standardized"),
              selected = "raw"),
            conditionalPanel(condition = sprintf("input['%s'] == 'raw'", ns("sesoi_mode")),
              fluidRow(
                column(6, numericInput(ns("sesoi_p1"),
                         help_tip("Proportion in group 1", "Your best current estimate of the baseline rate, e.g., your current conversion rate."),
                         value = 0.30, min = 0.001, max = 0.999, step = 0.01)),
                column(6, numericInput(ns("sesoi_p2"),
                         help_tip("Minimal proportion of interest in group 2", "The smallest rate in the second group that would still be meaningful to detect -- e.g., 'I'd want to know about even a 5-point improvement'."),
                         value = 0.45, min = 0.001, max = 0.999, step = 0.01))
              )
            ),
            conditionalPanel(condition = sprintf("input['%s'] == 'standardized'", ns("sesoi_mode")),
              numericInput(ns("sesoi_h"), "Smallest effect size of interest (Cohen's h)", value = 0.3, step = 0.05)
            )
          ),
          safeguard_metric_label = "Published Cohen's h"
        ),
        wizard_nav_ui(ns, "effect_size", next_label = "Compute")
      ),
      tabPanelBody("results",
        results_panel_ui(ns),
        wizard_nav_ui(ns, "results", show_next = FALSE)
      )
    )
  )
}

mod_proportions_server <- function(id) {
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

    effect_value <- reactive({
      branch <- input$es_branch %||% "sesoi"
      if (branch == "cohen") {
        unname(cohen_benchmarks("h")[input$cohen_size %||% "medium"])
      } else if (branch == "safeguard") {
        pub <- safe_numeric(input$sg_published_value, 0.0001, 5)
        n_pub <- safe_numeric(input$sg_published_n, 4, 1e6)
        req(pub, n_pub)
        sg <- safeguard_ci_h(pub, n1 = n_pub / 2, n2 = n_pub / 2,
                              conf_level = input$sg_conf_level %||% 0.80,
                              one_sided = identical(input$sg_one_sided, "one"))
        sg$h_safeguard
      } else {
        if (identical(input$sesoi_mode, "raw")) {
          req(input$sesoi_p1, input$sesoi_p2)
          abs(sesoi_proportions_to_h(input$sesoi_p1, input$sesoi_p2))
        } else {
          safe_numeric(input$sesoi_h, 0.0001, 5, 0.3)
        }
      }
    })

    effect_branch_details <- reactive({
      branch <- input$es_branch %||% "sesoi"
      p <- params()
      if (branch == "cohen") {
        list(label = input$cohen_size, value = sprintf("h = %.2f", effect_value()))
      } else if (branch == "safeguard") {
        pub <- safe_numeric(input$sg_published_value, 0.0001, 5)
        n_pub <- safe_numeric(input$sg_published_n, 4, 1e6)
        naive <- tryCatch(power_proportions_n(pub, p$alpha, p$power, p$tails, p$allocation_ratio)$n_total, error = function(e) NA)
        list(
          published_value = sprintf("h = %.3f", pub), published_n = round(n_pub),
          conf_level = input$sg_conf_level %||% 0.80,
          one_sided = identical(input$sg_one_sided, "one"),
          safeguard_value = sprintf("h = %.3f", effect_value()),
          n_naive = naive,
          n_safeguard = tryCatch(power_proportions_n(effect_value(), p$alpha, p$power, p$tails, p$allocation_ratio)$n_total, error = function(e) NA)
        )
      } else {
        list(description = sprintf("h = %.3f", effect_value()))
      }
    })

    result_r <- reactive({
      p <- params()
      req(effect_value())
      power_proportions_n(h = effect_value(), sig_level = p$alpha, power = p$power,
                           alternative = p$tails, allocation_ratio = p$allocation_ratio)
    })

    solve_n_fn <- function(sig_level, power, effect = NULL) {
      p <- params()
      power_proportions_n(h = effect %||% effect_value(), sig_level = sig_level, power = power,
                           alternative = p$tails, allocation_ratio = p$allocation_ratio)
    }
    effect_set_r <- reactive(effect_comparison_values(effect_value(), "magnitude"))

    n_summary_r <- reactive({
      res <- result_r()
      tagList(
        tags$p(icon("users"), sprintf(" Group 1: n = %d  |  Group 2: n = %d", res$n1, res$n2)),
        tags$p(icon("bullseye"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target))
      )
    })

    curve_extra_args_r <- reactive({
      p <- params()
      list(h = effect_value(), sig_level = p$alpha, alternative = p$tails)
    })
    n_solution_r <- reactive(result_r()$n1)

    sensitivity_fn <- function(n_max) {
      p <- params()
      sensitivity_min_effect("proportions", n_max = n_max, sig_level = p$alpha,
                              power = p$power, alternative = p$tails)
    }

    report_spec_r <- reactive({
      res <- result_r(); p <- params()
      list(
        analysis = "comparison of two independent proportions",
        design = "a between-subjects design with two independent groups and a binary outcome",
        effect_label_short = sprintf("an effect of h = %.3f", effect_value()),
        alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
        tails = p$tails,
        allocation = if (p$allocation_ratio == 1) "Groups were assumed to be equally sized." else sprintf("Groups followed an allocation ratio of %.2f (n2/n1).", p$allocation_ratio),
        n_total = res$n_total,
        n_per_group_txt = sprintf("n1 = %d, n2 = %d", res$n1, res$n2),
        effect_branch = input$es_branch %||% "sesoi",
        effect_branch_details = effect_branch_details(),
        method_key = "proportions"
      )
    })

    wire_results_server(input, output, session, family = "proportions",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn, effect_set_r = effect_set_r)
  })
}
