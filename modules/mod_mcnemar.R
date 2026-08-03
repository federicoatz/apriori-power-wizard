## mod_mcnemar.R
## -----------------------------------------------------------------------
## Family module: McNemar's test for paired binary outcomes (the same
## participants classified yes/no under two conditions). The effect is
## genuinely two-dimensional (the two discordant-pair probabilities), so
## unlike every other family this one does NOT reuse the generic
## Cohen's-convention / safeguard-power / SESOI effect-size step -- there
## is no single scalar "effect size" here to branch on, only a direct,
## concrete pair of numbers a beginner can reason about.
## -----------------------------------------------------------------------

mod_mcnemar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    step_progress_ui(1, c("Design", "Parameters", "Effect size", "Results")),
    tabsetPanel(
      id = ns("wiz"), type = "hidden",
      tabPanelBody("design",
        h4(icon("arrow-right-arrow-left"), " Step: Design structure"),
        p(class = "step-intro",
          "The SAME participants are classified on a yes/no outcome under",
          "two conditions (e.g., before/after, or two products each",
          "person tries). McNemar's test asks whether people are more",
          "likely to switch in one direction than the other."),
        guided_box("In plain language",
          p("This is the paired counterpart to 'Two proportions': same",
            "yes/no outcome idea, but measured on the SAME people twice",
            "instead of two separate groups. Only people who actually",
            "CHANGE their answer between the two conditions are",
            "informative -- people who answer the same way both times",
            "don't tell you anything about which direction the effect",
            "goes.")
        ),
        div(class = "well well-info", icon("lightbulb"),
            strong(" Example: "), "each participant states whether they",
            "support a policy before and after reading an argument --",
            "testing whether switches from 'against' to 'for' are more",
            "common than the reverse."),
        div(class = "field-hint", icon("circle-check"),
            " Only participants whose answer CHANGES between the two",
            "conditions are informative; people who answer the same way",
            "both times don't affect the result."),
        wizard_nav_ui(ns, "design", show_back = FALSE)
      ),
      tabPanelBody("params",
        params_step_ui(ns),
        div(class = "field-hint",
            "Test direction and allocation ratio are not used here: McNemar's test as implemented is two-sided, and there is one paired sample, not two groups to allocate between."),
        wizard_nav_ui(ns, "params")
      ),
      tabPanelBody("effect_size",
        h4(icon("bullseye"), " Step: Effect size"),
        p(class = "step-intro",
          "For a paired yes/no design, only the participants who switch",
          "between conditions -- the discordant pairs -- carry",
          "information. State how common you expect each direction of",
          "switching to be, out of ALL participants."),
        div(class = "well well-info", icon("lightbulb"),
            " If you have no prior data to draw on, a modest asymmetry",
            "(e.g., 25% switch one way, 10% switch the other) is a",
            "reasonable starting assumption -- adjust based on what you",
            "know about your specific outcome."),
        fluidRow(
          column(6, numericInput(ns("p10"),
            help_tip("P(condition 1 = yes, condition 2 = no)",
              "The proportion of ALL participants (not just those who switch) you expect to answer yes under condition 1 and no under condition 2."),
            value = 0.25, min = 0.001, max = 0.99, step = 0.01)),
          column(6, numericInput(ns("p01"),
            help_tip("P(condition 1 = no, condition 2 = yes)",
              "The proportion of ALL participants you expect to answer no under condition 1 and yes under condition 2 -- typically the direction of the effect you're trying to detect."),
            value = 0.10, min = 0.001, max = 0.99, step = 0.01))
        ),
        uiOutput(ns("mcnemar_summary")),
        wizard_nav_ui(ns, "effect_size", next_label = "Compute")
      ),
      tabPanelBody("results",
        results_panel_ui(ns),
        wizard_nav_ui(ns, "results", show_next = FALSE)
      )
    )
  )
}

mod_mcnemar_server <- function(id) {
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

    params <- reactive(read_params_step(input))

    p10_r <- reactive(safe_numeric(input$p10, 0.0001, 0.999))
    p01_r <- reactive(safe_numeric(input$p01, 0.0001, 0.999))

    mcnemar_valid <- reactive({
      p10 <- p10_r(); p01 <- p01_r()
      !is.na(p10) && !is.na(p01) && (p10 + p01) < 1 && !isTRUE(all.equal(p10, p01))
    })

    output$mcnemar_summary <- renderUI({
      p10 <- p10_r(); p01 <- p01_r()
      if (is.na(p10) || is.na(p01)) {
        return(helpText("Enter both proportions above."))
      }
      if (p10 + p01 >= 1) {
        return(div(class = "well well-warning", icon("triangle-exclamation"),
          " P(condition 1 = yes, condition 2 = no) + P(condition 1 = no, condition 2 = yes) must be less than 1 -- together they can't exceed the whole sample."))
      }
      if (isTRUE(all.equal(p10, p01))) {
        return(div(class = "well well-warning", icon("triangle-exclamation"),
          " These two proportions are equal -- there is no difference to detect (an infinite sample would be needed)."))
      }
      div(class = "well well-result", icon("circle-check"),
          sprintf(" Expected switchers: %.1f%% of all participants. Difference to detect: |p10 - p01| = %.3f.",
                  (p10 + p01) * 100, abs(p10 - p01)))
    })

    result_r <- reactive({
      p <- params()
      req(mcnemar_valid())
      power_mcnemar_n(p10 = p10_r(), p01 = p01_r(), sig_level = p$alpha, power = p$power)
    })

    solve_n_fn <- function(sig_level, power) {
      power_mcnemar_n(p10 = p10_r(), p01 = p01_r(), sig_level = sig_level, power = power)
    }

    n_summary_r <- reactive({
      res <- result_r()
      tagList(
        tags$p(icon("users"), sprintf(" Total pairs N = %d (%.1f%% expected discordant)", res$n_total, res$p_discordant * 100)),
        tags$p(icon("bullseye"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target))
      )
    })

    curve_extra_args_r <- reactive({
      list(p10 = p10_r(), p01 = p01_r(), sig_level = params()$alpha)
    })
    n_solution_r <- reactive(result_r()$n_total)

    sensitivity_fn <- function(n_max) {
      p <- params()
      sensitivity_min_effect("mcnemar", n_max = n_max, p_discordant = p10_r() + p01_r(),
                              sig_level = p$alpha, power = p$power)
    }

    report_spec_r <- reactive({
      res <- result_r(); p <- params()
      list(
        analysis = "McNemar's test for paired binary outcomes",
        design = "a paired design in which the same participants are classified on a yes/no outcome under two conditions",
        effect_label_short = sprintf("a discordant-pair difference of |p10 - p01| = %.3f", res$delta),
        alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
        tails = "two.sided",
        allocation = "There is one paired sample; no allocation between groups applies.",
        n_total = res$n_total,
        n_per_group_txt = sprintf("N = %d pairs", res$n_total),
        effect_branch = "mcnemar_raw",
        effect_branch_details = list(
          p10 = sprintf("%.3f", res$p10), p01 = sprintf("%.3f", res$p01),
          delta = sprintf("%.3f", res$delta)
        ),
        method_key = "mcnemar"
      )
    })

    wire_results_server(input, output, session, family = "mcnemar",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn)
  })
}
