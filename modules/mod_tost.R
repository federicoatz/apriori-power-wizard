## mod_tost.R
## -----------------------------------------------------------------------
## Family module: TOST (Two One-Sided Tests) equivalence/non-inferiority
## test for two independent means, on Cohen's d scale. Unlike a normal
## comparison ("is there a difference"), this asks "can we rule out a
## difference big enough to matter". The key parameter -- the equivalence
## margin -- is inherently a directly-stated SESOI-like quantity, so this
## family (like McNemar) does not reuse the generic Cohen's-convention /
## safeguard-power / SESOI effect-size step.
## -----------------------------------------------------------------------

mod_tost_ui <- function(id) {
  ns <- NS(id)
  tagList(
    step_progress_ui(1, c("Design", "Parameters", "Effect size", "Results")),
    tabsetPanel(
      id = ns("wiz"), type = "hidden",
      tabPanelBody("design",
        h3(icon("scale-balanced"), " Step: Design structure"),
        p(class = "step-intro",
          "Two independent groups compared on a continuous outcome, but",
          "instead of testing whether they differ, this tests whether",
          "any difference is small enough to be considered practically",
          "equivalent (a TOST -- Two One-Sided Tests -- procedure)."),
        guided_box("In plain language",
          p("A normal 'is there a difference' test can never PROVE two",
            "things are the same -- failing to find a difference just",
            "means you didn't find one, which could be because there",
            "genuinely isn't one, or just because your study was too",
            "small. Equivalence testing flips the logic: you actively try",
            "to rule out any difference bigger than a margin you decide",
            "in advance. Use this family only when your actual research",
            "question is 'are these practically the same', not 'do these",
            "differ'.")
        ),
        div(class = "well well-info", icon("lightbulb"),
            strong(" Example: "), "a generic version of a product claims",
            "to perform the same as the original -- rather than trying to",
            "detect ANY difference, you want to rule out a difference big",
            "enough to matter to users."),
        div(class = "field-hint", icon("circle-check"),
            " Use this instead of the standard 'Two independent means'",
            "family whenever the research question is \"are these",
            "practically the same\" rather than \"do these differ\"."),
        wizard_nav_ui(ns, "design", show_back = FALSE)
      ),
      tabPanelBody("params",
        params_step_ui(ns),
        div(class = "field-hint",
            "Test direction is not used here: TOST is always two one-sided tests by construction. Allocation ratio is not used either: the exact calculation implemented here assumes equal-sized groups."),
        wizard_nav_ui(ns, "params")
      ),
      tabPanelBody("effect_size",
        h3(icon("bullseye"), " Step: Equivalence margin"),
        p(class = "step-intro",
          "State the largest difference between the two groups (in",
          "Cohen's d units) that you would still consider practically",
          "equivalent. Differences smaller than this, in either",
          "direction, would not change your conclusions."),
        numericInput(ns("delta_eq"),
          help_tip("Equivalence margin (Cohen's d)",
            "Think of it as a SESOI in reverse: not 'the smallest effect I want to detect', but 'the largest effect I'm willing to treat as negligible'. A common starting point is 0.3-0.5 SDs, but this should reflect what actually matters for your specific claim."),
          value = 0.4, min = 0.01, step = 0.05),
        accordion(
          id = ns("tost_advanced_acc"), open = FALSE,
          accordion_panel(
            title = tagList(icon("sliders"), " Advanced: assumed true difference"),
            value = "advanced",
            numericInput(ns("assumed_theta"),
              help_tip("Assumed true difference (Cohen's d)",
                "For planning purposes, most equivalence studies assume the TRUE difference is exactly zero -- the most conservative, hardest-to-satisfy scenario, and the safest default. Only change this if you have a specific reason to expect a small nonzero true difference."),
              value = 0, step = 0.05)
          )
        ),
        uiOutput(ns("tost_summary")),
        wizard_nav_ui(ns, "effect_size", next_label = "Compute")
      ),
      tabPanelBody("results",
        results_panel_ui(ns),
        wizard_nav_ui(ns, "results", show_next = FALSE)
      )
    )
  )
}

mod_tost_server <- function(id) {
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
    delta_eq_r <- reactive(safe_numeric(input$delta_eq, 0.001, 20, 0.4))
    theta_r <- reactive(safe_numeric(input$assumed_theta, -20, 20, 0))

    tost_valid <- reactive({
      de <- delta_eq_r(); th <- theta_r()
      !is.na(de) && !is.na(th) && de > 0 && abs(th) < de
    })

    output$tost_summary <- renderUI({
      de <- delta_eq_r(); th <- theta_r()
      if (is.na(de) || de <= 0) return(helpText("Enter a positive equivalence margin above."))
      if (is.na(th)) return(helpText("Enter an assumed true difference above (0 by default)."))
      if (abs(th) >= de) {
        return(div(class = "well well-warning", icon("triangle-exclamation"),
          " The assumed true difference must be smaller (in absolute value) than the equivalence margin -- otherwise equivalence can never be demonstrated, at any sample size."))
      }
      div(class = "well well-result", icon("circle-check"),
          sprintf(" Testing whether the true difference falls strictly within ±%.3f, assuming a true difference of %.3f.", de, th))
    })

    result_r <- reactive({
      p <- params()
      req(tost_valid())
      power_tost_n(delta_eq = delta_eq_r(), theta = theta_r(), sig_level = p$alpha, power = p$power)
    })

    solve_n_fn <- function(sig_level, power, effect = NULL) {
      power_tost_n(delta_eq = effect %||% delta_eq_r(), theta = theta_r(), sig_level = sig_level, power = power)
    }
    effect_set_r <- reactive(effect_comparison_values(delta_eq_r(), "magnitude"))

    n_summary_r <- reactive({
      res <- result_r()
      tagList(
        tags$p(icon("users"), sprintf(" Group 1: n = %d  |  Group 2: n = %d", res$n_per_group, res$n_per_group)),
        tags$p(icon("bullseye"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target))
      )
    })

    curve_extra_args_r <- reactive({
      list(delta_eq = delta_eq_r(), theta = theta_r(), sig_level = params()$alpha)
    })
    n_solution_r <- reactive(result_r()$n)

    sensitivity_fn <- function(n_max) {
      p <- params()
      sensitivity_min_effect("tost", n_max = n_max, theta = theta_r(),
                              sig_level = p$alpha, power = p$power)
    }

    report_spec_r <- reactive({
      res <- result_r(); p <- params()
      list(
        analysis = "TOST equivalence test for two independent means",
        design = "a between-subjects design with two independent groups, tested for equivalence rather than difference",
        effect_label_short = sprintf("equivalence within ±%.3f (Cohen's d)", res$delta_eq),
        alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
        tails = "none",
        allocation = "Groups were assumed to be equally sized (required by the exact calculation implemented here).",
        n_total = res$n_total,
        n_per_group_txt = sprintf("n1 = %d, n2 = %d", res$n_per_group, res$n_per_group),
        effect_branch = "equivalence_margin",
        effect_branch_details = list(
          delta_eq = sprintf("%.3f", res$delta_eq),
          assumed_theta = sprintf("%.3f", res$theta),
          theta_is_zero = isTRUE(all.equal(res$theta, 0))
        ),
        method_key = "tost"
      )
    })

    wire_results_server(input, output, session, family = "tost",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn, effect_set_r = effect_set_r)
  })
}
