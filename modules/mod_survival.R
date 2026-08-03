## mod_survival.R
## -----------------------------------------------------------------------
## Family module: two-group log-rank test for a time-to-event outcome
## (see R/power_survival.R for the Schoenfeld 1983 formula and its
## documented scope/limitations). Like McNemar/TOST, the effect (a hazard
## ratio) does not fit the generic Cohen's-convention/safeguard/SESOI
## step, so this uses a custom, direct-entry-or-safeguard effect-size step.
##
## Framed for this app's actual audience (experimental economics/
## psychology), where time-to-event designs are a genuine but niche tool
## -- e.g. time until a participant exits a market/game in an
## experimental-economics study, or attrition/dropout analysis in a
## longitudinal design -- not the clinical-trial language ("time to
## death/relapse") the underlying method usually comes wrapped in.
## -----------------------------------------------------------------------

mod_survival_ui <- function(id) {
  ns <- NS(id)
  tagList(
    step_progress_ui(1, c("Design", "Parameters", "Effect size", "Results")),
    tabsetPanel(
      id = ns("wiz"), type = "hidden",
      tabPanelBody("design",
        h4(icon("hourglass-half"), " Step: Design structure"),
        p(class = "step-intro",
          "Two independent groups compared on a time-to-event outcome --",
          "not just WHETHER something happens, but HOW LONG it takes to",
          "happen. In experimental economics or psychology this usually",
          "means something like time until a participant exits a market",
          "or negotiation, time until a strategic choice is reached, or",
          "attrition/dropout timing in a longitudinal study."),
        guided_box("In plain language",
          p("This tests whether one group reaches the event (exits,",
            "decides, drops out...) systematically sooner or later than",
            "the other, using a ", strong("log-rank test"), " -- accounting",
            "for participants who never reach the event before the study",
            "ends (they're still informative: they tell you the event",
            "didn't happen for at least that long). This is a genuinely",
            "niche tool for this app's audience -- if your outcome is",
            "simply yes/no or a plain continuous measure, one of the",
            "other families is almost certainly the better fit."),
          p(strong("Note on scope: "), "this calculator assumes a single",
            "overall probability that a participant experiences the event",
            "by the end of the study, not a detailed accrual/censoring",
            "schedule. It is deliberately simple, matching the",
            "closed-form, non-simulation style of every other family in",
            "this app -- for complex accrual/follow-up designs, a",
            "dedicated survival-analysis tool is more appropriate.")
        ),
        numericInput(ns("p_event"),
          help_tip("Expected proportion of participants who experience the event",
            "Averaged across both groups: what fraction of participants do you expect to reach the event (exit, decide, drop out...) before the study ends, rather than being censored? Higher values mean more information per participant and a smaller required sample."),
          value = 0.70, min = 0.01, max = 0.99, step = 0.05),
        div(class = "field-hint", icon("circle-info"),
            " The formula used here (Schoenfeld, 1983) is most accurate when the hazard ratio is close to 1 and group sizes are balanced; for a strong effect combined with a very unequal allocation ratio, it tends to recommend a somewhat LARGER sample than strictly necessary, never a smaller one."),
        wizard_nav_ui(ns, "design", show_back = FALSE)
      ),
      tabPanelBody("params",
        params_step_ui(ns),
        wizard_nav_ui(ns, "params")
      ),
      tabPanelBody("effect_size",
        h4(icon("bullseye"), " Step: Effect size (hazard ratio)"),
        p(class = "step-intro",
          "The hazard ratio (HR) compares how quickly the event happens",
          "in one group relative to the other. HR < 1 means the event",
          "happens more slowly / less often in group 2 (e.g., participants",
          "take longer to reach a decision, or are less likely to drop",
          "out); HR > 1 means the opposite. HR = 1 means no difference at",
          "all, and can never be used as a target effect."),
        div(class = "choice-card-grid",
          radioButtons(ns("hr_branch"), NULL,
            choiceNames = list(
              div(class = "choice-card",
                icon("crosshairs", class = "choice-icon"),
                div(strong("Enter the hazard ratio directly"),
                    p(class = "choice-desc", "State the smallest hazard ratio you consider meaningful."))),
              div(class = "choice-card",
                icon("shield-halved", class = "choice-icon"),
                div(strong("From a previous study"),
                    p(class = "choice-desc", "A published hazard ratio, corrected for its imprecision (safeguard power).")))
            ),
            choiceValues = c("direct", "safeguard"),
            selected = "direct")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'direct'", ns("hr_branch")),
          numericInput(ns("hr_direct_value"),
            help_tip("Hazard ratio", "The smallest hazard ratio (in either direction away from 1) that you would consider meaningful for your research question."),
            value = 0.60, min = 0.001, max = 20, step = 0.05)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'safeguard'", ns("hr_branch")),
          div(class = "well well-info", icon("circle-info"), " ", safeguard_power_explainer()),
          fluidRow(
            column(6, numericInput(ns("hr_sg_published"), "Published hazard ratio", value = NA, min = 0.001, max = 20)),
            column(6, numericInput(ns("hr_sg_events"),
                     help_tip("Original study's total number of events", "The TOTAL number of events (not the total N) recorded across both arms of the original study -- usually reported directly in its results (e.g., \"with 87 events recorded...\")."),
                     value = NA, min = 2))
          ),
          fluidRow(
            column(6, sliderInput(ns("hr_sg_conf_level"),
                     help_tip("Confidence level for the safeguard bound", "How conservative the correction should be. 80% is the default recommended by Perugini et al. (2014); higher values are more cautious and require a larger sample."),
                     min = 0.5, max = 0.99, value = 0.80, step = 0.01)),
            column(6, radioButtons(ns("hr_sg_one_sided"), "Interval type",
                                    choices = c("One-sided (recommended, Perugini et al. 2014)" = "one",
                                                "Two-sided" = "two"),
                                    selected = "one"))
          )
        ),
        uiOutput(ns("hr_summary")),
        wizard_nav_ui(ns, "effect_size", next_label = "Compute")
      ),
      tabPanelBody("results",
        results_panel_ui(ns),
        wizard_nav_ui(ns, "results", show_next = FALSE)
      )
    )
  )
}

mod_survival_server <- function(id) {
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
    p_event_r <- reactive(safe_numeric(input$p_event, 0.01, 0.99, 0.70))

    effect_value <- reactive({
      branch <- input$hr_branch %||% "direct"
      if (branch == "safeguard") {
        pub_hr <- safe_numeric(input$hr_sg_published, 0.001, 20)
        pub_events <- safe_numeric(input$hr_sg_events, 2, 1e6)
        req(pub_hr, pub_events)
        sg <- safeguard_ci_logHR(pub_hr, pub_events,
                                  alloc_ratio = params()$allocation_ratio,
                                  conf_level = input$hr_sg_conf_level %||% 0.80,
                                  one_sided = identical(input$hr_sg_one_sided, "one"))
        sg$hr_safeguard
      } else {
        safe_numeric(input$hr_direct_value, 0.001, 20, 0.60)
      }
    })

    hr_valid <- reactive({
      hr <- effect_value()
      !is.null(hr) && !is.na(hr) && hr > 0 && !isTRUE(all.equal(hr, 1))
    })

    output$hr_summary <- renderUI({
      if (!hr_valid()) {
        return(div(class = "well well-warning", icon("triangle-exclamation"),
          " Enter a hazard ratio different from 1 (an HR of exactly 1 means no effect at all, and can never be detected at any sample size)."))
      }
      div(class = "well well-result", icon("circle-check"),
          sprintf(" Target hazard ratio: %.3f", effect_value()))
    })

    effect_branch_details <- reactive({
      branch <- input$hr_branch %||% "direct"
      if (branch == "safeguard") {
        pub_hr <- safe_numeric(input$hr_sg_published, 0.001, 20)
        pub_events <- safe_numeric(input$hr_sg_events, 2, 1e6)
        list(
          published_hr = sprintf("%.3f", pub_hr),
          published_events = round(pub_events),
          conf_level = input$hr_sg_conf_level %||% 0.80,
          one_sided = identical(input$hr_sg_one_sided, "one"),
          safeguard_hr = sprintf("%.3f", effect_value())
        )
      } else {
        list(hr = sprintf("%.3f", effect_value()))
      }
    })

    result_r <- reactive({
      p <- params()
      req(hr_valid())
      power_survival_n(hr = effect_value(), p_event = p_event_r(),
                        alloc_ratio = p$allocation_ratio, sig_level = p$alpha,
                        power = p$power, tails = p$tails)
    })

    solve_n_fn <- function(sig_level, power, effect = NULL) {
      p <- params()
      power_survival_n(hr = effect %||% effect_value(), p_event = p_event_r(),
                        alloc_ratio = p$allocation_ratio, sig_level = sig_level,
                        power = power, tails = p$tails)
    }
    effect_set_r <- reactive(effect_comparison_values(effect_value(), "ratio"))

    n_summary_r <- reactive({
      res <- result_r()
      tagList(
        tags$p(icon("users"), sprintf(" Group 1: n = %d  |  Group 2: n = %d  (Total N = %d)", res$n1, res$n2, res$n_total)),
        tags$p(icon("chart-simple"), sprintf(" Events required: %d (at an assumed event rate of %.0f%%)", res$events_required, p_event_r() * 100)),
        tags$p(icon("gauge-high"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target))
      )
    })

    curve_extra_args_r <- reactive({
      p <- params()
      list(hr = effect_value(), p_event = p_event_r(),
           alloc_ratio = p$allocation_ratio, sig_level = p$alpha, tails = p$tails)
    })
    n_solution_r <- reactive(result_r()$n_total)

    sensitivity_fn <- function(n_max) {
      p <- params()
      sensitivity_min_effect("survival", n_max = round(n_max), p_event = p_event_r(),
                              alloc_ratio = p$allocation_ratio, sig_level = p$alpha,
                              power = p$power, tails = p$tails)
    }

    report_spec_r <- reactive({
      res <- result_r(); p <- params()
      list(
        analysis = "two-group log-rank test",
        design = sprintf("a two-group comparison of time-to-event outcomes (log-rank test), assuming %.0f%% of participants experience the event by the end of the study",
                          p_event_r() * 100),
        effect_label_short = sprintf("a hazard ratio of %.3f", effect_value()),
        alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
        tails = p$tails,
        allocation = if (isTRUE(all.equal(p$allocation_ratio, 1))) {
          "Groups were assumed to be equally sized."
        } else {
          sprintf("Groups followed an allocation ratio of %.2f (n2/n1).", p$allocation_ratio)
        },
        n_total = res$n_total,
        n_per_group_txt = sprintf("n1 = %d, n2 = %d (%d events required)", res$n1, res$n2, res$events_required),
        effect_branch = if (identical(input$hr_branch %||% "direct", "safeguard")) "hr_safeguard" else "hr_direct",
        effect_branch_details = effect_branch_details(),
        method_key = "survival"
      )
    })

    wire_results_server(input, output, session, family = "survival",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn, effect_set_r = effect_set_r)
  })
}
