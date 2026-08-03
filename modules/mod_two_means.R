## mod_two_means.R
## -----------------------------------------------------------------------
## Family module: comparison of two independent means (Student's t-test).
## Internal sub-wizard: Design -> Parameters -> Effect size -> Results.
## Uses a hidden tabsetPanel (type = "hidden") so every sub-step's inputs
## stay mounted in the DOM -- this is what lets users go back and forth
## without losing anything they already entered.
## -----------------------------------------------------------------------

mod_two_means_ui <- function(id) {
  ns <- NS(id)
  tagList(
    step_progress_ui(1, c("Design", "Parameters", "Effect size", "Results")),
    tabsetPanel(
      id = ns("wiz"), type = "hidden",
      tabPanelBody("design",
        h4(icon("people-arrows"), " Step: Design structure"),
        p(class = "step-intro",
          "Two independent groups (e.g., treatment vs. control), each measured",
          "once on a continuous outcome, compared with an independent-samples",
          "t-test. Nothing to configure here beyond confirming this matches",
          "your study."),
        guided_box("In plain language",
          p("You have two groups of DIFFERENT people (not the same",
            "people measured twice -- that's a different family, 'Paired",
            "/ repeated measures'), and one continuous number you measure",
            "on each person. The question is simply: on average, do the",
            "two groups differ on that number?")
        ),
        div(class = "well well-info", icon("lightbulb"),
            strong(" Example: "), "an online experiment randomly assigns",
            "participants to a bonus-pay condition or a flat-pay condition,",
            "then measures task performance -- that's exactly this design."),
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
            help_tip("Benchmark", "d is Cohen's standardized mean difference: (mean1 - mean2) / pooled SD. As a rule of thumb, 0.2 SDs apart is a small gap between groups, 0.5 is visible to the naked eye, 0.8 is large."),
            choices = c("Small (d = 0.20)" = "small",
                        "Medium (d = 0.50)" = "medium",
                        "Large (d = 0.80)" = "large"),
            selected = "medium"),
          sesoi_ui = tagList(
            radioButtons(ns("sesoi_mode"), "Specify the SESOI as:",
              choices = c("Raw mean difference + expected SD" = "raw",
                          "Already standardized (Cohen's d)" = "standardized"),
              selected = "standardized"),
            conditionalPanel(condition = sprintf("input['%s'] == 'raw'", ns("sesoi_mode")),
              fluidRow(
                column(6, numericInput(ns("sesoi_raw_diff"),
                         help_tip("Minimal difference of interest (raw units)", "In the outcome's own units -- e.g., '2 dollars', '150 milliseconds', '0.5 points on the scale'."),
                         value = NA)),
                column(6, numericInput(ns("sesoi_sd"),
                         help_tip("Expected (pooled) SD", "How spread out individual scores typically are around the mean, in the same raw units. If you have pilot data or a similar prior study, use its SD."),
                         value = NA, min = 0.0001))
              )
            ),
            conditionalPanel(condition = sprintf("input['%s'] == 'standardized'", ns("sesoi_mode")),
              numericInput(ns("sesoi_d"), "Smallest effect size of interest (Cohen's d)", value = 0.3, step = 0.05)
            )
          ),
          safeguard_metric_label = "Published Cohen's d"
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

mod_two_means_server <- function(id) {
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
        unname(cohen_benchmarks("d")[input$cohen_size %||% "medium"])
      } else if (branch == "safeguard") {
        pub <- safe_numeric(input$sg_published_value, 0.0001, 5)
        n_pub <- safe_numeric(input$sg_published_n, 4, 1e6)
        req(pub, n_pub)
        sg <- safeguard_ci_d(pub, n1 = n_pub / 2, n2 = n_pub / 2,
                              conf_level = input$sg_conf_level %||% 0.80,
                              one_sided = identical(input$sg_one_sided, "one"))
        sg$d_safeguard
      } else {
        if (identical(input$sesoi_mode, "raw")) {
          req(input$sesoi_raw_diff, input$sesoi_sd)
          sesoi_raw_to_d(input$sesoi_raw_diff, input$sesoi_sd)
        } else {
          safe_numeric(input$sesoi_d, 0.0001, 5, 0.3)
        }
      }
    })

    effect_branch_details <- reactive({
      branch <- input$es_branch %||% "sesoi"
      p <- params()
      if (branch == "cohen") {
        list(label = input$cohen_size, value = sprintf("d = %.2f", effect_value()))
      } else if (branch == "safeguard") {
        pub <- safe_numeric(input$sg_published_value, 0.0001, 5)
        n_pub <- safe_numeric(input$sg_published_n, 4, 1e6)
        naive <- tryCatch(power_two_means_n(pub, p$alpha, p$power, p$tails, p$allocation_ratio)$n_total, error = function(e) NA)
        list(
          published_value = sprintf("d = %.3f", pub), published_n = round(n_pub),
          conf_level = input$sg_conf_level %||% 0.80,
          one_sided = identical(input$sg_one_sided, "one"),
          safeguard_value = sprintf("d = %.3f", effect_value()),
          n_naive = naive,
          n_safeguard = tryCatch(power_two_means_n(effect_value(), p$alpha, p$power, p$tails, p$allocation_ratio)$n_total, error = function(e) NA)
        )
      } else {
        list(description = sprintf("d = %.3f", effect_value()))
      }
    })

    result_r <- reactive({
      p <- params()
      req(effect_value())
      power_two_means_n(d = effect_value(), sig_level = p$alpha, power = p$power,
                         alternative = p$tails, allocation_ratio = p$allocation_ratio)
    })

    solve_n_fn <- function(sig_level, power) {
      p <- params()
      power_two_means_n(d = effect_value(), sig_level = sig_level, power = power,
                         alternative = p$tails, allocation_ratio = p$allocation_ratio)
    }

    n_summary_r <- reactive({
      res <- result_r()
      tagList(
        tags$p(icon("users"), sprintf(" Group 1: n = %d  |  Group 2: n = %d", res$n1, res$n2)),
        tags$p(icon("bullseye"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target))
      )
    })

    curve_extra_args_r <- reactive({
      p <- params()
      list(d = effect_value(), sig_level = p$alpha, alternative = p$tails)
    })

    n_solution_r <- reactive(result_r()$n1)

    sensitivity_fn <- function(n_max) {
      p <- params()
      sensitivity_min_effect("two_means", n_max = n_max, sig_level = p$alpha,
                              power = p$power, alternative = p$tails)
    }

    report_spec_r <- reactive({
      res <- result_r(); p <- params()
      list(
        analysis = "comparison of two independent means (Student's t-test)",
        design = "a between-subjects design with two independent groups",
        effect_label_short = sprintf("an effect of d = %.3f", effect_value()),
        alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
        tails = p$tails,
        allocation = if (p$allocation_ratio == 1) "Groups were assumed to be equally sized." else sprintf("Groups followed an allocation ratio of %.2f (n2/n1).", p$allocation_ratio),
        n_total = res$n_total,
        n_per_group_txt = sprintf("n1 = %d, n2 = %d", res$n1, res$n2),
        effect_branch = input$es_branch %||% "sesoi",
        effect_branch_details = effect_branch_details(),
        method_key = "two_means"
      )
    })

    wire_results_server(input, output, session, family = "two_means",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn)
  })
}
