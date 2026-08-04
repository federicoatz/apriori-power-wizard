## mod_correlation.R
## -----------------------------------------------------------------------
## Family module: test of a single bivariate Pearson correlation against
## zero. The simplest of the sub-wizards -- a single sample, no groups to
## allocate between.
## -----------------------------------------------------------------------

mod_correlation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    step_progress_ui(1, c("Design", "Parameters", "Effect size", "Results")),
    tabsetPanel(
      id = ns("wiz"), type = "hidden",
      tabPanelBody("design",
        h3(icon("chart-line"), " Step: Design structure"),
        p(class = "step-intro",
          "A single sample in which two continuous variables are measured",
          "on each participant, tested for whether they are correlated",
          "(Pearson's r)."),
        guided_box("In plain language",
          p("Both variables are measured on the SAME people, and neither",
            "one is manipulated or treated as the 'outcome' specifically",
            "-- you're just asking whether they tend to move together.",
            "This is the simplest case of 'a predictor relates to an",
            "outcome': if you have other variables you want to control",
            "for at the same time, use 'Multiple linear regression'",
            "instead.")
        ),
        div(class = "well well-info", icon("lightbulb"),
            strong(" Example: "), "whether time spent on a task relates",
            "to a post-task satisfaction score, measured once per",
            "participant."),
        wizard_nav_ui(ns, "design", show_back = FALSE)
      ),
      tabPanelBody("params",
        params_step_ui(ns),
        div(class = "field-hint",
            "Allocation ratio is not used here: there is one sample, not two groups."),
        wizard_nav_ui(ns, "params")
      ),
      tabPanelBody("effect_size",
        effect_size_step_ui(
          ns,
          cohen_ui = radioButtons(ns("cohen_size"),
            help_tip("Benchmark", "r is the correlation coefficient itself, already on its own natural -1 to 1 scale."),
            choices = c("Small (r = 0.10)" = "small",
                        "Medium (r = 0.30)" = "medium",
                        "Large (r = 0.50)" = "large"),
            selected = "medium"),
          sesoi_ui = numericInput(ns("sesoi_r"),
            "Smallest correlation of interest (r)", value = 0.3, min = -0.99, max = 0.99, step = 0.01),
          safeguard_metric_label = "Published correlation (r)"
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

mod_correlation_server <- function(id) {
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
        unname(cohen_benchmarks("r")[input$cohen_size %||% "medium"])
      } else if (branch == "safeguard") {
        pub <- safe_numeric(input$sg_published_value, -0.99, 0.99)
        n_pub <- safe_numeric(input$sg_published_n, 5, 1e6)
        req(pub, n_pub)
        sg <- safeguard_ci_r(pub, n = n_pub,
                              conf_level = input$sg_conf_level %||% 0.80,
                              one_sided = identical(input$sg_one_sided, "one"))
        sg$r_safeguard
      } else {
        safe_numeric(input$sesoi_r, -0.99, 0.99, 0.3)
      }
    })

    effect_branch_details <- reactive({
      branch <- input$es_branch %||% "sesoi"
      p <- params()
      if (branch == "cohen") {
        list(label = input$cohen_size, value = sprintf("r = %.2f", effect_value()))
      } else if (branch == "safeguard") {
        pub <- safe_numeric(input$sg_published_value, -0.99, 0.99)
        n_pub <- safe_numeric(input$sg_published_n, 5, 1e6)
        naive <- tryCatch(power_correlation_n(pub, p$alpha, p$power, p$tails)$n_total, error = function(e) NA)
        list(
          published_value = sprintf("r = %.3f", pub), published_n = round(n_pub),
          conf_level = input$sg_conf_level %||% 0.80,
          one_sided = identical(input$sg_one_sided, "one"),
          safeguard_value = sprintf("r = %.3f", effect_value()),
          n_naive = naive,
          n_safeguard = tryCatch(power_correlation_n(effect_value(), p$alpha, p$power, p$tails)$n_total, error = function(e) NA)
        )
      } else {
        list(description = sprintf("r = %.3f", effect_value()))
      }
    })

    result_r <- reactive({
      p <- params()
      req(effect_value())
      power_correlation_n(r = effect_value(), sig_level = p$alpha, power = p$power,
                           alternative = p$tails)
    })

    solve_n_fn <- function(sig_level, power, effect = NULL) {
      p <- params()
      power_correlation_n(r = effect %||% effect_value(), sig_level = sig_level, power = power,
                           alternative = p$tails)
    }
    effect_set_r <- reactive(effect_comparison_values(effect_value(), "magnitude"))

    n_summary_r <- reactive({
      res <- result_r()
      tagList(
        tags$p(icon("users"), sprintf(" Total N = %d", res$n_total)),
        tags$p(icon("bullseye"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target))
      )
    })

    curve_extra_args_r <- reactive({
      p <- params()
      list(r = effect_value(), sig_level = p$alpha, alternative = p$tails)
    })
    n_solution_r <- reactive(result_r()$n_total)

    sensitivity_fn <- function(n_max) {
      p <- params()
      sensitivity_min_effect("correlation", n_max = n_max, sig_level = p$alpha,
                              power = p$power, alternative = p$tails)
    }

    report_spec_r <- reactive({
      res <- result_r(); p <- params()
      list(
        analysis = "bivariate correlation test",
        design = "a single sample with two continuous variables measured on each participant",
        effect_label_short = sprintf("a correlation of r = %.3f", effect_value()),
        alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
        tails = p$tails,
        allocation = "There is a single sample; no allocation between groups applies.",
        n_total = res$n_total,
        n_per_group_txt = sprintf("N = %d", res$n_total),
        effect_branch = input$es_branch %||% "sesoi",
        effect_branch_details = effect_branch_details(),
        method_key = "correlation"
      )
    })

    wire_results_server(input, output, session, family = "correlation",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn, effect_set_r = effect_set_r)
  })
}
