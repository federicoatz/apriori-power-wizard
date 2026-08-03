## mod_regression.R
## -----------------------------------------------------------------------
## Family module: multiple linear regression, testing a focal predictor
## (or a set of predictors) over and above covariates already in the
## model. Effect size metric: Cohen's f2.
## -----------------------------------------------------------------------

mod_regression_ui <- function(id) {
  ns <- NS(id)
  tagList(
    step_progress_ui(1, c("Design", "Parameters", "Effect size", "Results")),
    tabsetPanel(
      id = ns("wiz"), type = "hidden",
      tabPanelBody("design",
        h4(icon("chart-line"), " Step: Design structure"),
        p(class = "step-intro",
          "Specify the regression model: the focal test concerns one or",
          "more predictors, evaluated over and above any covariates",
          "already included in the model."),
        guided_box("In plain language",
          p("You're testing whether one or more predictors relate to a",
            "continuous outcome, ", strong("controlling for"), " whatever",
            "else is already in the model (the covariates). Power here",
            "depends on the covariates too: predictors that overlap a lot",
            "with your covariates give you less unique information to",
            "work with, so they generally need a somewhat larger sample",
            "to detect at the same effect size.")
        ),
        fluidRow(
          column(6, numericInput(ns("u"),
                    help_tip("Number of predictors in the focal test (u)", "How many predictors your hypothesis test is actually about. For one focal predictor's effect (e.g., 'does income predict trust, controlling for age?'), u = 1. Use u > 1 only if you're jointly testing a block of predictors (e.g., a set of dummy variables for one categorical factor)."),
                    value = 1, min = 1, step = 1)),
          column(6, numericInput(ns("n_cov"),
                    help_tip("Number of other covariates already in the model", "Control variables that are in the model but are NOT part of your hypothesis (e.g., age, gender, session fixed effects)."),
                    value = 0, min = 0, step = 1))
        ),
        div(class = "well well-info", icon("lightbulb"),
            strong(" Example: "), "testing whether a nudge intervention (1 focal",
            "predictor, u = 1) predicts savings behavior, controlling for income",
            "and age (2 covariates)."),
        wizard_nav_ui(ns, "design", show_back = FALSE)
      ),
      tabPanelBody("params",
        params_step_ui(ns),
        wizard_nav_ui(ns, "params")
      ),
      tabPanelBody("effect_size",
        effect_size_step_ui(
          ns,
          cohen_ui = radioButtons(ns("cohen_size"), "Benchmark",
            choices = c("Small (f² = 0.02)" = "small",
                        "Medium (f² = 0.15)" = "medium",
                        "Large (f² = 0.35)" = "large"),
            selected = "medium"),
          sesoi_ui = tagList(
            radioButtons(ns("sesoi_mode"), "Specify the SESOI as:",
              choices = c("R² change (full vs. reduced model)" = "r2change",
                          "Raw slope + predictor/outcome SD" = "raw",
                          "Already standardized (Cohen's f²)" = "standardized"),
              selected = "r2change"),
            conditionalPanel(condition = sprintf("input['%s'] == 'r2change'", ns("sesoi_mode")),
              fluidRow(
                column(6, numericInput(ns("sesoi_r2_full"), "R² of full model", value = NA, min = 0, max = 0.999, step = 0.01)),
                column(6, numericInput(ns("sesoi_r2_reduced"), "R² of reduced model (covariates only)", value = 0, min = 0, max = 0.999, step = 0.01))
              )
            ),
            conditionalPanel(condition = sprintf("input['%s'] == 'raw'", ns("sesoi_mode")),
              fluidRow(
                column(4, numericInput(ns("sesoi_b"), "Minimal slope of interest (raw units)", value = NA)),
                column(4, numericInput(ns("sesoi_sdx"), "SD of the predictor", value = NA, min = 0.0001)),
                column(4, numericInput(ns("sesoi_sdy"), "SD of the outcome", value = NA, min = 0.0001))
              )
            ),
            conditionalPanel(condition = sprintf("input['%s'] == 'standardized'", ns("sesoi_mode")),
              numericInput(ns("sesoi_f2"), "Smallest effect size of interest (Cohen's f²)", value = 0.05, min = 0.0001, step = 0.01)
            )
          ),
          safeguard_metric_label = "Published (semi-partial) correlation r or standardized beta"
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

mod_regression_server <- function(id) {
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
    u <- reactive(max(1L, as.integer(safe_numeric(input$u, 1, 50, 1))))
    n_cov <- reactive(max(0L, as.integer(safe_numeric(input$n_cov, 0, 200, 0))))

    effect_value <- reactive({
      branch <- input$es_branch %||% "sesoi"
      if (branch == "cohen") {
        unname(cohen_benchmarks("f2")[input$cohen_size %||% "medium"])
      } else if (branch == "safeguard") {
        r_pub <- safe_numeric(input$sg_published_value, -0.999, 0.999)
        n_pub <- safe_numeric(input$sg_published_n, 5, 1e6)
        req(r_pub, n_pub)
        sg <- safeguard_ci_r(r_pub, n_pub, conf_level = input$sg_conf_level %||% 0.80,
                              one_sided = identical(input$sg_one_sided, "one"))
        r2_to_f2(sg$r_safeguard^2 + 1e-9, 0) # treat r^2 as R2_full with R2_reduced = 0
      } else {
        mode <- input$sesoi_mode %||% "r2change"
        if (mode == "r2change") {
          req(input$sesoi_r2_full)
          r2_to_f2(safe_numeric(input$sesoi_r2_full, 0.0001, 0.999),
                    safe_numeric(input$sesoi_r2_reduced, 0, 0.999, 0))
        } else if (mode == "raw") {
          req(input$sesoi_b, input$sesoi_sdx, input$sesoi_sdy)
          sesoi_slope_to_f2(input$sesoi_b, input$sesoi_sdx, input$sesoi_sdy)
        } else {
          safe_numeric(input$sesoi_f2, 0.0001, 5, 0.05)
        }
      }
    })

    effect_branch_details <- reactive({
      branch <- input$es_branch %||% "sesoi"
      p <- params()
      if (branch == "cohen") {
        list(label = input$cohen_size, value = sprintf("f² = %.3f", effect_value()))
      } else if (branch == "safeguard") {
        r_pub <- safe_numeric(input$sg_published_value, -0.999, 0.999)
        n_pub <- safe_numeric(input$sg_published_n, 5, 1e6)
        naive_f2 <- r2_to_f2(r_pub^2 + 1e-9, 0)
        naive <- tryCatch(power_regression_n(naive_f2, u(), n_cov(), p$alpha, p$power)$n_total, error = function(e) NA)
        list(
          published_value = sprintf("r = %.3f", r_pub), published_n = round(n_pub),
          conf_level = input$sg_conf_level %||% 0.80,
          one_sided = identical(input$sg_one_sided, "one"),
          safeguard_value = sprintf("f² = %.3f", effect_value()),
          n_naive = naive,
          n_safeguard = tryCatch(power_regression_n(effect_value(), u(), n_cov(), p$alpha, p$power)$n_total, error = function(e) NA)
        )
      } else {
        list(description = sprintf("f² = %.3f", effect_value()))
      }
    })

    result_r <- reactive({
      p <- params()
      req(effect_value())
      power_regression_n(f2 = effect_value(), n_predictors_tested = u(),
                          n_covariates = n_cov(), sig_level = p$alpha, power = p$power)
    })

    solve_n_fn <- function(sig_level, power) {
      power_regression_n(f2 = effect_value(), n_predictors_tested = u(),
                          n_covariates = n_cov(), sig_level = sig_level, power = power)
    }

    n_summary_r <- reactive({
      res <- result_r()
      tagList(
        tags$p(icon("list-check"), sprintf(" Predictors tested (u): %d  |  Other covariates: %d", res$u, n_cov())),
        tags$p(icon("bullseye"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target))
      )
    })

    curve_extra_args_r <- reactive({
      p <- params()
      list(f2 = effect_value(), n_predictors_tested = u(), sig_level = p$alpha)
    })
    n_solution_r <- reactive(result_r()$n_total)

    sensitivity_fn <- function(n_max) {
      p <- params()
      sensitivity_min_effect("regression", n_max = n_max, n_predictors_tested = u(),
                              sig_level = p$alpha, power = p$power)
    }

    report_spec_r <- reactive({
      res <- result_r(); p <- params()
      list(
        analysis = "multiple linear regression (focal predictor test)",
        design = sprintf("a multiple regression model testing %d focal predictor(s) over %d other covariate(s)", u(), n_cov()),
        effect_label_short = sprintf("an effect of f² = %.3f", effect_value()),
        alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
        tails = "two.sided",
        allocation = "Not applicable (single-sample regression design).",
        n_total = res$n_total,
        n_per_group_txt = sprintf("N = %d (u = %d, v ≈ %.1f)", res$n_total, res$u, res$v_achieved),
        effect_branch = input$es_branch %||% "sesoi",
        effect_branch_details = effect_branch_details(),
        method_key = "regression"
      )
    })

    wire_results_server(input, output, session, family = "regression",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn)
  })
}
