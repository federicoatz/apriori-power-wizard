## mod_anova_factorial.R
## -----------------------------------------------------------------------
## Family module: between-subjects factorial ANOVA (1 or 2 factors),
## computed with a closed-form noncentral-F formula (base R, not simulation).
## This is the module where the "focal contrast" question lives: the user
## must state whether the target is a main effect or the interaction, and
## power is computed for exactly that effect (see R/power_anova_factorial.R
## for the noncentral-F formula and its degrees-of-freedom rules).
## -----------------------------------------------------------------------

mod_anova_factorial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    step_progress_ui(1, c("Design", "Parameters", "Effect size", "Results")),
    tabsetPanel(
      id = ns("wiz"), type = "hidden",
      tabPanelBody("design",
        h4(icon("table-cells"), " Step: Design structure"),
        p(class = "step-intro",
          "A factor is anything you manipulate or group participants by (e.g.,",
          "\"pay scheme\" with levels \"flat\" / \"bonus\"). With two factors you",
          "can also ask whether they interact -- whether the effect of one",
          "depends on the level of the other."),
        guided_box("In plain language",
          p("ANOVA compares averages across THREE OR MORE conditions at",
            "once (if you only have two conditions, 'Two independent",
            "means' is simpler and gives the exact same answer). With",
            "two factors crossed together, you additionally get to ask a",
            "richer question: not just whether each factor matters on",
            "its own, but whether they ", strong("interact"), " -- whether",
            "the effect of one depends on the level of the other. Decide",
            "up front which single question is your actual hypothesis;",
            "power is computed for that one contrast, not for everything",
            "at once.")
        ),
        radioButtons(ns("n_factors"),
                     help_tip("Number of between-subjects factors", "Each participant experiences only ONE combination of factor levels (that's what 'between-subjects' means). Pick 1 for a single manipulation with several conditions; pick 2 if you're crossing two manipulations, e.g., pay scheme x task difficulty."),
                     choices = c("1 (one-way ANOVA)" = "1", "2 (factorial ANOVA)" = "2"),
                     selected = "2", inline = TRUE),
        fluidRow(
          column(6, numericInput(ns("levels_a"),
                    help_tip("Levels of Factor A", "How many conditions Factor A has, e.g., 2 for \"control vs. treatment\", 3 for \"low / medium / high\"."),
                    value = 2, min = 2, max = 8, step = 1)),
          column(6, conditionalPanel(condition = sprintf("input['%s'] == '2'", ns("n_factors")),
                    numericInput(ns("levels_b"), "Levels of Factor B", value = 2, min = 2, max = 8, step = 1)))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == '2'", ns("n_factors")),
          radioButtons(ns("focal"),
                       help_tip("Which contrast is your focal hypothesis?", "This is the single most important choice in a factorial design. A main effect asks 'does Factor A matter on average, across all levels of B?'. The interaction asks 'does the effect of A change depending on B?'. These are different hypotheses with very different required sample sizes -- pick the one your research question is actually about."),
                       choices = c("Main effect of Factor A" = "A",
                                   "Main effect of Factor B" = "B",
                                   "Interaction A × B" = "A:B"),
                       selected = "A"),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'A:B'", ns("focal")),
            div(class = "well well-warning", icon("triangle-exclamation"), strong(" Important: "), interaction_power_warning())
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] != 'A:B'", ns("focal")),
            div(class = "well well-info", icon("circle-info"), " ", em(interaction_power_warning()))
          )
        ),
        div(class = "field-hint", icon("circle-check"),
            " Power is computed for this specific contrast only -- not for a generic omnibus F-test."),
        wizard_nav_ui(ns, "design", show_back = FALSE)
      ),
      tabPanelBody("params",
        params_step_ui(ns),
        div(class = "field-hint", "Allocation ratio is not used for factorial ANOVA; cell sizes are equal by design."),
        wizard_nav_ui(ns, "params")
      ),
      tabPanelBody("effect_size",
        effect_size_step_ui(
          ns,
          cohen_ui = radioButtons(ns("cohen_size"),
            help_tip("Benchmark", "Cohen's f measures how spread out the condition means are, relative to within-condition noise. It applies to whichever contrast (main effect or interaction) you selected in the previous step."),
            choices = c("Small (f = 0.10)" = "small",
                        "Medium (f = 0.25)" = "medium",
                        "Large (f = 0.40)" = "large"),
            selected = "medium"),
          sesoi_ui = tagList(
            radioButtons(ns("sesoi_mode"), "Specify the SESOI as:",
              choices = c("Partial eta-squared" = "eta2",
                          "Raw mean difference + SD (two-level focal factor only)" = "raw",
                          "Already standardized (Cohen's f)" = "standardized"),
              selected = "eta2"),
            conditionalPanel(condition = sprintf("input['%s'] == 'eta2'", ns("sesoi_mode")),
              numericInput(ns("sesoi_eta2"), "Partial eta-squared for the focal contrast",
                           value = 0.02, min = 0.0001, max = 0.99, step = 0.005)
            ),
            conditionalPanel(condition = sprintf("input['%s'] == 'raw'", ns("sesoi_mode")),
              fluidRow(
                column(6, numericInput(ns("sesoi_raw_diff"), "Minimal difference of interest (raw units)", value = NA)),
                column(6, numericInput(ns("sesoi_sd"), "Expected (pooled) SD", value = NA, min = 0.0001))
              ),
              helpText("Exact only when the focal factor has 2 levels (f = d / 2). With more levels this is an approximation.")
            ),
            conditionalPanel(condition = sprintf("input['%s'] == 'standardized'", ns("sesoi_mode")),
              numericInput(ns("sesoi_f"), "Smallest effect size of interest (Cohen's f)", value = 0.15, min = 0.0001, step = 0.01)
            )
          ),
          safeguard_metric_label = "Published Cohen's f (or partial eta-squared, converted below)"
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'safeguard'", ns("es_branch")),
          checkboxInput(ns("sg_is_eta2"), "The published value above is partial eta-squared, not f", value = FALSE)
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

mod_anova_factorial_server <- function(id) {
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
    n_factors <- reactive(as.integer(input$n_factors %||% "2"))
    levels_a <- reactive(max(2L, as.integer(safe_numeric(input$levels_a, 2, 8, 2))))
    levels_b <- reactive(if (n_factors() == 2) max(2L, as.integer(safe_numeric(input$levels_b, 2, 8, 2))) else NULL)
    focal <- reactive(if (n_factors() == 2) (input$focal %||% "A") else "A")

    effect_value <- reactive({
      branch <- input$es_branch %||% "sesoi"
      if (branch == "cohen") {
        unname(cohen_benchmarks("f")[input$cohen_size %||% "medium"])
      } else if (branch == "safeguard") {
        pub <- safe_numeric(input$sg_published_value, 0.0001, 5)
        n_pub <- safe_numeric(input$sg_published_n, 4, 1e6)
        req(pub, n_pub)
        pub_f <- if (isTRUE(input$sg_is_eta2)) eta2_to_f(pub) else pub
        sg <- safeguard_ci_d(pub_f * 2, n1 = n_pub / 2, n2 = n_pub / 2,
                              conf_level = input$sg_conf_level %||% 0.80,
                              one_sided = identical(input$sg_one_sided, "one"))
        max(sg$d_safeguard / 2, 1e-4)
      } else {
        mode <- input$sesoi_mode %||% "eta2"
        if (mode == "eta2") {
          eta2_to_f(safe_numeric(input$sesoi_eta2, 0.0001, 0.99, 0.02))
        } else if (mode == "raw") {
          req(input$sesoi_raw_diff, input$sesoi_sd)
          sesoi_raw_to_f_twolevel(input$sesoi_raw_diff, input$sesoi_sd)
        } else {
          safe_numeric(input$sesoi_f, 0.0001, 5, 0.15)
        }
      }
    })

    effect_branch_details <- reactive({
      branch <- input$es_branch %||% "sesoi"
      p <- params()
      if (branch == "cohen") {
        list(label = input$cohen_size, value = sprintf("f = %.3f", effect_value()))
      } else if (branch == "safeguard") {
        pub <- safe_numeric(input$sg_published_value, 0.0001, 5)
        n_pub <- safe_numeric(input$sg_published_n, 4, 1e6)
        pub_f <- if (isTRUE(input$sg_is_eta2)) eta2_to_f(pub) else pub
        naive <- tryCatch(power_anova_factorial_n(levels_a(), levels_b(), focal(), pub_f, 1, p$alpha, p$power)$n_total, error = function(e) NA)
        list(
          published_value = sprintf("f = %.3f%s", pub_f, if (isTRUE(input$sg_is_eta2)) sprintf(" (from eta² = %.3f)", pub) else ""),
          published_n = round(n_pub),
          conf_level = input$sg_conf_level %||% 0.80,
          one_sided = identical(input$sg_one_sided, "one"),
          safeguard_value = sprintf("f = %.3f", effect_value()),
          n_naive = naive,
          n_safeguard = tryCatch(power_anova_factorial_n(levels_a(), levels_b(), focal(), effect_value(), 1, p$alpha, p$power)$n_total, error = function(e) NA)
        )
      } else {
        list(description = sprintf("f = %.3f", effect_value()))
      }
    })

    result_r <- reactive({
      p <- params()
      req(effect_value())
      power_anova_factorial_n(levels_a = levels_a(), levels_b = levels_b(),
                               focal = focal(), f_target = effect_value(), sd = 1,
                               sig_level = p$alpha, power = p$power)
    })

    solve_n_fn <- function(sig_level, power, effect = NULL) {
      power_anova_factorial_n(levels_a = levels_a(), levels_b = levels_b(),
                               focal = focal(), f_target = effect %||% effect_value(), sd = 1,
                               sig_level = sig_level, power = power)
    }
    effect_set_r <- reactive(effect_comparison_values(effect_value(), "magnitude"))

    n_summary_r <- reactive({
      res <- result_r()
      tagList(
        tags$p(icon("users"), sprintf(" Total N = %d  (n per cell = %d × %d cells)", res$n_total, res$n_per_cell, res$n_cells)),
        tags$p(icon("bullseye"), sprintf(" Focal contrast: %s", switch(res$focal, A = "Main effect of Factor A", B = "Main effect of Factor B", `A:B` = "Interaction A × B"))),
        tags$p(icon("gauge-high"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target)),
        if (identical(res$focal, "A:B")) div(class = "well well-warning", icon("triangle-exclamation"), strong(" "), interaction_power_warning()) else NULL
      )
    })

    curve_extra_args_r <- reactive({
      p <- params()
      list(levels_a = levels_a(), levels_b = levels_b(), focal = focal(),
           f_target = effect_value(), sd = 1, sig_level = p$alpha)
    })
    n_solution_r <- reactive(result_r()$n_per_cell)

    sensitivity_fn <- function(n_max) {
      p <- params()
      sensitivity_min_effect("anova_factorial", n_max = round(n_max), levels_a = levels_a(),
                              levels_b = levels_b(), focal = focal(), sd = 1,
                              sig_level = p$alpha, power = p$power)
    }

    report_spec_r <- reactive({
      res <- result_r(); p <- params()
      design_txt <- if (is.null(levels_b())) {
        sprintf("a one-way between-subjects ANOVA with %d levels", levels_a())
      } else {
        sprintf("a %d×%d between-subjects factorial ANOVA, focal contrast = %s",
                levels_a(), levels_b(),
                switch(focal(), A = "main effect of Factor A", B = "main effect of Factor B", `A:B` = "the A×B interaction"))
      }
      list(
        analysis = "factorial between-subjects ANOVA",
        design = design_txt,
        effect_label_short = sprintf("an effect of Cohen's f = %.3f", effect_value()),
        alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
        tails = "two.sided",
        allocation = "Equal n per cell (required by the closed-form exact-power solution).",
        n_total = res$n_total,
        n_per_group_txt = sprintf("n per cell = %d (%d cells)", res$n_per_cell, res$n_cells),
        effect_branch = input$es_branch %||% "sesoi",
        effect_branch_details = effect_branch_details(),
        method_key = "anova_factorial",
        interaction_warning_shown = identical(focal(), "A:B")
      )
    })

    wire_results_server(input, output, session, family = "anova_factorial",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn, effect_set_r = effect_set_r)
  })
}
