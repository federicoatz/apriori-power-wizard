## mod_chisq.R
## -----------------------------------------------------------------------
## Family module: chi-square test for categorical data -- either a single
## categorical variable against a reference distribution (goodness-of-fit)
## or two categorical variables crossed in a contingency table (test of
## independence). Both share the same power formula (R/power_chisq.R)
## once degrees of freedom are known; only how df is computed from the
## design differs, so a single "Design type" choice up front covers both.
## -----------------------------------------------------------------------

mod_chisq_ui <- function(id) {
  ns <- NS(id)
  tagList(
    step_progress_ui(1, c("Design", "Parameters", "Effect size", "Results")),
    tabsetPanel(
      id = ns("wiz"), type = "hidden",
      tabPanelBody("design",
        h4(icon("chart-pie"), " Step: Design structure"),
        p(class = "step-intro",
          "Compares counts or proportions across categories with a",
          "chi-square test: either one categorical variable against a",
          "reference distribution, or two categorical variables crossed",
          "in a contingency table."),
        guided_box("In plain language",
          p("Use this whenever your outcome is a CATEGORY (not a number,",
            "and not just yes/no) -- e.g., which of several options",
            "someone picked -- and you're comparing that across groups.",
            "Most experiments with several treatment arms and a",
            "categorical choice as the outcome want ", strong("test of",
            "independence"), " (the default below): does the distribution",
            "of choices differ across arms?")
        ),
        radioButtons(ns("chisq_design"),
          help_tip("Design type", "Goodness-of-fit asks 'does this one variable's distribution across categories match a reference (e.g., an even split, or a known population distribution)?'. Test of independence asks 'are these two categorical variables related?' (e.g., treatment group x choice made) -- this is the more common case in experiments with several treatment arms and a categorical outcome."),
          choices = c("Test of independence (two categorical variables / contingency table)" = "independence",
                      "Goodness-of-fit (one categorical variable vs. a reference distribution)" = "gof"),
          selected = "independence"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'independence'", ns("chisq_design")),
          fluidRow(
            column(6, numericInput(ns("n_rows"),
                      help_tip("Number of rows", "E.g., the number of treatment groups or conditions being compared."),
                      value = 4, min = 2, max = 30, step = 1)),
            column(6, numericInput(ns("n_cols"),
                      help_tip("Number of columns", "E.g., the number of possible categorical response values (2 for a yes/no choice among 3+ groups -- for exactly 2x2, the dedicated 'Two proportions' family gives you Cohen's h and a more familiar effect-size scale)."),
                      value = 2, min = 2, max = 30, step = 1))
          ),
          div(class = "field-hint", icon("circle-info"),
              " Example: 4 treatment arms (rows) x 2 choices made (columns) -- exactly the omnibus test for \"does the distribution of choices differ across treatments?\"")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'gof'", ns("chisq_design")),
          numericInput(ns("n_categories"),
            help_tip("Number of categories", "How many categories the single variable can fall into, e.g., 4 for four possible choices."),
            value = 4, min = 2, max = 30, step = 1),
          div(class = "field-hint", icon("circle-info"),
              " Example: testing whether choices are evenly split across 4 options, or match a known reference distribution.")
        ),
        uiOutput(ns("df_display")),
        div(class = "field-hint", icon("circle-check"),
            " Power is computed for the omnibus chi-square test only -- not for any single cell or pairwise follow-up comparison."),
        wizard_nav_ui(ns, "design", show_back = FALSE)
      ),
      tabPanelBody("params",
        params_step_ui(ns),
        div(class = "field-hint",
            "Test direction and allocation ratio are not used for the chi-square test: it is inherently non-directional (omnibus), and N is a single total split across cells rather than allocated between two groups."),
        wizard_nav_ui(ns, "params")
      ),
      tabPanelBody("effect_size",
        effect_size_step_ui(
          ns,
          cohen_ui = radioButtons(ns("cohen_size"),
            help_tip("Benchmark", "Cohen's w measures the overall discrepancy between the observed and expected (or independent) distribution across all cells of the design."),
            choices = c("Small (w = 0.10)" = "small",
                        "Medium (w = 0.30)" = "medium",
                        "Large (w = 0.50)" = "large"),
            selected = "medium"),
          sesoi_ui = numericInput(ns("sesoi_w"),
            "Smallest effect size of interest (Cohen's w)", value = 0.3, min = 0.0001, step = 0.01),
          safeguard_metric_label = "Published Cohen's w"
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

mod_chisq_server <- function(id) {
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

    is_gof <- reactive(identical(input$chisq_design %||% "independence", "gof"))

    df_val <- reactive({
      if (is_gof()) {
        k <- max(2L, as.integer(safe_numeric(input$n_categories, 2, 1000, 4)))
        k - 1L
      } else {
        r <- max(2L, as.integer(safe_numeric(input$n_rows, 2, 1000, 4)))
        cl <- max(2L, as.integer(safe_numeric(input$n_cols, 2, 1000, 2)))
        (r - 1L) * (cl - 1L)
      }
    })

    output$df_display <- renderUI({
      div(class = "field-hint", icon("circle-check"),
          sprintf(" Degrees of freedom for this design: df = %d.", df_val()))
    })

    effect_value <- reactive({
      branch <- input$es_branch %||% "sesoi"
      if (branch == "cohen") {
        unname(cohen_benchmarks("w")[input$cohen_size %||% "medium"])
      } else if (branch == "safeguard") {
        pub <- safe_numeric(input$sg_published_value, 0.0001, 5)
        n_pub <- safe_numeric(input$sg_published_n, 4, 1e6)
        req(pub, n_pub)
        sg <- safeguard_ci_w(pub, n_published = n_pub, df = df_val(),
                              conf_level = input$sg_conf_level %||% 0.80,
                              one_sided = identical(input$sg_one_sided, "one"))
        sg$w_safeguard
      } else {
        safe_numeric(input$sesoi_w, 0.0001, 5, 0.3)
      }
    })

    effect_branch_details <- reactive({
      branch <- input$es_branch %||% "sesoi"
      p <- params()
      if (branch == "cohen") {
        list(label = input$cohen_size, value = sprintf("w = %.2f", effect_value()))
      } else if (branch == "safeguard") {
        pub <- safe_numeric(input$sg_published_value, 0.0001, 5)
        n_pub <- safe_numeric(input$sg_published_n, 4, 1e6)
        naive <- tryCatch(power_chisq_n(pub, df_val(), p$alpha, p$power)$n_total, error = function(e) NA)
        list(
          published_value = sprintf("w = %.3f", pub), published_n = round(n_pub),
          conf_level = input$sg_conf_level %||% 0.80,
          one_sided = identical(input$sg_one_sided, "one"),
          safeguard_value = sprintf("w = %.3f", effect_value()),
          n_naive = naive,
          n_safeguard = tryCatch(power_chisq_n(effect_value(), df_val(), p$alpha, p$power)$n_total, error = function(e) NA)
        )
      } else {
        list(description = sprintf("w = %.3f", effect_value()))
      }
    })

    result_r <- reactive({
      p <- params()
      req(effect_value())
      power_chisq_n(w = effect_value(), df = df_val(), sig_level = p$alpha, power = p$power)
    })

    solve_n_fn <- function(sig_level, power, effect = NULL) {
      power_chisq_n(w = effect %||% effect_value(), df = df_val(), sig_level = sig_level, power = power)
    }
    effect_set_r <- reactive(effect_comparison_values(effect_value(), "magnitude"))

    n_summary_r <- reactive({
      res <- result_r()
      tagList(
        tags$p(icon("users"), sprintf(" Total N = %d (df = %d)", res$n_total, res$df)),
        tags$p(icon("bullseye"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target))
      )
    })

    curve_extra_args_r <- reactive({
      list(w = effect_value(), df = df_val(), sig_level = params()$alpha)
    })
    n_solution_r <- reactive(result_r()$n_total)

    sensitivity_fn <- function(n_max) {
      p <- params()
      sensitivity_min_effect("chisq", n_max = n_max, df = df_val(),
                              sig_level = p$alpha, power = p$power)
    }

    report_spec_r <- reactive({
      res <- result_r(); p <- params()
      design_desc <- if (is_gof()) {
        sprintf("a chi-square goodness-of-fit test with %d categories (df = %d) against a reference distribution",
                round(safe_numeric(input$n_categories, 2, 1000, 4)), res$df)
      } else {
        sprintf("a chi-square test of independence in a %d x %d contingency table (df = %d)",
                round(safe_numeric(input$n_rows, 2, 1000, 4)), round(safe_numeric(input$n_cols, 2, 1000, 2)), res$df)
      }
      list(
        analysis = if (is_gof()) "chi-square goodness-of-fit test" else "chi-square test of independence",
        design = design_desc,
        effect_label_short = sprintf("an effect of Cohen's w = %.3f", effect_value()),
        alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
        tails = "none",
        allocation = "N is a single total sample size, distributed across all cells of the design rather than allocated between two groups.",
        n_total = res$n_total,
        n_per_group_txt = sprintf("total N = %d across the design's cells", res$n_total),
        effect_branch = input$es_branch %||% "sesoi",
        effect_branch_details = effect_branch_details(),
        method_key = "chisq"
      )
    })

    wire_results_server(input, output, session, family = "chisq",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn, effect_set_r = effect_set_r)
  })
}
