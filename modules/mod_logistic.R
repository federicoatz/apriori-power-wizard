## mod_logistic.R
## -----------------------------------------------------------------------
## Family module: logistic regression with a single focal predictor
## (binary or continuous/normal). Effect size is expressed as two outcome
## probabilities (p0 at the reference level, p1 at the shifted level),
## which jointly imply an odds ratio.
## -----------------------------------------------------------------------

mod_logistic_ui <- function(id) {
  ns <- NS(id)
  tagList(
    step_progress_ui(1, c("Design", "Parameters", "Effect size", "Results")),
    tabsetPanel(
      id = ns("wiz"), type = "hidden",
      tabPanelBody("design",
        h3(icon("toggle-on"), " Step: Design structure"),
        p(class = "step-intro",
          "A single focal predictor of a binary (yes/no) outcome, tested via",
          "logistic regression (Demidenko, 2007)."),
        guided_box("In plain language",
          p("Use this whenever your OUTCOME is a yes/no (or",
            "success/failure) variable rather than a continuous number,",
            "and you're testing whether some predictor relates to it. If",
            "your predictor is instead the yes/no variable and you're",
            "comparing two independent groups' rates directly, 'Two",
            "proportions' is the simpler, more standard choice for that",
            "specific case.")
        ),
        fluidRow(
          column(6, radioButtons(ns("predictor_dist"),
                                  help_tip("Focal predictor type", "Binary if the predictor is a category (e.g., treated vs. not); continuous/standardized if it's a numeric variable like age or a survey score (rescaled to mean 0, SD 1)."),
                                  choices = c("Binary (e.g., treatment indicator)" = "binary",
                                              "Continuous, standardized (mean 0, SD 1)" = "normal"),
                                  selected = "binary")),
          column(6, conditionalPanel(condition = sprintf("input['%s'] == 'binary'", ns("predictor_dist")),
                    numericInput(ns("prop_predictor"),
                                 help_tip("Proportion with predictor = 1", "What share of your sample will fall into the '1' category -- 0.5 for a balanced random assignment."),
                                 value = 0.5, min = 0.01, max = 0.99, step = 0.01)))
        ),
        numericInput(ns("p0"),
                     help_tip("Baseline outcome probability (predictor = 0 / at the mean)", "How often the outcome happens in the reference group -- e.g., your current conversion rate without the intervention."),
                     value = 0.20, min = 0.001, max = 0.999, step = 0.01),
        div(class = "well well-info", icon("lightbulb"),
            strong(" Example: "), "does receiving a reminder email (binary predictor)",
            "predict whether someone completes a task (binary outcome)?"),
        wizard_nav_ui(ns, "design", show_back = FALSE)
      ),
      tabPanelBody("params",
        params_step_ui(ns),
        wizard_nav_ui(ns, "params")
      ),
      tabPanelBody("effect_size",
        effect_size_step_ui(
          ns,
          cohen_ui = radioButtons(ns("cohen_size"), "Benchmark (approximate odds ratio, Chinn 2000)",
            choices = c("Small (OR ≈ 1.68)" = "small",
                        "Medium (OR ≈ 3.47)" = "medium",
                        "Large (OR ≈ 6.71)" = "large"),
            selected = "medium"),
          sesoi_ui = tagList(
            radioButtons(ns("sesoi_mode"), "Specify the SESOI as:",
              choices = c("Two outcome probabilities (p0, p1)" = "raw",
                          "Odds ratio (applied to the baseline p0 above)" = "or"),
              selected = "raw"),
            conditionalPanel(condition = sprintf("input['%s'] == 'raw'", ns("sesoi_mode")),
              numericInput(ns("sesoi_p1"), "Minimal outcome probability of interest (p1)",
                           value = 0.35, min = 0.001, max = 0.999, step = 0.01)
            ),
            conditionalPanel(condition = sprintf("input['%s'] == 'or'", ns("sesoi_mode")),
              numericInput(ns("sesoi_or"), "Smallest odds ratio of interest", value = 1.8, min = 1.001, step = 0.1)
            )
          ),
          safeguard_metric_label = "Published odds ratio"
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

mod_logistic_server <- function(id) {
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
    p0 <- reactive(safe_numeric(input$p0, 0.001, 0.999, 0.20))
    predictor_dist <- reactive(input$predictor_dist %||% "binary")
    prop_predictor <- reactive(safe_numeric(input$prop_predictor, 0.01, 0.99, 0.5))

    or_to_p1 <- function(p0, or) (or * p0) / (1 - p0 + or * p0)

    p1_value <- reactive({
      branch <- input$es_branch %||% "sesoi"
      base <- p0()
      if (branch == "cohen") {
        or <- unname(cohen_benchmarks("or")[input$cohen_size %||% "medium"])
        or_to_p1(base, or)
      } else if (branch == "safeguard") {
        or_pub <- safe_numeric(input$sg_published_value, 1.001, 1000)
        n_pub <- safe_numeric(input$sg_published_n, 5, 1e6)
        req(or_pub, n_pub)
        d_pub <- or_to_d(or_pub)
        sg <- safeguard_ci_d(d_pub, n1 = n_pub / 2, n2 = n_pub / 2,
                              conf_level = input$sg_conf_level %||% 0.80,
                              one_sided = identical(input$sg_one_sided, "one"))
        or_safeguard <- d_to_or(sg$d_safeguard)
        or_to_p1(base, or_safeguard)
      } else {
        if (identical(input$sesoi_mode, "or")) {
          or_to_p1(base, safe_numeric(input$sesoi_or, 1.001, 1000, 1.8))
        } else {
          safe_numeric(input$sesoi_p1, 0.001, 0.999, 0.35)
        }
      }
    })

    effect_branch_details <- reactive({
      branch <- input$es_branch %||% "sesoi"
      p <- params()
      or_val <- probs_to_or(p0(), p1_value())
      if (branch == "cohen") {
        list(label = input$cohen_size, value = sprintf("OR ≈ %.2f (p0=%.2f, p1=%.2f)", or_val, p0(), p1_value()))
      } else if (branch == "safeguard") {
        or_pub <- safe_numeric(input$sg_published_value, 1.001, 1000)
        n_pub <- safe_numeric(input$sg_published_n, 5, 1e6)
        p1_naive <- or_to_p1(p0(), or_pub)
        naive <- tryCatch(power_logistic_n(p0(), p1_naive, predictor_dist(), prop_predictor(), p$alpha, p$power, p$tails)$n_total, error = function(e) NA)
        list(
          published_value = sprintf("OR = %.2f", or_pub), published_n = round(n_pub),
          conf_level = input$sg_conf_level %||% 0.80,
          one_sided = identical(input$sg_one_sided, "one"),
          safeguard_value = sprintf("OR ≈ %.2f (p1 ≈ %.3f)", or_val, p1_value()),
          n_naive = naive,
          n_safeguard = tryCatch(power_logistic_n(p0(), p1_value(), predictor_dist(), prop_predictor(), p$alpha, p$power, p$tails)$n_total, error = function(e) NA)
        )
      } else {
        list(description = sprintf("p0 = %.3f, p1 = %.3f (OR ≈ %.2f)", p0(), p1_value(), or_val))
      }
    })

    result_r <- reactive({
      p <- params()
      req(p1_value())
      power_logistic_n(p0 = p0(), p1 = p1_value(), predictor_dist = predictor_dist(),
                        prop_predictor = prop_predictor(), sig_level = p$alpha,
                        power = p$power, alternative = p$tails)
    })

    solve_n_fn <- function(sig_level, power) {
      p <- params()
      power_logistic_n(p0 = p0(), p1 = p1_value(), predictor_dist = predictor_dist(),
                        prop_predictor = prop_predictor(), sig_level = sig_level,
                        power = power, alternative = p$tails)
    }

    n_summary_r <- reactive({
      res <- result_r()
      tagList(
        tags$p(icon("percent"), sprintf(" p0 = %.3f, p1 = %.3f, odds ratio ≈ %.2f", res$p0, res$p1, res$odds_ratio)),
        tags$p(icon("bullseye"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target))
      )
    })

    curve_extra_args_r <- reactive({
      p <- params()
      list(p0 = p0(), p1 = p1_value(), predictor_dist = predictor_dist(),
           prop_predictor = prop_predictor(), sig_level = p$alpha, alternative = p$tails)
    })
    n_solution_r <- reactive(result_r()$n_total)

    sensitivity_fn <- function(n_max) {
      p <- params()
      sensitivity_min_effect("logistic", n_max = n_max, p0 = p0(),
                              predictor_dist = predictor_dist(), prop_predictor = prop_predictor(),
                              sig_level = p$alpha, power = p$power, alternative = p$tails,
                              direction = if (p1_value() >= p0()) "greater" else "less")
    }

    report_spec_r <- reactive({
      res <- result_r(); p <- params()
      list(
        analysis = "logistic regression (single focal predictor)",
        design = sprintf("a logistic regression with a %s focal predictor of a binary outcome",
                          if (predictor_dist() == "binary") "binary" else "continuous (standardized)"),
        effect_label_short = sprintf("p0 = %.3f vs. p1 = %.3f (OR ≈ %.2f)", res$p0, res$p1, res$odds_ratio),
        alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
        tails = p$tails,
        allocation = if (predictor_dist() == "binary") sprintf("The focal predictor was assumed Bernoulli-distributed with P(X=1) = %.2f.", prop_predictor()) else "The focal predictor was assumed standard normal.",
        n_total = res$n_total,
        n_per_group_txt = sprintf("N = %d", res$n_total),
        effect_branch = input$es_branch %||% "sesoi",
        effect_branch_details = effect_branch_details(),
        method_key = "logistic"
      )
    })

    wire_results_server(input, output, session, family = "logistic",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn)
  })
}
