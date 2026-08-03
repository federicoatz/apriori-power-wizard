## mod_ancova.R
## -----------------------------------------------------------------------
## Family module: one-way between-subjects ANCOVA (k groups, one
## continuous covariate), computed via pwr::pwr.f2.test with a
## covariate-adjusted f2 (see R/power_ancova.R). Structurally the closest
## sibling of mod_anova_factorial.R -- same Cohen's-f effect-size step --
## with the factorial (two-factor/interaction) complexity dropped and one
## extra Design input (the covariate-outcome correlation) added instead.
## -----------------------------------------------------------------------

mod_ancova_ui <- function(id) {
  ns <- NS(id)
  tagList(
    step_progress_ui(1, c("Design", "Parameters", "Effect size", "Results")),
    tabsetPanel(
      id = ns("wiz"), type = "hidden",
      tabPanelBody("design",
        h4(icon("table-cells"), " Step: Design structure"),
        p(class = "step-intro",
          "ANCOVA compares group means on an outcome while statistically",
          "adjusting for one continuous covariate measured on the same",
          "participants -- e.g., a pre-test/baseline score, a demographic",
          "variable, or a pre-registered nuisance variable known to",
          "correlate with the outcome."),
        guided_box("In plain language",
          p("ANCOVA is a between-subjects group comparison (like",
            "'Factorial ANOVA' with a single factor) with one twist: it",
            "also uses a covariate -- something you measured but didn't",
            "manipulate -- to soak up some of the noise in your outcome",
            "before comparing groups. A covariate that correlates well",
            "with the outcome (e.g., a pre-test score in a within-subject",
            "pretest/posttest design, or a baseline measure in a lab",
            "experiment) can substantially reduce the sample size needed,",
            "because it removes variance the groups would otherwise have",
            "to \"fight through\". A covariate that DOESN'T correlate with",
            "the outcome costs you a small amount of power for nothing --",
            "so only include one you actually expect to matter.")
        ),
        fluidRow(
          column(6, numericInput(ns("k_groups"),
                    help_tip("Number of groups", "How many conditions you're comparing, e.g., 2 for \"treatment vs. control\", 3 for \"low / medium / high\"."),
                    value = 2, min = 2, max = 8, step = 1)),
          column(6, numericInput(ns("r_cov"),
                    help_tip("Expected correlation between the covariate and the outcome",
                      "How strongly the covariate is expected to relate to the outcome, ignoring group. Common baseline/pre-test covariates in behavioral experiments often correlate around 0.3-0.6 with the outcome; 0 means the covariate isn't expected to help at all (and will cost a small amount of power for the degree of freedom it uses)."),
                    value = 0.30, min = 0, max = 0.99, step = 0.05))
        ),
        div(class = "field-hint", icon("circle-check"),
            " Power is computed for the group effect, adjusted for the covariate you specify."),
        wizard_nav_ui(ns, "design", show_back = FALSE)
      ),
      tabPanelBody("params",
        params_step_ui(ns),
        div(class = "field-hint", "Allocation ratio is not used for ANCOVA; group sizes are equal by design."),
        wizard_nav_ui(ns, "params")
      ),
      tabPanelBody("effect_size",
        effect_size_step_ui(
          ns,
          cohen_ui = radioButtons(ns("cohen_size"),
            help_tip("Benchmark", "Cohen's f measures how spread out the (unadjusted) condition means are, relative to within-condition noise. It's the same convention used for ANOVA -- the covariate's own contribution is handled separately, via the correlation you entered in the Design step."),
            choices = c("Small (f = 0.10)" = "small",
                        "Medium (f = 0.25)" = "medium",
                        "Large (f = 0.40)" = "large"),
            selected = "medium"),
          sesoi_ui = tagList(
            radioButtons(ns("sesoi_mode"), "Specify the SESOI as:",
              choices = c("Partial eta-squared" = "eta2",
                          "Raw mean difference + SD (two-group designs only)" = "raw",
                          "Already standardized (Cohen's f)" = "standardized"),
              selected = "eta2"),
            conditionalPanel(condition = sprintf("input['%s'] == 'eta2'", ns("sesoi_mode")),
              numericInput(ns("sesoi_eta2"), "Partial eta-squared for the group effect",
                           value = 0.02, min = 0.0001, max = 0.99, step = 0.005)
            ),
            conditionalPanel(condition = sprintf("input['%s'] == 'raw'", ns("sesoi_mode")),
              fluidRow(
                column(6, numericInput(ns("sesoi_raw_diff"), "Minimal difference of interest (raw units)", value = NA)),
                column(6, numericInput(ns("sesoi_sd"), "Expected (pooled) SD", value = NA, min = 0.0001))
              ),
              helpText("Exact only with 2 groups (f = d / 2). With more groups this is an approximation.")
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

mod_ancova_server <- function(id) {
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
    k <- reactive(max(2L, as.integer(safe_numeric(input$k_groups, 2, 8, 2))))
    r_cov <- reactive(safe_numeric(input$r_cov, 0, 0.99, 0.30))

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
        naive <- tryCatch(power_ancova_n(k(), pub_f, r_cov(), p$alpha, p$power)$n_total, error = function(e) NA)
        list(
          published_value = sprintf("f = %.3f%s", pub_f, if (isTRUE(input$sg_is_eta2)) sprintf(" (from eta² = %.3f)", pub) else ""),
          published_n = round(n_pub),
          conf_level = input$sg_conf_level %||% 0.80,
          one_sided = identical(input$sg_one_sided, "one"),
          safeguard_value = sprintf("f = %.3f", effect_value()),
          n_naive = naive,
          n_safeguard = tryCatch(power_ancova_n(k(), effect_value(), r_cov(), p$alpha, p$power)$n_total, error = function(e) NA)
        )
      } else {
        list(description = sprintf("f = %.3f", effect_value()))
      }
    })

    result_r <- reactive({
      p <- params()
      req(effect_value())
      power_ancova_n(k = k(), f_target = effect_value(), r_cov = r_cov(),
                      sig_level = p$alpha, power = p$power)
    })

    solve_n_fn <- function(sig_level, power) {
      power_ancova_n(k = k(), f_target = effect_value(), r_cov = r_cov(),
                      sig_level = sig_level, power = power)
    }

    n_summary_r <- reactive({
      res <- result_r()
      tagList(
        tags$p(icon("users"), sprintf(" Total N = %d  (n per group = %d × %d groups)", res$n_total, res$n_per_group, res$k)),
        tags$p(icon("bullseye"), sprintf(" Covariate-adjusted effect: f2 = %.3f (unadjusted f = %.3f, r with covariate = %.2f)", res$f2_adjusted, res$f, res$r_cov)),
        tags$p(icon("gauge-high"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target))
      )
    })

    curve_extra_args_r <- reactive({
      p <- params()
      list(k = k(), f_target = effect_value(), r_cov = r_cov(), sig_level = p$alpha)
    })
    n_solution_r <- reactive(result_r()$n_per_group)

    sensitivity_fn <- function(n_max) {
      p <- params()
      sensitivity_min_effect("ancova", n_max = round(n_max), k = k(),
                              r_cov = r_cov(), sig_level = p$alpha, power = p$power)
    }

    report_spec_r <- reactive({
      res <- result_r(); p <- params()
      list(
        analysis = "one-way ANCOVA",
        design = sprintf("a one-way ANCOVA with %d groups, adjusting for one covariate expected to correlate r = %.2f with the outcome",
                          k(), r_cov()),
        effect_label_short = sprintf("a group effect of Cohen's f = %.3f", effect_value()),
        alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
        tails = "two.sided",
        allocation = "Equal n per group (required by the closed-form exact-power solution).",
        n_total = res$n_total,
        n_per_group_txt = sprintf("n per group = %d (%d groups)", res$n_per_group, res$k),
        effect_branch = input$es_branch %||% "sesoi",
        effect_branch_details = effect_branch_details(),
        method_key = "ancova"
      )
    })

    wire_results_server(input, output, session, family = "ancova",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn)
  })
}
