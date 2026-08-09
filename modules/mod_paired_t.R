## mod_paired_t.R
## -----------------------------------------------------------------------
## Family module: paired-samples / repeated-measures comparison (each
## participant is measured under both conditions, e.g. before/after or two
## within-subjects conditions), analyzed with a paired-samples t-test.
## Internal sub-wizard: Design -> Parameters -> Effect size -> Results.
## -----------------------------------------------------------------------

mod_paired_t_ui <- function(id) {
  ns <- NS(id)
  tagList(
    step_progress_ui(1, c("Design", "Parameters", "Effect size", "Results")),
    tabsetPanel(
      id = ns("wiz"), type = "hidden",
      tabPanelBody("design",
        h3(icon("clock-rotate-left"), " Step: Design structure"),
        p(class = "step-intro",
          "The SAME participants are measured on a continuous outcome under",
          "two conditions (e.g., before and after an intervention, or two",
          "within-subjects conditions), and the two sets of scores are",
          "compared with a paired-samples t-test. Nothing to configure here",
          "beyond confirming this matches your study."),
        guided_box("In plain language",
          p("The key word is ", strong("SAME"), " participants, measured",
            "twice. Because each person acts as their own comparison,",
            "this design usually needs FEWER participants than comparing",
            "two separate groups of the same size would, for the same",
            "effect size -- individual differences between people wash",
            "out of the comparison entirely.")
        ),
        div(class = "well well-info", icon("lightbulb"),
            strong(" Example: "), "the same participants complete a task",
            "once before a training program and once after it -- that's",
            "exactly this design."),
        div(class = "field-hint", icon("circle-info"),
            " Because both measurements come from the same people, this design is usually more efficient (needs fewer participants) than comparing two independent groups for the same underlying effect size."),
        wizard_nav_ui(ns, "design", show_back = FALSE)
      ),
      tabPanelBody("params",
        params_step_ui(ns),
        div(class = "field-hint", "Allocation ratio doesn't apply here: there's a single group of participants, each measured twice."),
        wizard_nav_ui(ns, "params")
      ),
      tabPanelBody("effect_size",
        effect_size_step_ui(
          ns,
          cohen_ui = radioButtons(ns("cohen_size"),
            help_tip("Benchmark", "d here is the difference-score standardized effect (dz): mean(difference) / SD(difference), NOT the same scale as an independent-samples d for the same study. As a rule of thumb, 0.2 is a small gap between the two conditions, 0.5 is visible to the naked eye, 0.8 is large."),
            choices = c("Small (dz = 0.20)" = "small",
                        "Medium (dz = 0.50)" = "medium",
                        "Large (dz = 0.80)" = "large"),
            selected = "medium"),
          sesoi_ui = tagList(
            radioButtons(ns("sesoi_mode"), "Specify the SESOI as:",
              choices = c("Raw mean difference + SD of the differences" = "raw",
                          "Already standardized (dz)" = "standardized"),
              selected = "standardized"),
            conditionalPanel(condition = sprintf("input['%s'] == 'raw'", ns("sesoi_mode")),
              fluidRow(
                column(6, numericInput(ns("sesoi_raw_diff"),
                         help_tip("Minimal difference of interest (raw units)", "In the outcome's own units -- e.g., '2 dollars', '150 milliseconds', '0.5 points on the scale'."),
                         value = NA)),
                column(6, numericInput(ns("sesoi_sd"),
                         help_tip("Expected SD of the DIFFERENCE scores", "How spread out each person's own before-after (or condition A minus condition B) change typically is -- not the SD of the raw scores themselves. If you have pilot data, use the SD of the paired differences."),
                         value = NA, min = 0.0001))
              )
            ),
            conditionalPanel(condition = sprintf("input['%s'] == 'standardized'", ns("sesoi_mode")),
              numericInput(ns("sesoi_d"), "Smallest effect size of interest (dz)", value = 0.3, step = 0.05)
            )
          ),
          safeguard_metric_label = "Published dz (difference-score standardized d)"
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

mod_paired_t_server <- function(id) {
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
        # safeguard_ci_dz(), not safeguard_ci_d() with the total split in
        # half: this design has n PAIRS, not two independent groups of
        # n/2, and the two variances differ by a factor of about four in
        # the leading term. See R/safeguard_power.R for what the old route
        # cost (980 pairs where 138 are needed, at d_z = 0.40 from 30).
        sg <- safeguard_ci_dz(pub, n_pairs = n_pub,
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
        list(label = input$cohen_size, value = sprintf("dz = %.2f", effect_value()))
      } else if (branch == "safeguard") {
        pub <- safe_numeric(input$sg_published_value, 0.0001, 5)
        n_pub <- safe_numeric(input$sg_published_n, 4, 1e6)
        naive <- tryCatch(power_paired_t_n(pub, p$alpha, p$power, p$tails)$n_total, error = function(e) NA)
        list(
          published_value = sprintf("dz = %.3f", pub), published_n = round(n_pub),
          conf_level = input$sg_conf_level %||% 0.80,
          one_sided = identical(input$sg_one_sided, "one"),
          safeguard_value = sprintf("dz = %.3f", effect_value()),
          n_naive = naive,
          n_safeguard = tryCatch(power_paired_t_n(effect_value(), p$alpha, p$power, p$tails)$n_total, error = function(e) NA)
        )
      } else {
        list(description = sprintf("dz = %.3f", effect_value()))
      }
    })

    result_r <- reactive({
      p <- params()
      req(effect_value())
      power_paired_t_n(d = effect_value(), sig_level = p$alpha, power = p$power,
                        alternative = p$tails)
    })

    solve_n_fn <- function(sig_level, power, effect = NULL) {
      p <- params()
      power_paired_t_n(d = effect %||% effect_value(), sig_level = sig_level, power = power,
                        alternative = p$tails)
    }
    effect_set_r <- reactive(effect_comparison_values(effect_value(), "magnitude"))

    n_summary_r <- reactive({
      res <- result_r()
      tagList(
        tags$p(icon("users"), sprintf(" Participants: n = %d (each measured twice, %d total observations)", res$n_pairs, res$n_pairs * 2)),
        tags$p(icon("bullseye"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target))
      )
    })

    curve_extra_args_r <- reactive({
      p <- params()
      list(d = effect_value(), sig_level = p$alpha, alternative = p$tails)
    })

    n_solution_r <- reactive(result_r()$n_pairs)

    sensitivity_fn <- function(n_max) {
      p <- params()
      sensitivity_min_effect("paired_t", n_max = n_max, sig_level = p$alpha,
                              power = p$power, alternative = p$tails)
    }

    report_spec_r <- reactive({
      res <- result_r(); p <- params()
      list(
        analysis = "paired-samples comparison (paired-samples t-test)",
        design = "a within-subjects design in which the same participants are measured under two conditions",
        effect_label_short = sprintf("a difference-score effect of dz = %.3f", effect_value()),
        alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
        tails = p$tails,
        allocation = "Each participant serves as their own control; no allocation ratio applies.",
        n_total = res$n_pairs,
        n_per_group_txt = sprintf("n = %d pairs (%d total observations)", res$n_pairs, res$n_pairs * 2),
        effect_branch = input$es_branch %||% "sesoi",
        effect_branch_details = effect_branch_details(),
        method_key = "paired_t"
      )
    })

    wire_results_server(input, output, session, family = "paired_t",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn, effect_set_r = effect_set_r)
  })
}
