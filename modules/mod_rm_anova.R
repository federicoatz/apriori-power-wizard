## mod_rm_anova.R
## -----------------------------------------------------------------------
## Family module: repeated-measures ANOVA (see R/power_rm_anova.R for the
## noncentrality/df formulation and its Monte Carlo + paired-t validation).
## Handles both the pure within-subjects main effect and the within x
## between interaction of a mixed design.
##
## Uses the standard three-branch Cohen/safeguard/SESOI effect-size step on
## Cohen's f, exactly like the between-subjects factorial ANOVA family --
## what makes this family different lives in the Design step (number of
## repeated measurements, their correlation, the sphericity correction),
## not in how the effect size is chosen.
## -----------------------------------------------------------------------

mod_rm_anova_ui <- function(id) {
  ns <- NS(id)
  tagList(
    step_progress_ui(1, c("Design", "Parameters", "Effect size", "Results")),
    tabsetPanel(
      id = ns("wiz"), type = "hidden",
      tabPanelBody("design",
        h4(icon("repeat"), " Step: Design structure"),
        p(class = "step-intro",
          "The SAME participants are measured several times -- across",
          "conditions, rounds, or time points. Measuring each person",
          "repeatedly removes the differences between people from the error",
          "term, which is why these designs usually need far fewer",
          "participants than the equivalent between-subjects study."),
        guided_box("In plain language",
          p("If every participant experiences every condition, you can",
            "compare each person to themselves rather than to other people.",
            "That removes the single biggest source of noise in most",
            "studies -- the fact that people simply differ from one",
            "another -- so you need fewer of them."),
          p("How much fewer depends almost entirely on ", strong("how",
            "strongly a participant's own measurements correlate with each",
            "other"), " (the value you enter below). A participant who",
            "scores high in one condition usually scoring high in the",
            "others means a high correlation and a big saving; if the",
            "measurements are essentially unrelated, the saving disappears",
            "and you are back to roughly a between-subjects sample size.",
            "This is the input that matters most here, so it is worth",
            "getting from pilot data rather than guessing.")
        ),
        radioButtons(ns("effect_type"),
          help_tip("Which effect are you powering for?",
            "The within-subject main effect asks whether the average changes across conditions/rounds at all. The interaction asks whether that change DIFFERS between independent groups -- the usual hypothesis in a pretest/posttest-by-treatment design."),
          choices = c("Main effect of the repeated factor (one group measured repeatedly)" = "within",
                      "Interaction: does the change across conditions differ between groups?" = "interaction"),
          selected = "within"),
        fluidRow(
          column(4, numericInput(ns("m_measures"),
                    help_tip("Number of repeated measurements", "How many times each participant is measured -- conditions, rounds, or time points. Must be at least 2; with exactly 2 this is equivalent to a paired t-test."),
                    value = 3, min = 2, max = 50, step = 1)),
          column(4, conditionalPanel(
            condition = sprintf("input['%s'] == 'interaction'", ns("effect_type")),
            numericInput(ns("n_groups"),
              help_tip("Number of independent groups", "How many separate between-subject groups (e.g., treatment vs. control) each measured repeatedly."),
              value = 2, min = 2, max = 10, step = 1))),
          column(4, numericInput(ns("rho"),
                    help_tip("Correlation among the repeated measurements",
                      "How strongly a participant's measurements agree with each other. Higher means a bigger saving in sample size. Estimate this from pilot data or a prior study using the same measure whenever possible -- it drives the answer more than anything else on this page. If you genuinely have nothing to go on, a moderate value around 0.5 is a common starting point, and lower values are the conservative direction."),
                    value = 0.5, min = 0, max = 0.99, step = 0.05))
        ),
        accordion(
          id = ns("rm_advanced_acc"), open = FALSE,
          accordion_panel(
            title = tagList(icon("sliders"), " Advanced: sphericity correction"),
            value = "advanced",
            numericInput(ns("eps"),
              help_tip("Nonsphericity correction (epsilon)",
                "Repeated-measures ANOVA assumes 'sphericity': that all pairs of measurements are equally correlated. When they aren't, the test is corrected (Greenhouse-Geisser), which costs power. 1 means sphericity holds exactly; the lowest possible value is 1/(m-1). Leave at 1 unless you have a specific reason, and note that lower values are the conservative choice."),
              value = 1, min = 0.05, max = 1, step = 0.05),
            div(class = "field-hint", icon("circle-info"),
                " With only 2 repeated measurements sphericity is automatic and this has no effect.")
          )
        ),
        div(class = "field-hint", icon("circle-check"),
            " Power is computed for the single effect selected above, not for a generic omnibus test."),
        wizard_nav_ui(ns, "design", show_back = FALSE)
      ),
      tabPanelBody("params",
        params_step_ui(ns),
        div(class = "field-hint", "Test direction and allocation ratio are not used here; the F test is inherently non-directional and groups are assumed equally sized."),
        wizard_nav_ui(ns, "params")
      ),
      tabPanelBody("effect_size",
        effect_size_step_ui(
          ns,
          cohen_ui = radioButtons(ns("cohen_size"),
            help_tip("Benchmark", "Cohen's f for the effect being tested, on the same scale as a between-subjects ANOVA. The within-subject correlation you entered in the Design step is applied separately -- do NOT inflate f yourself to account for it."),
            choices = c("Small (f = 0.10)" = "small",
                        "Medium (f = 0.25)" = "medium",
                        "Large (f = 0.40)" = "large"),
            selected = "medium"),
          sesoi_ui = tagList(
            radioButtons(ns("sesoi_mode"), "Specify the SESOI as:",
              choices = c("Partial eta-squared" = "eta2",
                          "Already standardized (Cohen's f)" = "standardized"),
              selected = "eta2"),
            conditionalPanel(condition = sprintf("input['%s'] == 'eta2'", ns("sesoi_mode")),
              numericInput(ns("sesoi_eta2"), "Partial eta-squared for the effect",
                           value = 0.06, min = 0.0001, max = 0.99, step = 0.005)
            ),
            conditionalPanel(condition = sprintf("input['%s'] == 'standardized'", ns("sesoi_mode")),
              numericInput(ns("sesoi_f"), "Smallest effect size of interest (Cohen's f)",
                           value = 0.25, min = 0.0001, step = 0.01)
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

mod_rm_anova_server <- function(id) {
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
    effect_type <- reactive(input$effect_type %||% "within")
    m_measures <- reactive(max(2L, as.integer(safe_numeric(input$m_measures, 2, 50, 3))))
    n_groups <- reactive({
      if (identical(effect_type(), "interaction")) {
        max(2L, as.integer(safe_numeric(input$n_groups, 2, 10, 2)))
      } else {
        1L
      }
    })
    rho <- reactive(safe_numeric(input$rho, 0, 0.99, 0.5))
    # Epsilon can never be below 1/(m-1); clamp rather than error, so a
    # stale value left over from a larger m can't wedge the results step.
    eps <- reactive({
      e <- safe_numeric(input$eps, 0.05, 1, 1)
      max(e, 1 / (m_measures() - 1))
    })

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
        if (identical(input$sesoi_mode %||% "eta2", "eta2")) {
          eta2_to_f(safe_numeric(input$sesoi_eta2, 0.0001, 0.99, 0.06))
        } else {
          safe_numeric(input$sesoi_f, 0.0001, 5, 0.25)
        }
      }
    })

    effect_branch_details <- reactive({
      branch <- input$es_branch %||% "sesoi"
      p <- params()
      solve_for <- function(f) {
        tryCatch(power_rm_anova_n(m_measures(), f, rho(), effect_type(),
                                   n_groups(), eps(), p$alpha, p$power)$n_total,
                 error = function(e) NA)
      }
      if (branch == "cohen") {
        list(label = input$cohen_size, value = sprintf("f = %.3f", effect_value()))
      } else if (branch == "safeguard") {
        pub <- safe_numeric(input$sg_published_value, 0.0001, 5)
        n_pub <- safe_numeric(input$sg_published_n, 4, 1e6)
        pub_f <- if (isTRUE(input$sg_is_eta2)) eta2_to_f(pub) else pub
        list(
          published_value = sprintf("f = %.3f%s", pub_f,
                                     if (isTRUE(input$sg_is_eta2)) sprintf(" (from eta² = %.3f)", pub) else ""),
          published_n = round(n_pub),
          conf_level = input$sg_conf_level %||% 0.80,
          one_sided = identical(input$sg_one_sided, "one"),
          safeguard_value = sprintf("f = %.3f", effect_value()),
          n_naive = solve_for(pub_f),
          n_safeguard = solve_for(effect_value())
        )
      } else {
        list(description = sprintf("f = %.3f", effect_value()))
      }
    })

    result_r <- reactive({
      p <- params()
      req(effect_value())
      power_rm_anova_n(m = m_measures(), f_target = effect_value(), rho = rho(),
                        effect = effect_type(), n_groups = n_groups(), eps = eps(),
                        sig_level = p$alpha, power = p$power)
    })

    solve_n_fn <- function(sig_level, power, effect = NULL) {
      power_rm_anova_n(m = m_measures(), f_target = effect %||% effect_value(),
                        rho = rho(), effect = effect_type(), n_groups = n_groups(),
                        eps = eps(), sig_level = sig_level, power = power)
    }
    effect_set_r <- reactive(effect_comparison_values(effect_value(), "magnitude"))

    n_summary_r <- reactive({
      res <- result_r()
      tagList(
        tags$p(icon("users"), sprintf(" Participants: %d%s", res$n_total,
                                       if (res$n_groups > 1) sprintf(" (%d per group x %d groups)", res$n_per_group, res$n_groups) else "")),
        tags$p(icon("repeat"), sprintf(" Each measured %d times -- %d observations in total", res$m, res$n_observations)),
        tags$p(icon("link"), sprintf(" Assumed correlation among repeated measures: %.2f%s",
                                      res$rho, if (res$eps < 1) sprintf("; sphericity correction eps = %.2f", res$eps) else "")),
        tags$p(icon("bullseye"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target))
      )
    })

    curve_extra_args_r <- reactive({
      p <- params()
      list(m = m_measures(), f_target = effect_value(), rho = rho(),
           effect = effect_type(), n_groups = n_groups(), eps = eps(),
           sig_level = p$alpha)
    })
    n_solution_r <- reactive(result_r()$n_total)

    sensitivity_fn <- function(n_max) {
      p <- params()
      sensitivity_min_effect("rm_anova", n_max = round(n_max), m = m_measures(),
                              rho = rho(), effect = effect_type(),
                              n_groups = n_groups(), eps = eps(),
                              sig_level = p$alpha, power = p$power)
    }

    report_spec_r <- reactive({
      res <- result_r(); p <- params()
      design_txt <- if (identical(effect_type(), "interaction")) {
        sprintf("a mixed design with %d independent groups, each measured %d times, testing the within x between interaction (assumed correlation among repeated measures = %.2f%s)",
                res$n_groups, res$m, res$rho,
                if (res$eps < 1) sprintf(", Greenhouse-Geisser epsilon = %.2f", res$eps) else "")
      } else {
        sprintf("a within-subjects design in which each participant is measured %d times (assumed correlation among repeated measures = %.2f%s)",
                res$m, res$rho,
                if (res$eps < 1) sprintf(", Greenhouse-Geisser epsilon = %.2f", res$eps) else "")
      }
      list(
        analysis = if (identical(effect_type(), "interaction")) {
          "repeated-measures ANOVA (within x between interaction)"
        } else {
          "repeated-measures ANOVA (within-subject main effect)"
        },
        design = design_txt,
        effect_label_short = sprintf("an effect of Cohen's f = %.3f", effect_value()),
        alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
        tails = "none",
        allocation = if (res$n_groups > 1) {
          "Groups were assumed to be equally sized."
        } else {
          "Not applicable (a single group measured repeatedly)."
        },
        n_total = res$n_total,
        n_per_group_txt = sprintf("%d participants x %d measurements = %d observations",
                                   res$n_total, res$m, res$n_observations),
        effect_branch = input$es_branch %||% "sesoi",
        effect_branch_details = effect_branch_details(),
        method_key = "rm_anova"
      )
    })

    wire_results_server(input, output, session, family = "rm_anova",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn, effect_set_r = effect_set_r)
  })
}
