## mod_clustered_cat.R
## -----------------------------------------------------------------------
## Family module: clustered design with a BINARY or CATEGORICAL outcome
## (see R/power_clustered_cat.R for the design-effect method and its
## scope note). Sibling of mod_clustered.R, which covers the continuous
## case and shares the same lab/field framing switch.
##
## One module rather than two families because the design inputs (cluster
## size, ICC, lab-vs-field framing) are identical and only the outcome's
## effect-size metric differs -- h for a binary outcome via the
## two-proportions machinery, w for a categorical one via chi-square. The
## outcome type therefore selects between two thin solvers, both of which
## wrap an already-validated unclustered family.
## -----------------------------------------------------------------------

mod_clustered_cat_ui <- function(id) {
  ns <- NS(id)
  is_binary <- sprintf("input['%s'] == 'binary'", ns("outcome_type"))
  is_cat <- sprintf("input['%s'] == 'categorical'", ns("outcome_type"))

  tagList(
    step_progress_ui(1, c("Design", "Parameters", "Effect size", "Results")),
    tabsetPanel(
      id = ns("wiz"), type = "hidden",
      tabPanelBody("design",
        h3(icon("sitemap"), " Step: Design structure"),
        p(class = "step-intro",
          "Participants are grouped -- they interact in fixed matching groups",
          "or sessions, or whole groups are assigned to condition together --",
          "and the outcome is a yes/no or a choice among several categories."),
        guided_box("In plain language",
          p("This is the same clustering problem as the continuous case, on",
            "a different kind of outcome. If your participants influenced",
            "each other, then the fact that 60% of them cooperated is not",
            "60 independent observations out of 100: it is a handful of",
            "independent GROUPS that happened to settle at a high rate.",
            "Treating the individual choices as independent overstates your",
            "evidence in exactly the same way it would for a mean."),
          p("Rates are where this bites hardest in practice, because a",
            "cooperation or contribution rate is so often the headline",
            "number in the abstract -- and it is the number most likely to",
            "have been computed by pooling every participant together.")
        ),
        radioButtons(ns("outcome_type"),
          help_tip("What kind of outcome?",
            "Binary = one of two possible outcomes per participant (cooperated or not, contributed or not, accepted or rejected). Categorical = a choice among three or more options, or counts spread across categories."),
          choices = c("Binary: a yes/no outcome per participant" = "binary",
                      "Categorical: a choice among 3+ options" = "categorical"),
          selected = "binary"),
        radioButtons(ns("setting"),
          help_tip("What kind of grouping is this?",
            "This changes only the wording and guidance below -- the calculation is identical either way."),
          choices = c("Lab or online experiment: sessions / matching groups" = "lab",
                      "Field trial: schools, clinics, stores, villages" = "field"),
          selected = "lab"),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'lab'", ns("setting")),
          div(class = "well well-info", icon("lightbulb"),
              strong(" Example: "), "a public goods experiment compares the",
              "share of participants who contribute above half their",
              "endowment between a punishment and a no-punishment treatment.",
              "Participants played in fixed matching groups of 4, so the",
              "matching group -- not the individual -- is the independent",
              "unit: enter 4 as the group size below.")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'field'", ns("setting")),
          div(class = "well well-info", icon("lightbulb"),
              strong(" Example: "), "entire clinics are assigned to a new",
              "reminder protocol or the old one, and the outcome is whether",
              "each patient attended their appointment.")
        ),

        conditionalPanel(
          condition = is_cat,
          fluidRow(
            column(6, numericInput(ns("n_categories"),
                      help_tip("Number of outcome categories", "How many options the outcome can take, e.g., 3 for \"cooperate / defect / abstain\"."),
                      value = 3, min = 2, max = 100, step = 1)),
            column(6, numericInput(ns("n_arms"),
                      help_tip("Number of treatment arms", "How many conditions the categories are compared across. With 2 arms and 3 categories the test has (3-1)x(2-1) = 2 degrees of freedom."),
                      value = 2, min = 2, max = 100, step = 1))
          ),
          uiOutput(ns("df_note"))
        ),

        fluidRow(
          column(6, numericInput(ns("cluster_size"),
                    help_tip("Average group size (participants per cluster)", "In a lab experiment this is the matching-group size (often 2-4 in an interactive game) or the participants per session; in a field trial, patients per clinic or students per classroom."),
                    value = 20, min = 2, step = 1)),
          column(6, numericInput(ns("icc"),
                    help_tip("Intraclass correlation (ICC)", "How similar participants within the SAME group are on the outcome. See the guidance below; when in doubt use a LARGER value, which is the conservative direction."),
                    value = 0.05, min = 0, max = 0.99, step = 0.01))
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'lab'", ns("setting")),
          div(class = "well well-warning", icon("triangle-exclamation"), strong(" Choosing the ICC: "),
              "there is no safe universal default for interactive experiments, and the small values often quoted for schools and clinics (around 0.01-0.05) are NOT appropriate here. When participants influence each other directly, within-group correlation is typically substantially higher. Estimate it from pilot data or a published study using the same paradigm, and prefer the larger value if unsure.")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'field'", ns("setting")),
          div(class = "well well-warning", icon("triangle-exclamation"), strong(" Choosing the ICC: "),
              "typical values for individual-level outcomes in schools and clinics are small but not negligible, often around 0.01-0.05; some settings run higher (0.1+). If you have no pilot estimate, 0.05 is a common conservative default.")
        ),
        conditionalPanel(
          condition = is_cat,
          div(class = "field-hint", icon("circle-info"),
              " For a categorical outcome the design effect is applied to the chi-square sample size. That is the standard first-order adjustment for planning -- the same quantity the Rao-Scott correction estimates from data -- but it is an approximation, and the analysis of the resulting data should itself account for the clustering.")
        ),
        wizard_nav_ui(ns, "design", show_back = FALSE)
      ),

      tabPanelBody("params",
        params_step_ui(ns),
        conditionalPanel(
          condition = is_cat,
          div(class = "field-hint", "Test direction and allocation ratio are not used for a categorical outcome; the chi-square test is non-directional.")
        ),
        wizard_nav_ui(ns, "params")
      ),

      tabPanelBody("effect_size",
        effect_size_step_ui(
          ns,
          cohen_ui = tagList(
            conditionalPanel(condition = is_binary,
              radioButtons(ns("cohen_size_h"),
                help_tip("Benchmark", "Cohen's h is the arcsine-based effect size for two proportions. The raw-proportions option under SESOI is usually easier to reason about."),
                choices = c("Small (h = 0.20)" = "small",
                            "Medium (h = 0.50)" = "medium",
                            "Large (h = 0.80)" = "large"),
                selected = "medium")),
            conditionalPanel(condition = is_cat,
              radioButtons(ns("cohen_size_w"),
                help_tip("Benchmark", "Cohen's w measures the overall discrepancy between the observed and expected distribution across all cells."),
                choices = c("Small (w = 0.10)" = "small",
                            "Medium (w = 0.30)" = "medium",
                            "Large (w = 0.50)" = "large"),
                selected = "medium")),
            div(class = "field-hint", icon("circle-info"),
                " This is the INDIVIDUAL-level effect size -- the one that would apply without clustering. The app inflates the sample size internally; do not adjust it yourself.")
          ),
          sesoi_ui = tagList(
            conditionalPanel(condition = is_binary,
              radioButtons(ns("sesoi_mode"), "Specify the SESOI as:",
                choices = c("Two raw proportions" = "raw",
                            "Already standardized (Cohen's h)" = "standardized"),
                selected = "raw"),
              conditionalPanel(condition = sprintf("input['%s'] == 'raw'", ns("sesoi_mode")),
                fluidRow(
                  column(6, numericInput(ns("sesoi_p1"),
                           help_tip("Rate in group 1", "Your best estimate of the baseline rate, e.g., the cooperation rate you expect without the treatment."),
                           value = 0.30, min = 0.001, max = 0.999, step = 0.01)),
                  column(6, numericInput(ns("sesoi_p2"),
                           help_tip("Minimal rate of interest in group 2", "The smallest rate in the treated group that would still matter to you."),
                           value = 0.45, min = 0.001, max = 0.999, step = 0.01))
                )
              ),
              conditionalPanel(condition = sprintf("input['%s'] == 'standardized'", ns("sesoi_mode")),
                numericInput(ns("sesoi_h"), "Smallest effect size of interest (Cohen's h)",
                             value = 0.3, min = 0.0001, step = 0.05)
              )
            ),
            conditionalPanel(condition = is_cat,
              numericInput(ns("sesoi_w"), "Smallest effect size of interest (Cohen's w)",
                           value = 0.3, min = 0.0001, step = 0.01)
            )
          ),
          safeguard_metric_label = "Published individual-level effect size (h for binary, w for categorical)"
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

mod_clustered_cat_server <- function(id) {
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
    is_binary <- reactive(identical(input$outcome_type %||% "binary", "binary"))
    cluster_size <- reactive(max(1, safe_numeric(input$cluster_size, 1, 1e6, 20)))
    icc <- reactive(safe_numeric(input$icc, 0, 0.99, 0.05))
    df_val <- reactive({
      k <- max(2L, as.integer(safe_numeric(input$n_categories, 2, 1000, 3)))
      a <- max(2L, as.integer(safe_numeric(input$n_arms, 2, 1000, 2)))
      (k - 1L) * (a - 1L)
    })

    # Wording only; the calculation is identical in both settings.
    setting <- reactive(input$setting %||% "lab")
    words <- reactive({
      if (identical(setting(), "lab")) {
        list(unit = "matching group", units = "matching groups", unit_cap = "Matching groups",
             design_phrase = "a design in which participants interact within fixed matching groups (so the matching group, not the individual participant, is the independent unit of observation)")
      } else {
        list(unit = "cluster", units = "clusters", unit_cap = "Clusters",
             design_phrase = "a cluster-randomized design")
      }
    })

    output$df_note <- renderUI({
      div(class = "field-hint", icon("circle-check"),
          sprintf(" Degrees of freedom for this design: df = %d.", df_val()))
    })

    effect_value <- reactive({
      branch <- input$es_branch %||% "sesoi"
      if (branch == "cohen") {
        if (is_binary()) {
          unname(cohen_benchmarks("h")[input$cohen_size_h %||% "medium"])
        } else {
          unname(cohen_benchmarks("w")[input$cohen_size_w %||% "medium"])
        }
      } else if (branch == "safeguard") {
        pub <- safe_numeric(input$sg_published_value, 0.0001, 5)
        n_pub <- safe_numeric(input$sg_published_n, 4, 1e6)
        req(pub, n_pub)
        if (is_binary()) {
          sg <- safeguard_ci_h(pub, n1 = n_pub / 2, n2 = n_pub / 2,
                                conf_level = input$sg_conf_level %||% 0.80,
                                one_sided = identical(input$sg_one_sided, "one"))
          sg$h_safeguard
        } else {
          sg <- safeguard_ci_w(pub, n_published = n_pub, df = df_val(),
                                conf_level = input$sg_conf_level %||% 0.80,
                                one_sided = identical(input$sg_one_sided, "one"))
          sg$w_safeguard
        }
      } else {
        if (is_binary()) {
          if (identical(input$sesoi_mode %||% "raw", "raw")) {
            p1 <- safe_numeric(input$sesoi_p1, 0.001, 0.999, 0.30)
            p2 <- safe_numeric(input$sesoi_p2, 0.001, 0.999, 0.45)
            abs(sesoi_proportions_to_h(p1, p2))
          } else {
            abs(safe_numeric(input$sesoi_h, 0.0001, 5, 0.3))
          }
        } else {
          safe_numeric(input$sesoi_w, 0.0001, 5, 0.3)
        }
      }
    })

    solve_for <- function(eff, sig_level, power) {
      if (is_binary()) {
        power_clustered_binary_n(h = eff, icc = icc(), cluster_size = cluster_size(),
                                  sig_level = sig_level, power = power,
                                  alternative = params()$tails,
                                  allocation_ratio = params()$allocation_ratio)
      } else {
        power_clustered_cat_n(w = eff, df = df_val(), icc = icc(),
                               cluster_size = cluster_size(),
                               sig_level = sig_level, power = power)
      }
    }

    effect_branch_details <- reactive({
      branch <- input$es_branch %||% "sesoi"
      p <- params()
      metric <- if (is_binary()) "h" else "w"
      if (branch == "cohen") {
        list(label = if (is_binary()) input$cohen_size_h else input$cohen_size_w,
             value = sprintf("%s = %.3f", metric, effect_value()))
      } else if (branch == "safeguard") {
        pub <- safe_numeric(input$sg_published_value, 0.0001, 5)
        n_pub <- safe_numeric(input$sg_published_n, 4, 1e6)
        list(
          published_value = sprintf("%s = %.3f", metric, pub),
          published_n = round(n_pub),
          conf_level = input$sg_conf_level %||% 0.80,
          one_sided = identical(input$sg_one_sided, "one"),
          safeguard_value = sprintf("%s = %.3f", metric, effect_value()),
          n_naive = tryCatch(solve_for(pub, p$alpha, p$power)$n_total, error = function(e) NA),
          n_safeguard = tryCatch(solve_for(effect_value(), p$alpha, p$power)$n_total, error = function(e) NA)
        )
      } else {
        list(description = sprintf("%s = %.3f", metric, effect_value()))
      }
    })

    result_r <- reactive({
      p <- params()
      req(effect_value())
      solve_for(effect_value(), p$alpha, p$power)
    })

    solve_n_fn <- function(sig_level, power, effect = NULL) {
      solve_for(effect %||% effect_value(), sig_level, power)
    }
    effect_set_r <- reactive(effect_comparison_values(effect_value(), "magnitude"))

    n_summary_r <- reactive({
      res <- result_r(); w <- words()
      tagList(
        if (is_binary()) {
          tags$p(icon("users"), sprintf(" Participants: n1 = %d, n2 = %d (total %d)", res$n1, res$n2, res$n_total))
        } else {
          tags$p(icon("users"), sprintf(" Participants: %d in total", res$n_total))
        },
        tags$p(icon("layer-group"),
               if (is_binary()) {
                 sprintf(" %s: %d in arm 1, %d in arm 2 (%d total, ~%d participants each)",
                         w$unit_cap, res$k1, res$k2, res$n_clusters_total, res$cluster_size)
               } else {
                 sprintf(" %s: %d in total (~%d participants each)",
                         w$unit_cap, res$n_clusters_total, res$cluster_size)
               }),
        tags$p(icon("scale-unbalanced"), sprintf(" Design effect: %.2f (ICC = %.3f)", res$design_effect, res$icc)),
        tags$p(icon("bullseye"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target)),
        if (identical(setting(), "lab")) {
          div(class = "field-hint", icon("circle-info"),
              sprintf(" Your statistical test effectively has %d independent observations, not %d -- report and analyse at the %s level.",
                      res$n_clusters_total, res$n_total, w$unit))
        }
      )
    })

    curve_extra_args_r <- reactive({
      p <- params()
      if (is_binary()) {
        list(h = effect_value(), icc = icc(), cluster_size = cluster_size(),
             sig_level = p$alpha, alternative = p$tails)
      } else {
        list(w = effect_value(), df = df_val(), icc = icc(),
             cluster_size = cluster_size(), sig_level = p$alpha)
      }
    })
    n_solution_r <- reactive(if (is_binary()) result_r()$n1 else result_r()$n_total)

    sensitivity_fn <- function(n_max) {
      p <- params()
      if (is_binary()) {
        sensitivity_min_effect("clustered_binary", n_max = round(n_max), icc = icc(),
                                cluster_size = cluster_size(), sig_level = p$alpha,
                                power = p$power, alternative = p$tails)
      } else {
        sensitivity_min_effect("clustered_cat", n_max = round(n_max), df = df_val(),
                                icc = icc(), cluster_size = cluster_size(),
                                sig_level = p$alpha, power = p$power)
      }
    }

    report_spec_r <- reactive({
      res <- result_r(); p <- params(); w <- words()
      if (is_binary()) {
        list(
          analysis = "clustered two-arm comparison of proportions (binary outcome)",
          design = sprintf("%s with a binary outcome, an average of %d participants per %s, and an assumed intraclass correlation (ICC) of %.3f",
                            w$design_phrase, res$cluster_size, w$unit, res$icc),
          effect_label_short = sprintf("an individual-level effect of h = %.3f (before design-effect inflation)", effect_value()),
          alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
          tails = p$tails,
          allocation = if (isTRUE(all.equal(p$allocation_ratio, 1))) "Arms were assumed to have equal numbers of participants." else sprintf("Arms followed an allocation ratio of %.2f (n2/n1).", p$allocation_ratio),
          n_total = res$n_total,
          n_per_group_txt = sprintf("n1 = %d (%d %s), n2 = %d (%d %s); design effect = %.2f",
                                     res$n1, res$k1, w$units, res$n2, res$k2, w$units, res$design_effect),
          effect_branch = input$es_branch %||% "sesoi",
          effect_branch_details = effect_branch_details(),
          method_key = "clustered_binary"
        )
      } else {
        list(
          analysis = "clustered chi-square test (categorical outcome)",
          design = sprintf("%s with a categorical outcome (df = %d), an average of %d participants per %s, and an assumed intraclass correlation (ICC) of %.3f",
                            w$design_phrase, res$df, res$cluster_size, w$unit, res$icc),
          effect_label_short = sprintf("an individual-level effect of w = %.3f (before design-effect inflation)", effect_value()),
          alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
          tails = "none",
          allocation = "Not applicable (a single chi-square test across all categories).",
          n_total = res$n_total,
          n_per_group_txt = sprintf("%d %s of ~%d participants; design effect = %.2f",
                                     res$n_clusters_total, w$units, res$cluster_size, res$design_effect),
          effect_branch = input$es_branch %||% "sesoi",
          effect_branch_details = effect_branch_details(),
          method_key = "clustered_cat"
        )
      }
    })

    wire_results_server(input, output, session, family = "clustered_cat",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn, effect_set_r = effect_set_r)
  })
}
