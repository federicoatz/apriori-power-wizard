## mod_clustered.R
## -----------------------------------------------------------------------
## Family module: two-arm cluster-randomized / multi-site trial. Whole
## clusters (classrooms, clinics, stores, villages) rather than
## individuals are assigned to condition; participants within a cluster
## tend to be more similar to each other than to participants in a
## different cluster (the intraclass correlation, ICC), which reduces the
## effective information per person and requires more total participants
## than an individually randomized trial detecting the same effect size.
## Internal sub-wizard: Design -> Parameters -> Effect size -> Results.
## -----------------------------------------------------------------------

mod_clustered_ui <- function(id) {
  ns <- NS(id)
  tagList(
    step_progress_ui(1, c("Design", "Parameters", "Effect size", "Results")),
    tabsetPanel(
      id = ns("wiz"), type = "hidden",
      tabPanelBody("design",
        h4(icon("sitemap"), " Step: Design structure"),
        p(class = "step-intro",
          "Two conditions are compared on a continuous outcome, but whole",
          "GROUPS -- classrooms, clinics, stores, villages -- are assigned to",
          "condition, not individuals. Everyone inside the same cluster gets",
          "the same condition."),
        guided_box("In plain language",
          p("People within the same cluster tend to be more alike than",
            "people picked completely at random (co-workers in the same",
            "store, students in the same classroom) -- this is measured",
            "by the ", strong("ICC"), " below. That similarity means each",
            "additional PERSON gives you less new information than in a",
            "fully individually-randomized study, so cluster designs",
            "generally need noticeably more total participants than the",
            "'Two independent means' calculation alone would suggest for",
            "the same effect.")
        ),
        div(class = "well well-info", icon("lightbulb"),
            strong(" Example: "), "a field experiment randomly assigns entire",
            "retail stores (not individual customers) to a new checkout flow",
            "or the old one, then measures average spending per customer",
            "within each store -- that's exactly this design."),
        fluidRow(
          column(6, numericInput(ns("cluster_size"),
                    help_tip("Average cluster size (individuals per cluster)", "How many people, on average, are in each cluster -- e.g., students per classroom, patients per clinic, customers per store. Doesn't need to be exact; the average across clusters is what matters."),
                    value = 20, min = 2, step = 1)),
          column(6, numericInput(ns("icc"),
                    help_tip("Intraclass correlation (ICC)", "How similar people within the SAME cluster are to each other on the outcome, relative to people in different clusters. 0 = no similarity beyond chance (clustering doesn't matter); close to 1 = everyone in a cluster has nearly the same value. Typical values for individual-level outcomes in schools/clinics are small but not negligible, often around 0.01-0.05; some settings run higher (0.1+). If you have no pilot estimate, 0.05 is a common conservative default -- when in doubt, use a LARGER ICC, since it never requires fewer participants."),
                    value = 0.05, min = 0, max = 0.99, step = 0.01))
        ),
        div(class = "well well-warning", icon("triangle-exclamation"), strong(" Important: "),
            "Because clusters, not individuals, are randomized, this design always needs MORE total participants than an individually randomized trial detecting the same underlying effect -- the inflation grows with both the cluster size and the ICC. There is no way around this; it reflects a real loss of statistical information, not a modeling artifact."),
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
            help_tip("Benchmark", "This is the INDIVIDUAL-level Cohen's d -- the standardized mean difference you'd expect if you could randomize individuals directly. The app inflates the required sample size internally to account for clustering. As a rule of thumb, 0.2 SDs apart is a small gap, 0.5 is visible to the naked eye, 0.8 is large."),
            choices = c("Small (d = 0.20)" = "small",
                        "Medium (d = 0.50)" = "medium",
                        "Large (d = 0.80)" = "large"),
            selected = "medium"),
          sesoi_ui = tagList(
            radioButtons(ns("sesoi_mode"), "Specify the SESOI as:",
              choices = c("Raw mean difference + expected SD" = "raw",
                          "Already standardized (individual-level Cohen's d)" = "standardized"),
              selected = "standardized"),
            conditionalPanel(condition = sprintf("input['%s'] == 'raw'", ns("sesoi_mode")),
              fluidRow(
                column(6, numericInput(ns("sesoi_raw_diff"),
                         help_tip("Minimal difference of interest (raw units)", "In the outcome's own units -- e.g., '2 dollars', '150 milliseconds', '0.5 points on the scale'."),
                         value = NA)),
                column(6, numericInput(ns("sesoi_sd"),
                         help_tip("Expected individual-level SD", "How spread out individual scores typically are around the mean, ignoring clustering for the moment -- the ICC (previous step) already accounts for the within-cluster similarity."),
                         value = NA, min = 0.0001))
              )
            ),
            conditionalPanel(condition = sprintf("input['%s'] == 'standardized'", ns("sesoi_mode")),
              numericInput(ns("sesoi_d"), "Smallest effect size of interest (individual-level Cohen's d)", value = 0.3, step = 0.05)
            )
          ),
          safeguard_metric_label = "Published individual-level Cohen's d"
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

mod_clustered_server <- function(id) {
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
    cluster_size <- reactive(max(1, safe_numeric(input$cluster_size, 1, 1e6, 20)))
    icc <- reactive(safe_numeric(input$icc, 0, 0.99, 0.05))

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
        naive <- tryCatch(power_clustered_n(pub, icc(), cluster_size(), p$alpha, p$power, p$tails, p$allocation_ratio)$n_total, error = function(e) NA)
        list(
          published_value = sprintf("d = %.3f", pub), published_n = round(n_pub),
          conf_level = input$sg_conf_level %||% 0.80,
          one_sided = identical(input$sg_one_sided, "one"),
          safeguard_value = sprintf("d = %.3f", effect_value()),
          n_naive = naive,
          n_safeguard = tryCatch(power_clustered_n(effect_value(), icc(), cluster_size(), p$alpha, p$power, p$tails, p$allocation_ratio)$n_total, error = function(e) NA)
        )
      } else {
        list(description = sprintf("d = %.3f", effect_value()))
      }
    })

    result_r <- reactive({
      p <- params()
      req(effect_value())
      power_clustered_n(d = effect_value(), icc = icc(), cluster_size = cluster_size(),
                         sig_level = p$alpha, power = p$power, alternative = p$tails,
                         allocation_ratio = p$allocation_ratio)
    })

    solve_n_fn <- function(sig_level, power) {
      p <- params()
      power_clustered_n(d = effect_value(), icc = icc(), cluster_size = cluster_size(),
                         sig_level = sig_level, power = power, alternative = p$tails,
                         allocation_ratio = p$allocation_ratio)
    }

    n_summary_r <- reactive({
      res <- result_r()
      tagList(
        tags$p(icon("users"), sprintf(" Individuals: n1 = %d, n2 = %d (total %d)", res$n1, res$n2, res$n_total)),
        tags$p(icon("layer-group"), sprintf(" Clusters: %d in arm 1, %d in arm 2 (%d total, ~%d per cluster)", res$k1, res$k2, res$n_clusters_total, res$cluster_size)),
        tags$p(icon("scale-unbalanced"), sprintf(" Design effect: %.2f (ICC = %.3f)", res$design_effect, res$icc)),
        tags$p(icon("bullseye"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target))
      )
    })

    curve_extra_args_r <- reactive({
      p <- params()
      list(d = effect_value(), icc = icc(), cluster_size = cluster_size(), sig_level = p$alpha, alternative = p$tails)
    })

    n_solution_r <- reactive(result_r()$n1)

    sensitivity_fn <- function(n_max) {
      p <- params()
      sensitivity_min_effect("clustered_rct", n_max = n_max, icc = icc(),
                              cluster_size = cluster_size(), sig_level = p$alpha,
                              power = p$power, alternative = p$tails)
    }

    report_spec_r <- reactive({
      res <- result_r(); p <- params()
      list(
        analysis = "cluster-randomized two-arm trial (continuous outcome)",
        design = sprintf("a two-arm cluster-randomized design with an average of %d individuals per cluster and an assumed intraclass correlation (ICC) of %.3f",
                          res$cluster_size, res$icc),
        effect_label_short = sprintf("an individual-level effect of d = %.3f (before design-effect inflation)", effect_value()),
        alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
        tails = p$tails,
        allocation = if (p$allocation_ratio == 1) "Arms were assumed to have equal numbers of individuals." else sprintf("Arms followed an allocation ratio of %.2f (n2/n1) at the individual level.", p$allocation_ratio),
        n_total = res$n_total,
        n_per_group_txt = sprintf("n1 = %d (%d clusters), n2 = %d (%d clusters); design effect = %.2f",
                                   res$n1, res$k1, res$n2, res$k2, res$design_effect),
        effect_branch = input$es_branch %||% "sesoi",
        effect_branch_details = effect_branch_details(),
        method_key = "clustered_rct"
      )
    })

    wire_results_server(input, output, session, family = "clustered_rct",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn)
  })
}
