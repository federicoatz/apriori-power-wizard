## mod_clustered.R
## -----------------------------------------------------------------------
## Family module: two-arm clustered design. Whole clusters rather than
## individuals are assigned to condition; participants within a cluster
## tend to be more similar to each other than to participants in a
## different cluster (the intraclass correlation, ICC), which reduces the
## effective information per person and requires more total participants
## than an individually randomized trial detecting the same effect size.
##
## TWO FRAMINGS OF THE SAME MATHEMATICS. The `setting` input switches only
## the wording, examples, and ICC guidance -- never the calculation:
##
##   "lab"   -- experimental sessions / matching groups. This is the case
##              that matters most for this app's audience (experimental
##              economics and psychology): when subjects INTERACT (public
##              goods games, markets, auctions, bargaining), their outcomes
##              are not independent, and the accepted independent unit of
##              observation is the matching group (or the session, when
##              subjects are not partitioned into matching groups), not the
##              individual subject. Treating interacting subjects as
##              independent is one of the most common and most-criticized
##              errors in experimental-economics design, and it inflates
##              the true Type I error rate rather than merely wasting
##              participants.
##   "field" -- the classic cluster-randomized / multi-site trial
##              (classrooms, clinics, stores, villages) that the underlying
##              Donner & Klar (2000) design-effect literature addresses.
##
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
          "GROUPS rather than individuals are the unit that gets assigned to",
          "condition -- or, in an interactive experiment, the unit within",
          "which participants influence each other. Everyone inside the same",
          "group gets the same condition."),
        radioButtons(ns("setting"),
          help_tip("What kind of grouping is this?",
            "This changes only the wording, examples, and guidance below -- the calculation is identical either way. Pick whichever describes your study; if participants interact with each other in fixed groups, choose the first option."),
          choices = c("Lab or online experiment: sessions / matching groups" = "lab",
                      "Field trial: schools, clinics, stores, villages" = "field"),
          selected = "lab"),

        # ---- Lab / experimental-session framing (the default) -------------
        conditionalPanel(
          condition = sprintf("input['%s'] == 'lab'", ns("setting")),
          guided_box("In plain language",
            p("If your participants ", strong("interact with each other"),
              " -- a public goods game, a market, an auction, a bargaining",
              "task -- then their outcomes are not independent. What one",
              "member of a matching group does changes what the others see",
              "and earn, so the group as a whole, not each individual",
              "separately, is what carries independent information."),
            p("This is the single most common criticism levelled at",
              "experimental designs of this kind: analysing 24 interacting",
              "subjects as 24 independent observations, when they were",
              "really 6 independent matching groups of 4, overstates your",
              "evidence and inflates your false-positive rate. It is not a",
              "matter of being wasteful -- it makes the reported p-values",
              "wrong. Planning at the matching-group level from the start",
              "is what this step is for.")
          ),
          div(class = "well well-info", icon("lightbulb"),
              strong(" Example: "), "a public goods experiment runs subjects in",
              "fixed matching groups of 4 who play only with each other for",
              "all rounds, and compares average contributions between a",
              "punishment and a no-punishment treatment. Subjects inside a",
              "matching group influence one another, so the matching group",
              "is the independent unit -- enter 4 as the group size below."),
          div(class = "field-hint", icon("circle-info"),
              " If subjects are re-matched across the whole session (\"stranger\" matching without separate matching groups), the SESSION is the independent unit: enter the number of participants per session as the group size.")
        ),

        # ---- Classic field-trial framing ----------------------------------
        conditionalPanel(
          condition = sprintf("input['%s'] == 'field'", ns("setting")),
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
              "within each store -- that's exactly this design.")
        ),

        fluidRow(
          column(6, numericInput(ns("cluster_size"),
                    help_tip("Average group size (participants per cluster)", "How many people, on average, are in each independent group. In a lab experiment this is the matching-group size (often 2-4 in a standard interactive game) or the number of participants per session. In a field trial it is students per classroom, patients per clinic, customers per store. It doesn't need to be exact; the average is what matters."),
                    value = 20, min = 2, step = 1)),
          column(6, numericInput(ns("icc"),
                    help_tip("Intraclass correlation (ICC)", "How similar participants within the SAME group are to each other on the outcome, relative to participants in different groups. 0 = no similarity beyond chance (grouping doesn't matter); close to 1 = everyone in a group behaves almost identically. See the guidance directly below for how to choose a value in your setting -- and when in doubt use a LARGER value, since that is the conservative direction."),
                    value = 0.05, min = 0, max = 0.99, step = 0.01))
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'lab'", ns("setting")),
          div(class = "well well-warning", icon("triangle-exclamation"), strong(" Choosing the ICC: "),
              "there is no safe universal default for interactive experiments, and the small values often quoted for schools and clinics (around 0.01-0.05) are NOT appropriate here. When participants influence each other directly -- contributing, bidding, or bargaining together for many rounds -- within-group correlation is typically substantially higher than in those field settings. Estimate it from pilot data or from a published study using the same paradigm whenever you can, and prefer the more conservative (larger) value if you are unsure: a value that is too high only costs you extra participants, whereas one that is too low leaves the study genuinely underpowered and its p-values overstated."),
          div(class = "field-hint", icon("circle-check"),
              " Note that the total N below is what you recruit; the number of independent matching groups per arm, which is what your statistical test actually has to work with, is reported alongside it in the results.")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'field'", ns("setting")),
          div(class = "well well-warning", icon("triangle-exclamation"), strong(" Choosing the ICC: "),
              "typical values for individual-level outcomes in schools and clinics are small but not negligible, often around 0.01-0.05; some settings run higher (0.1+). If you have no pilot estimate, 0.05 is a common conservative default -- when in doubt, use a LARGER ICC, since it never requires fewer participants.")
        ),

        div(class = "well well-warning", icon("triangle-exclamation"), strong(" Important: "),
            "Because whole groups, not individuals, carry the independent information, this design always needs MORE total participants than an individually randomized study detecting the same underlying effect -- the inflation grows with both the group size and the ICC. There is no way around this; it reflects a real loss of statistical information, not a modeling artifact."),
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

    # Wording only -- the calculation is identical in both settings (see the
    # header comment). Kept as a small lookup rather than scattered inline
    # `if`s so the results summary and the report text can never drift into
    # describing the design two different ways.
    setting <- reactive(input$setting %||% "lab")
    words <- reactive({
      if (identical(setting(), "lab")) {
        list(unit = "matching group", units = "matching groups",
             unit_cap = "Matching groups",
             design_phrase = "a two-arm experimental design in which participants interact within fixed matching groups (so the matching group, not the individual participant, is the independent unit of observation)")
      } else {
        list(unit = "cluster", units = "clusters",
             unit_cap = "Clusters",
             design_phrase = "a two-arm cluster-randomized design")
      }
    })

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

    solve_n_fn <- function(sig_level, power, effect = NULL) {
      p <- params()
      power_clustered_n(d = effect %||% effect_value(), icc = icc(), cluster_size = cluster_size(),
                         sig_level = sig_level, power = power, alternative = p$tails,
                         allocation_ratio = p$allocation_ratio)
    }
    effect_set_r <- reactive(effect_comparison_values(effect_value(), "magnitude"))

    n_summary_r <- reactive({
      res <- result_r(); w <- words()
      tagList(
        tags$p(icon("users"), sprintf(" Participants: n1 = %d, n2 = %d (total %d)", res$n1, res$n2, res$n_total)),
        tags$p(icon("layer-group"), sprintf(" %s: %d in arm 1, %d in arm 2 (%d total, ~%d participants each)",
                                             w$unit_cap, res$k1, res$k2, res$n_clusters_total, res$cluster_size)),
        tags$p(icon("scale-unbalanced"), sprintf(" Design effect: %.2f (ICC = %.3f)", res$design_effect, res$icc)),
        tags$p(icon("bullseye"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target)),
        if (identical(setting(), "lab")) {
          div(class = "field-hint", icon("circle-info"),
              sprintf(" Your statistical test effectively has %d independent observations per arm, not %d and %d -- report and analyse at the %s level.",
                      res$k1, res$n1, res$n2, w$unit))
        }
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
      res <- result_r(); p <- params(); w <- words()
      list(
        analysis = if (identical(setting(), "lab")) {
          "two-arm comparison with participants clustered in matching groups (continuous outcome)"
        } else {
          "cluster-randomized two-arm trial (continuous outcome)"
        },
        design = sprintf("%s with an average of %d participants per %s and an assumed intraclass correlation (ICC) of %.3f",
                          w$design_phrase, res$cluster_size, w$unit, res$icc),
        effect_label_short = sprintf("an individual-level effect of d = %.3f (before design-effect inflation)", effect_value()),
        alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
        tails = p$tails,
        allocation = if (p$allocation_ratio == 1) "Arms were assumed to have equal numbers of participants." else sprintf("Arms followed an allocation ratio of %.2f (n2/n1) at the participant level.", p$allocation_ratio),
        n_total = res$n_total,
        n_per_group_txt = sprintf("n1 = %d (%d %s), n2 = %d (%d %s); design effect = %.2f",
                                   res$n1, res$k1, w$units, res$n2, res$k2, w$units, res$design_effect),
        effect_branch = input$es_branch %||% "sesoi",
        effect_branch_details = effect_branch_details(),
        method_key = "clustered_rct"
      )
    })

    wire_results_server(input, output, session, family = "clustered_rct",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn, effect_set_r = effect_set_r)
  })
}
