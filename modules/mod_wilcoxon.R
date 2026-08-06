## mod_wilcoxon.R
## -----------------------------------------------------------------------
## Family module: Wilcoxon-Mann-Whitney rank-sum test for two independent
## groups (see R/power_wilcoxon.R for Noether's 1987 formula and its
## Monte Carlo validation).
##
## This family reuses the standard three-branch effect-size step, but its
## native effect size is the probability of superiority P(X < Y) rather
## than Cohen's d, and the safeguard branch works entirely on that scale:
## the published statistic can be entered as p itself, as the Mann-Whitney
## U (whose p-hat = U/(n1*n2) IS the sample estimate of p, with no
## distributional assumption), or as the reported z (inverted through the
## large-sample U ~ normal approximation) -- and the confidence interval
## behind the safeguard bound is built directly on the p scale via
## safeguard_ci_auc() (Hanley & McNeil 1982), never passing through d.
## A published Cohen's d is still accepted as a last resort, but only the
## POINT conversion d -> p (exact under normality, d_to_p_superiority())
## is normal-theory; the interval is then still built on the p scale.
## Cohen's conventions and the d-input SESOI mode keep the same point
## conversion, flagged by the existing used_d_conversion warning.
## -----------------------------------------------------------------------

mod_wilcoxon_ui <- function(id) {
  ns <- NS(id)
  tagList(
    step_progress_ui(1, c("Design", "Parameters", "Effect size", "Results")),
    tabsetPanel(
      id = ns("wiz"), type = "hidden",
      tabPanelBody("design",
        h3(icon("ranking-star"), " Step: Design structure"),
        p(class = "step-intro",
          "Two independent groups are compared on an outcome that is ordinal,",
          "or continuous but not plausibly normal -- skewed earnings,",
          "bounded contributions, lumpy bids, Likert-type ratings. Instead of",
          "comparing means, the Wilcoxon-Mann-Whitney test compares ranks,",
          "asking whether values from one group tend to be larger than",
          "values from the other."),
        guided_box("In plain language",
          p("A t-test asks \"is the AVERAGE different?\" and leans on the",
            "assumption that the data are roughly normally distributed.",
            "This test instead asks \"if I pick one participant at random",
            "from each group, how often is one of them higher?\" It makes",
            "no assumption about the shape of the distribution, which is",
            "why it is the standard choice in experimental economics for",
            "outcomes like contributions, bids, and earnings that are",
            "typically skewed, bounded, or piled up at zero."),
          p("The trade-off is small: with genuinely normal data this test",
            "needs only about 5% more participants than a t-test. With the",
            "heavy-tailed or skewed data it is designed for, it often needs",
            strong(" fewer"), " -- so the cost of choosing it defensively is",
            "much lower than most people assume.")
        ),
        div(class = "well well-info", icon("lightbulb"),
            strong(" Example: "), "comparing average contributions in a public",
            "goods game between a punishment and a no-punishment treatment,",
            "where contributions cluster at 0 and at the endowment and are",
            "clearly not normal -- a rank test is the conventional choice."),
        div(class = "field-hint", icon("triangle-exclamation"),
            " If your participants interact with each other in fixed groups, the individual is not the independent unit -- use the \"Clustered: sessions / matching groups\" family instead, or apply this test at the matching-group level."),
        wizard_nav_ui(ns, "design", show_back = FALSE)
      ),
      tabPanelBody("params",
        params_step_ui(ns),
        wizard_nav_ui(ns, "params")
      ),
      tabPanelBody("effect_size",
        effect_size_step_ui(
          ns,
          cohen_ui = tagList(
            radioButtons(ns("cohen_size"),
              help_tip("Benchmark", "Cohen's conventions are defined for the mean-difference scale (d); the app converts your choice to the probability of superiority that the rank test actually works with, which is shown below."),
              choices = c("Small (d = 0.20)" = "small",
                          "Medium (d = 0.50)" = "medium",
                          "Large (d = 0.80)" = "large"),
              selected = "medium"),
            div(class = "field-hint", icon("circle-info"),
                " These are converted to P(X < Y) = 0.556, 0.638, and 0.714 respectively.")
          ),
          sesoi_ui = tagList(
            radioButtons(ns("sesoi_mode"), "Specify the SESOI as:",
              choices = c("Probability of superiority, P(X < Y)" = "psup",
                          "Standardized mean difference (Cohen's d), converted" = "d"),
              selected = "psup"),
            conditionalPanel(condition = sprintf("input['%s'] == 'psup'", ns("sesoi_mode")),
              numericInput(ns("sesoi_psup"),
                help_tip("Smallest P(X < Y) of interest",
                  "If you drew one participant at random from each group, how often would the second one have to score higher for the difference to matter to you? 0.5 means no difference at all; 0.56 is a small effect, 0.64 medium, 0.71 large, by analogy with Cohen's conventions."),
                value = 0.64, min = 0.001, max = 0.999, step = 0.01)
            ),
            conditionalPanel(condition = sprintf("input['%s'] == 'd'", ns("sesoi_mode")),
              numericInput(ns("sesoi_d"), "Smallest effect size of interest (Cohen's d)",
                           value = 0.5, min = 0.0001, step = 0.05),
              helpText("Converted to P(X < Y) assuming both groups are normal with equal variance; the conversion is exact under that assumption.")
            )
          ),
          safeguard_pre_ui = tagList(
            radioButtons(ns("sg_metric"),
              help_tip("Which statistic does the paper report?",
                "Papers rarely report P(X < Y) itself, but it is exactly recoverable from what they DO report: the Mann-Whitney U is the count of between-group pairs with X < Y, so U/(n1*n2) IS the sample P(X < Y), and a reported z inverts to the same quantity through the test's own large-sample approximation. Both routes are distribution-free. A published Cohen's d is accepted as a last resort, but converting it to p assumes normality -- the assumption a rank test is usually chosen to avoid."),
              choices = c("Probability of superiority, P(X < Y)" = "p",
                          "Mann-Whitney U statistic" = "u",
                          "The reported z statistic" = "z",
                          "Cohen's d (normal-theory conversion, last resort)" = "d"),
              selected = "u"),
            div(class = "field-hint", icon("circle-info"),
                " The original study's two group sizes are taken as equal halves of the total N entered on the right. The safeguard interval itself is built directly on the P(X < Y) scale (Hanley & McNeil, 1982) -- it never assumes normality, whichever statistic you start from.")
          ),
          safeguard_metric_label = "Published value (in the statistic selected above)",
          safeguard_hint = NULL
        ),
        uiOutput(ns("psup_summary")),
        wizard_nav_ui(ns, "effect_size", next_label = "Compute")
      ),
      tabPanelBody("results",
        results_panel_ui(ns),
        wizard_nav_ui(ns, "results", show_next = FALSE)
      )
    )
  )
}

mod_wilcoxon_server <- function(id) {
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

    # The published statistic, whatever form it was entered in, resolved to
    # the p = P(X < Y) scale. Returns list(p, n1, n2, metric, raw) or NULL
    # while the inputs are incomplete/invalid. The original study's groups
    # are taken as equal halves of its total N, the same balanced-design
    # assumption every other family's safeguard branch already makes.
    sg_published <- reactive({
      n_pub <- safe_numeric(input$sg_published_n, 4, 1e6)
      val <- safe_numeric(input$sg_published_value, -1e9, 1e9)
      if (is.na(n_pub) || is.na(val)) return(NULL)
      n1o <- n_pub / 2
      n2o <- n_pub / 2
      metric <- input$sg_metric %||% "u"
      p_pub <- tryCatch(switch(metric,
        p = val,
        u = u_to_p_superiority(val, n1o, n2o),
        z = z_to_p_superiority(val, n1o, n2o),
        d = d_to_p_superiority(val)
      ), error = function(e) NA_real_)
      p_pub <- safe_numeric(p_pub, 0.001, 0.999)
      if (is.na(p_pub)) return(NULL)
      list(p = p_pub, n1 = n1o, n2 = n2o, metric = metric, raw = val)
    })

    # Always returned on the > 0.5 side: the test is symmetric in
    # |p - 0.5|, and the params step's own one-/two-tailed control is what
    # expresses directionality. (For the safeguard branch the fold happens
    # AFTER the shrink, which is equivalent by the same symmetry.)
    effect_value <- reactive({
      branch <- input$es_branch %||% "sesoi"
      p <- if (branch == "cohen") {
        d_to_p_superiority(unname(cohen_benchmarks("d")[input$cohen_size %||% "medium"]))
      } else if (branch == "safeguard") {
        pub <- sg_published()
        req(pub)
        # The interval is built directly on the p (AUC) scale -- Hanley &
        # McNeil (1982) -- so no normality assumption enters here even when
        # the point estimate arrived via the d conversion.
        sg <- safeguard_ci_auc(pub$p, n1 = pub$n1, n2 = pub$n2,
                                conf_level = input$sg_conf_level %||% 0.80,
                                one_sided = identical(input$sg_one_sided, "one"))
        sg$p_safeguard
      } else {
        if (identical(input$sesoi_mode %||% "psup", "d")) {
          d_to_p_superiority(safe_numeric(input$sesoi_d, 0.0001, 5, 0.5))
        } else {
          safe_numeric(input$sesoi_psup, 0.001, 0.999, 0.64)
        }
      }
      if (p < 0.5) 1 - p else p
    })

    valid_effect <- reactive({
      p <- effect_value()
      !is.null(p) && !is.na(p) && p > 0.5 && p < 1
    })

    # TRUE when p was reached by converting a d rather than stated directly.
    # This matters more than it looks: the power formula itself is close to
    # invariant to the shape of the outcome distribution once p is fixed
    # (see validation/monte_carlo_validation.R, parent-shape block), which
    # is the whole reason this family is parameterized on p. But
    # d_to_p_superiority() is p = pnorm(d/sqrt(2)), which is a NORMAL-theory
    # identity. So a user arriving via a d re-imports at the conversion step
    # exactly the distributional assumption the rest of the route avoids --
    # and does so silently, since the converted p looks like any other p.
    used_d_conversion <- reactive({
      branch <- input$es_branch %||% "sesoi"
      branch == "cohen" ||
        (identical(branch, "safeguard") && identical(input$sg_metric %||% "u", "d")) ||
        (identical(branch, "sesoi") && identical(input$sesoi_mode %||% "psup", "d"))
    })

    output$psup_summary <- renderUI({
      p <- tryCatch(effect_value(), error = function(e) NA_real_)
      if (is.na(p) || p <= 0.5) {
        return(div(class = "well well-warning", icon("triangle-exclamation"),
          " The effect must correspond to a probability of superiority different from 0.5 -- exactly 0.5 means the two groups are indistinguishable and can never be detected at any sample size."))
      }
      tagList(
        div(class = "well well-result", icon("circle-check"),
            sprintf(" Planning for P(X < Y) = %.3f (equivalent to Cohen's d = %.3f under normality).",
                    p, p_superiority_to_d(p))),
        if (isTRUE(used_d_conversion())) {
          div(class = "well well-warning", icon("triangle-exclamation"),
              HTML(" This P(X &lt; Y) was converted from a standardized mean difference using
                    <em>p</em> = &Phi;(<em>d</em>/&radic;2), which assumes the outcome is normally
                    distributed. The power calculation itself does not need that assumption, but
                    this conversion does. If your outcome is skewed, bounded, or piles up at zero
                    -- the usual reason for choosing a rank test -- prefer stating P(X &lt; Y)
                    directly: it is the quantity this test is actually sensitive to."))
        }
      )
    })

    effect_branch_details <- reactive({
      branch <- input$es_branch %||% "sesoi"
      p <- params()
      pv <- effect_value()
      if (branch == "cohen") {
        list(label = input$cohen_size,
             value = sprintf("P(X < Y) = %.3f (from d = %.2f)",
                              pv, unname(cohen_benchmarks("d")[input$cohen_size %||% "medium"])))
      } else if (branch == "safeguard") {
        pub <- sg_published()
        req(pub)
        p_naive <- if (pub$p < 0.5) 1 - pub$p else pub$p
        naive <- tryCatch(power_wilcoxon_n(p_naive, p$alpha, p$power, p$tails, p$allocation_ratio)$n_total,
                           error = function(e) NA)
        list(
          published_value = switch(pub$metric,
            p = sprintf("P(X < Y) = %.3f", pub$p),
            u = sprintf("U = %s, hence P(X < Y) = U/(n1*n2) = %.3f", format(pub$raw, big.mark = ","), pub$p),
            z = sprintf("z = %.2f, hence P(X < Y) = %.3f via the test's large-sample approximation", pub$raw, pub$p),
            d = sprintf("d = %.3f, hence P(X < Y) = %.3f under normality", pub$raw, pub$p)),
          published_n = round(pub$n1 + pub$n2),
          conf_level = input$sg_conf_level %||% 0.80,
          one_sided = identical(input$sg_one_sided, "one"),
          safeguard_value = sprintf("P(X < Y) = %.3f, interval built on the P(X < Y) scale itself (Hanley & McNeil, 1982)", pv),
          n_naive = naive,
          n_safeguard = tryCatch(power_wilcoxon_n(pv, p$alpha, p$power, p$tails, p$allocation_ratio)$n_total,
                                  error = function(e) NA)
        )
      } else {
        list(description = sprintf("P(X < Y) = %.3f (equivalent to d = %.3f under normality)",
                                    pv, p_superiority_to_d(pv)))
      }
    })

    result_r <- reactive({
      p <- params()
      req(valid_effect())
      power_wilcoxon_n(p_sup = effect_value(), sig_level = p$alpha, power = p$power,
                        alternative = p$tails, allocation_ratio = p$allocation_ratio)
    })

    solve_n_fn <- function(sig_level, power, effect = NULL) {
      p <- params()
      power_wilcoxon_n(p_sup = effect %||% effect_value(), sig_level = sig_level,
                        power = power, alternative = p$tails,
                        allocation_ratio = p$allocation_ratio)
    }
    # Scaled on the DISTANCE from 0.5 (the test's own null), not on p
    # itself -- halving p would be nonsense, halving |p - 0.5| is exactly
    # "half as strong an effect".
    effect_set_r <- reactive({
      p <- effect_value()
      if (is.null(p) || is.na(p) || p <= 0.5) return(stats::setNames(numeric(0), character(0)))
      shifted <- effect_comparison_values(p - 0.5, "magnitude")
      vals <- pmin(0.5 + unname(shifted), 0.999)
      stats::setNames(vals, names(shifted))
    })

    n_summary_r <- reactive({
      res <- result_r()
      tagList(
        tags$p(icon("users"), sprintf(" Group 1: n = %d  |  Group 2: n = %d  (total %d)", res$n1, res$n2, res$n_total)),
        tags$p(icon("ranking-star"), sprintf(" P(X < Y) = %.3f (equivalent to d = %.3f under normality)", res$p_sup, res$d_equivalent)),
        tags$p(icon("bullseye"), sprintf(" Achieved power: %.4f (target: %.2f)", res$power_achieved, res$power_target))
      )
    })

    curve_extra_args_r <- reactive({
      p <- params()
      list(p_sup = effect_value(), sig_level = p$alpha, alternative = p$tails,
           allocation_ratio = p$allocation_ratio)
    })
    n_solution_r <- reactive(result_r()$n1)

    sensitivity_fn <- function(n_max) {
      p <- params()
      sensitivity_min_effect("wilcoxon", n_max = round(n_max), sig_level = p$alpha,
                              power = p$power, alternative = p$tails,
                              allocation_ratio = p$allocation_ratio)
    }

    report_spec_r <- reactive({
      res <- result_r(); p <- params()
      list(
        analysis = "Wilcoxon-Mann-Whitney rank-sum test (two independent groups)",
        design = "a between-subjects design with two independent groups, analysed with a rank-based (nonparametric) test rather than a t-test",
        effect_label_short = sprintf("an effect corresponding to P(X < Y) = %.3f (equivalent to Cohen's d = %.3f under normality)",
                                      res$p_sup, res$d_equivalent),
        alpha = p$alpha, power_target = p$power, power_achieved = res$power_achieved,
        tails = p$tails,
        allocation = if (isTRUE(all.equal(p$allocation_ratio, 1))) {
          "Groups were assumed to be equally sized."
        } else {
          sprintf("Groups followed an allocation ratio of %.2f (n2/n1).", p$allocation_ratio)
        },
        n_total = res$n_total,
        n_per_group_txt = sprintf("n1 = %d, n2 = %d", res$n1, res$n2),
        effect_branch = input$es_branch %||% "sesoi",
        effect_branch_details = effect_branch_details(),
        method_key = "wilcoxon"
      )
    })

    wire_results_server(input, output, session, family = "wilcoxon",
                         result_r = result_r, curve_extra_args_r = curve_extra_args_r,
                         n_solution_r = n_solution_r, sensitivity_fn = sensitivity_fn,
                         report_spec_r = report_spec_r, n_summary_r = n_summary_r,
                         solve_n_fn = solve_n_fn, effect_set_r = effect_set_r)
  })
}
