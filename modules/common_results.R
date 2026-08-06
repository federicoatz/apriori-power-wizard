## common_results.R
## -----------------------------------------------------------------------
## Shared "results step" UI + server wiring: sample-size summary, power
## curve, inverse sensitivity analysis, paste-ready report text, and
## HTML/PDF export. Every family module calls `results_panel_ui()` for its
## final sub-step and `wire_results_server()` from inside its own
## moduleServer to populate it. Kept generic over `family` so the family
## modules only need to supply small reactive expressions.
## -----------------------------------------------------------------------

#' UI for the results step (shared layout across all families)
#' @export
results_panel_ui <- function(ns) {
  tagList(
    h3(icon("chart-simple"), " Step: Results"),
    p(class = "step-intro",
      "Here is your recommended sample size, together with everything needed",
      "to check and defend it: how power changes with N, what you could still",
      "detect on a tighter budget, and paste-ready text for a paper or",
      "pre-registration."),
    guided_box("In plain language",
      p(strong("Total N"), " is the number of participants (or pairs, or",
        "cells, depending on the design) this study needs, always rounded",
        "UP to a whole number -- you can't recruit a fraction of a",
        "person, and rounding down would leave you slightly under-powered.",
        strong(" Achieved power"), " is the power you actually get at that",
        "rounded N, which is always at least as high as the target you",
        "asked for."),
      p("The ", strong("power curve"), " below shows what happens at OTHER",
        "sample sizes too -- useful if your recommended N turns out to be",
        "unaffordable and you want to see what you'd give up at a smaller",
        "one. The ", strong("sensitivity analysis"), " answers that same",
        "question the other way around: 'if I can only recruit this many",
        "people, what's the smallest effect I could still reliably",
        "detect?'")
    ),

    uiOutput(ns("safeguard_uninformative_note")),
    uiOutput(ns("value_boxes")),
    uiOutput(ns("multiplicity_note")),
    div(class = "well well-result", uiOutput(ns("n_summary"))),

    accordion(
      id = ns("cmp_acc"), open = FALSE,
      accordion_panel(
        title = tagList(icon("layer-group"), " Compare across alpha / power / effect size (optional)"),
        value = "compare",
        p(class = "field-hint",
          "Add extra values to see them plotted together with your current settings below.",
          "Your current alpha, power, and effect size (from the previous steps) are always included.",
          "Up to 3 alpha levels can be compared at once."),
        fluidRow(
          column(4, checkboxGroupInput(ns("cmp_alpha"), "Additional alpha (significance) levels:",
            choices = c("0.10" = "0.10", "0.05" = "0.05", "0.01" = "0.01", "0.001" = "0.001"),
            selected = character(0), inline = TRUE)),
          column(4, checkboxGroupInput(ns("cmp_power"), "Additional power targets:",
            choices = c("0.80" = "0.80", "0.90" = "0.90", "0.99" = "0.99"),
            selected = character(0), inline = TRUE)),
          column(4, uiOutput(ns("cmp_effect_ui")))
        )
      )
    ),

    h4(icon("chart-line"), " Power curve"),
    p(class = "field-hint",
      if (HAS_PLOTLY) {
        "Hover anywhere on the curve to read the power you'd get at that sample size; drag to zoom, double-click to reset. Dashed lines are power targets; marked points are the sample sizes each scenario needs."
      } else {
        "Dashed lines are power targets; marked points are the sample sizes each scenario needs."
      }),
    # Interactive (plotly) when available, static ggplot2 otherwise -- see
    # HAS_PLOTLY in global.R for why the fallback exists.
    if (HAS_PLOTLY) {
      plotly::plotlyOutput(ns("power_curve_plotly"), height = "340px")
    } else {
      tagList(
        plotOutput(ns("power_curve_plot"), height = "340px"),
        div(style = "text-align: right; margin-top: 4px;",
            actionButton(ns("dl_plot_svg"), tagList(icon("download"), " Download plot as SVG"),
                         class = "btn-outline-secondary btn-sm"))
      )
    },

    # The two interactive tools on this step (sensitivity and budget) used
    # to be plain hr()-separated headings, which made them read as footnotes
    # to the report text rather than as things you are meant to USE. They
    # are now cards with an accent spine, matching the .step-card language
    # of the wizard steps themselves.
    div(class = "results-tool",
      div(class = "results-tool-title", icon("magnifying-glass-dollar"),
          "What if you can't reach that sample size?"),
      p(class = "results-tool-intro",
        "Enter the largest sample you could realistically collect, and this",
        "tells you the smallest effect that design could still detect --",
        "the honest way to report a study you can afford rather than the",
        "one you'd like."),
      fluidRow(
        column(5, numericInput(ns("budget_n"), "Maximum feasible N (total, or per cell for factorial ANOVA)",
                                value = NA, min = 2)),
        column(7, div(class = "results-tool-output", uiOutput(ns("sensitivity_result"))))
      )
    ),

    uiOutput(ns("attrition_note")),

    div(class = "results-tool",
      div(class = "results-tool-title", icon("coins"),
          "What will it cost?"),
      p(class = "results-tool-intro",
        "Experiments pay their participants, so a sample size is also a line",
        "in a grant application. Enter what one participant costs and this",
        "turns the N above into a total -- and, if you give it a fixed",
        "budget, tells you how many participants that buys and the smallest",
        "effect they could detect."),
      fluidRow(
        column(3, selectInput(ns("currency"),
                  help_tip("Currency", "Used only to label the amounts below; it has no effect on any calculation."),
                  choices = c("Euro (\u20ac)" = "EUR", "Pound (\u00a3)" = "GBP", "US dollar ($)" = "USD"),
                  selected = "EUR")),
        column(3, numericInput(ns("cost_per_participant"),
                  help_tip("Cost per participant", "Show-up fee plus the average payment you expect a participant to earn."),
                  value = NA, min = 0, step = 1)),
        column(3, numericInput(ns("cost_fixed"),
                  help_tip("Fixed costs (optional)", "Costs that do not scale with the number of participants -- lab or platform fees, research assistant time, software."),
                  value = 0, min = 0, step = 10)),
        column(3, numericInput(ns("budget_cap"),
                  help_tip("Budget available (optional)", "If you have a fixed budget, enter it to see how many participants it covers and the smallest effect that sample could detect."),
                  value = NA, min = 0, step = 100))
      ),
      div(class = "results-tool-output", uiOutput(ns("budget_result")))
    ),

    hr(),
    div(class = "report-text-header",
        h4(icon("file-lines"), " Paste-ready report / pre-registration text"),
        tags$button(type = "button", class = "btn btn-outline-secondary btn-sm copy-report-btn",
                    `data-target` = ns("report_text"),
                    icon("copy"), tags$span(class = "copy-report-label", " Copy to clipboard"))
    ),
    verbatimTextOutput(ns("report_text")),

    fluidRow(
      column(4, actionButton(ns("dl_html"), tagList(icon("file-code"), " Download HTML report"), class = "btn-outline-primary")),
      column(4,
        if (!IS_SHINYLIVE) {
          downloadButton(ns("dl_pdf"), tagList(icon("file-pdf"), " Download PDF report"), class = "btn-outline-primary")
        } else {
          # PDF export needs a pandoc/LaTeX toolchain that does not exist
          # inside the browser-only (webR/shinylive) build -- see
          # IS_SHINYLIVE in global.R and README.md, "Browser-only
          # deployment". The HTML export above works identically in both
          # builds, so it's offered here as the alternative.
          span(class = "field-hint", icon("circle-info"),
               " PDF export isn't available in this browser-only version -- use the HTML report above, or the full server-hosted version.")
        }
      )
    )
  )
}

#' Wire the results step's server logic
#'
#' @param input,output,session standard moduleServer args of the CALLING
#'   family module (results are attached to that module's own output/ns)
#' @param family character, analysis family key (see [generate_power_curve()])
#' @param result_r reactive() returning the list produced by the family's
#'   `power_*_n()` solver (must include n_total or n_per_cell, and
#'   power_achieved)
#' @param curve_extra_args_r reactive() returning a named list of extra
#'   arguments (besides n) to pass to `generate_power_curve()`
#' @param n_solution_r reactive() returning the numeric N used as the
#'   curve's central/solution point (n_total, or n_per_cell for ANOVA)
#' @param sensitivity_fn function(n_max) -> list(metric, value); wraps
#'   [sensitivity_min_effect()] with the family's fixed arguments baked in
#' @param report_spec_r reactive() returning the `spec` list consumed by
#'   [build_report_text()]
#' @param n_summary_r reactive() returning a `tagList`/HTML describing N
#'   total and per-group/cell breakdown
#' @param solve_n_fn optional function(sig_level, power, effect = NULL) -> the
#'   same list shape as `result_r()`, with every other input (tails,
#'   design-specific args) held at the family's current values, and
#'   `effect = NULL` meaning "use the family's current effect size" (its
#'   existing default behavior). Powers the "Compare across alpha / power /
#'   effect size" chart controls; if omitted, the chart silently falls back
#'   to showing only the current single scenario.
#' @param effect_set_r optional reactive() returning a named numeric vector
#'   of EXTRA effect-size values to offer for comparison (see
#'   [effect_comparison_values()]) -- never including the current value,
#'   the same convention `alpha_set()`/`power_set()` use internally. Omit
#'   for families whose "effect" isn't a single freely-rescalable scalar
#'   (McNemar's two discordant-pair probabilities, logistic's p0/p1 pair),
#'   which silently disables the effect-comparison UI for that family,
#'   exactly like omitting `solve_n_fn` disables alpha/power comparison.
#' @export
wire_results_server <- function(input, output, session, family,
                                 result_r, curve_extra_args_r, n_solution_r,
                                 sensitivity_fn, report_spec_r, n_summary_r,
                                 solve_n_fn = NULL, effect_set_r = NULL) {

  # The x-axis quantity a curve point represents, per family: n1 (per-arm/
  # per-pair N) for every family except ANOVA (n_per_cell) and the two
  # single-sample-style families (n_total). Mirrors n_solution_r()'s own
  # per-family definition exactly, so a comparison scenario's marker lands
  # on the same x-axis meaning as the primary solution.
  extract_curve_n <- function(res) {
    if (family == "anova_factorial") res$n_per_cell
    else if (family == "ancova") res$n_per_group
    else if (family %in% c("regression", "logistic", "chisq", "correlation", "survival")) res$n_total
    else if (family %in% c("mcnemar", "tost")) res$n
    else if (family %in% c("wilcoxon")) res$n1
    else if (family %in% c("rm_anova", "clustered_cat")) res$n_total
    else res$n1
  }

  # Small, colorblind-safe categorical palette (first 3 slots of the
  # app's validated qualitative set) for up to 3 simultaneous alpha
  # curves. Power targets don't need their own colors -- they're drawn as
  # labeled horizontal reference lines shared across every curve.
  cmp_alpha_palette <- function(mode) {
    if (identical(mode, "dark")) c("#3987e5", "#d95926", "#199e70")
    else c("#2a78d6", "#eb6834", "#1baf7a")
  }

  current_alpha <- reactive({ curve_extra_args_r()$sig_level %||% 0.05 })
  current_power <- reactive({ result_r()$power_target %||% 0.80 })

  # Extra alphas beyond the current one, capped so the total never exceeds
  # 3 (the palette's validated all-pairs limit) -- sorted ascending for a
  # fixed, stable color assignment regardless of click order.
  alpha_set <- reactive({
    extra <- setdiff(suppressWarnings(as.numeric(input$cmp_alpha)), current_alpha())
    extra <- head(sort(unique(extra[!is.na(extra)])), 2)
    sort(unique(c(current_alpha(), extra)))
  })

  power_set <- reactive({
    extra <- suppressWarnings(as.numeric(input$cmp_power))
    sort(unique(c(current_power(), extra[!is.na(extra)])), decreasing = TRUE)
  })

  # Extra effect-size values beyond the current one, or just NA_real_ (a
  # sentinel meaning "use the current effect, don't override it") if this
  # family didn't supply effect_set_r. Values (not the current one) are
  # rendered as a checkboxGroupInput built dynamically from
  # effect_set_r(), since unlike alpha/power the sensible choices are
  # family- and even state-dependent (see effect_comparison_values()).
  output$cmp_effect_ui <- renderUI({
    if (is.null(effect_set_r)) return(NULL)
    extra <- effect_set_r()
    if (is.null(extra) || length(extra) == 0) return(NULL)
    formatted <- vapply(unname(extra), format_stat, character(1), digits = 3)
    choices <- stats::setNames(as.character(unname(extra)),
                                sprintf("%s (%s)", names(extra), formatted))
    checkboxGroupInput(session$ns("cmp_effect"), "Additional effect sizes:",
                        choices = choices, selected = character(0), inline = TRUE)
  })

  effect_set <- reactive({
    if (is.null(effect_set_r)) return(NA_real_)
    extra_all <- effect_set_r()
    if (is.null(extra_all) || length(extra_all) == 0) return(NA_real_)
    chosen <- suppressWarnings(as.numeric(input$cmp_effect))
    chosen <- chosen[!is.na(chosen)]
    c(NA_real_, chosen)
  })

  # One row per (alpha, power, effect) combination actually being compared,
  # each solved independently via the family's own solve_n_fn (everything
  # else held at the family's current values). `effect = NA` is the
  # sentinel for "don't override -- use the family's current effect size".
  scenario_grid <- reactive({
    # result_r() has already solved the CURRENT alpha/power/effect
    # combination -- reuse it (Shiny caches the reactive, so this is free)
    # rather than asking solve_n_fn() to redo the exact same solve a second
    # time. This matters even when no extra scenario is being compared:
    # without this, every single results render would silently pay for the
    # (potentially expensive -- e.g. ANOVA's or an unbalanced design's
    # search loop) solver twice for no benefit.
    cur_res <- result_r()
    if (is.null(solve_n_fn)) {
      return(data.frame(alpha = current_alpha(), power = current_power(), effect = NA_real_,
                         n = extract_curve_n(cur_res), power_achieved = cur_res$power_achieved,
                         is_current = TRUE))
    }
    alphas <- alpha_set(); powers <- power_set(); effects <- effect_set()
    grid <- expand.grid(alpha = alphas, power = powers, effect = effects, KEEP.OUT.ATTRS = FALSE)
    rows <- lapply(seq_len(nrow(grid)), function(i) {
      is_cur <- isTRUE(all.equal(grid$alpha[i], current_alpha())) &&
                isTRUE(all.equal(grid$power[i], current_power())) &&
                is.na(grid$effect[i])
      res <- if (is_cur) {
        cur_res
      } else if (is.na(grid$effect[i])) {
        tryCatch(solve_n_fn(grid$alpha[i], grid$power[i]), error = function(e) NULL)
      } else {
        tryCatch(solve_n_fn(grid$alpha[i], grid$power[i], effect = grid$effect[i]), error = function(e) NULL)
      }
      data.frame(
        alpha = grid$alpha[i], power = grid$power[i], effect = grid$effect[i],
        n = if (is.null(res)) NA_real_ else extract_curve_n(res),
        power_achieved = if (is.null(res)) NA_real_ else res$power_achieved,
        is_current = is_cur
      )
    })
    do.call(rbind, rows)
  })

  # Shared x-axis range across every active curve. With a single scenario
  # (the default, nothing extra selected) this reduces to exactly the
  # same lo/hi formula the single-curve chart always used.
  shared_n_range <- reactive({
    ns_vals <- scenario_grid()$n
    ns_vals <- ns_vals[!is.na(ns_vals)]
    if (length(ns_vals) == 0) return(NULL)
    c(max(2, round(min(ns_vals) * 0.3)), round(max(ns_vals) * 1.8) + 5)
  })

  # The headline quantity differs by family (per cell for factorial ANOVA,
  # per group for ANCOVA, total otherwise). Factored out so that anything
  # comparing two solves -- the multiplicity pair below, for instance --
  # is guaranteed to compare the same quantity the value box reports,
  # rather than a total against a per-cell figure.
  n_label <- if (family == "anova_factorial") "N per cell"
             else if (family == "ancova") "N per group"
             else "Total N"
  display_n <- function(res) {
    if (is.null(res)) return(NA_integer_)
    if (family == "anova_factorial") res$n_per_cell
    else if (family == "ancova") res$n_per_group
    else (res$n_total %||% NA_integer_)
  }

  output$value_boxes <- renderUI({
    if (isTRUE(safeguard_diverged())) return(NULL)
    res <- tryCatch(result_r(), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    n_display <- display_n(res) %||% n_solution_r()
    if (is.na(n_display)) n_display <- n_solution_r()

    layout_column_wrap(
      width = 1 / 2, class = "value-box-row",
      value_box(
        title = n_label,
        value = format(n_display, big.mark = ","),
        showcase = icon("users"),
        theme = "primary"
      ),
      value_box(
        title = "Achieved power",
        value = sprintf("%.1f%%", res$power_achieved * 100),
        showcase = icon("bolt"),
        theme = if (res$power_achieved >= res$power_target) "success" else "warning"
      )
    )
  })

  output$n_summary <- renderUI({
    if (isTRUE(safeguard_diverged())) return(NULL)
    n_summary_r()
  })

  # The active UI theme, reported by the browser (see the theme-toggle
  # script at the bottom of app.R).
  #
  # It arrives as a ROOT-level Shiny input (`pw_theme`), so it cannot be
  # read as `input$pw_theme` from in here -- inside a module that would
  # resolve to the namespaced `<id>-pw_theme`, which never exists. The
  # top-level server therefore parks a reactive in `session$userData`,
  # which Shiny shares between the root session and every module session
  # (and keeps isolated per user session). The defensive fallback covers
  # the first render, before the client has reported in.
  theme_mode <- reactive({
    tm <- session$userData$theme
    if (is.function(tm)) tm() else "light"
  })

  # Shared by both the plotly and the ggplot2 renderer: one curve per
  # alpha in alpha_set(), all drawn over the same shared x-axis range so
  # they're directly comparable. `n_solution` is set to a value that can
  # never match the grid (-1) because solution points are now marked
  # separately from scenario_grid(), not via generate_power_curve()'s own
  # is_solution flag.
  curve_data <- reactive({
    rng <- shared_n_range()
    validate(need(!is.null(rng), "Complete the previous steps to see the power curve."))
    base_args <- curve_extra_args_r()
    out <- lapply(alpha_set(), function(a) {
      args_i <- base_args
      args_i$sig_level <- a
      df <- tryCatch(
        do.call(generate_power_curve, c(
          list(family = family, n_solution = -1, n_range = rng), args_i)),
        error = function(e) NULL
      )
      if (!is.null(df)) df$alpha <- a
      df
    })
    do.call(rbind, out)
  })

  # ---- Interactive power curve (plotly) --------------------------------
  # Only registered when plotly is installed; results_panel_ui() renders
  # the matching output binding. See HAS_PLOTLY in global.R.
  if (HAS_PLOTLY) {
    output$power_curve_plotly <- plotly::renderPlotly({
      req(!isTRUE(safeguard_diverged()))
      res <- tryCatch(result_r(), error = function(e) NULL)
      validate(need(!is.null(res), "Complete the previous steps to see the power curve."))

      df <- curve_data()
      scen <- scenario_grid()
      pal <- app_palette(theme_mode())
      alphas <- alpha_set()
      multi_alpha <- length(alphas) > 1
      cols <- cmp_alpha_palette(theme_mode())

      p <- plotly::plot_ly(source = paste0("curve_", family))

      # One line per alpha. The current alpha keeps the app's accent color
      # and a filled band beneath it (it's the primary answer); extra
      # comparison alphas get a plain line from the small qualitative
      # palette so they read as secondary context, not competing answers.
      for (i in seq_along(alphas)) {
        a <- alphas[i]
        is_current <- isTRUE(all.equal(a, current_alpha()))
        df_a <- df[df$alpha == a, ]
        line_color <- if (is_current) pal$accent else cols[[((i - 2) %% length(cols)) + 1]]
        p <- p |> plotly::add_trace(
          data = df_a, x = ~n, y = ~power,
          type = "scatter", mode = "lines",
          line = list(color = line_color, width = if (is_current) 3 else 2,
                       shape = "spline", dash = if (is_current) "solid" else "dot"),
          fill = if (is_current) "tozeroy" else "none",
          fillcolor = pal$fill,
          name = sprintf("alpha = %s", format(a)),
          showlegend = multi_alpha,
          hovertemplate = paste0(
            sprintf("<b>alpha = %s, N = ", format(a)), "%{x:,.0f}</b><br>power = %{y:.1%}<extra></extra>")
        )
      }

      # One labeled dashed reference line per power target.
      for (pt in power_set()) {
        p <- p |> plotly::add_trace(
          x = range(df$n), y = rep(pt, 2),
          type = "scatter", mode = "lines",
          line = list(color = pal$muted, width = 1.4, dash = "dash"),
          hoverinfo = "skip", showlegend = FALSE
        )
      }
      if (nrow(scen) > 0) {
        p <- p |> plotly::add_annotations(
          x = max(df$n), y = power_set(),
          text = sprintf("%d%% power", round(power_set() * 100)),
          showarrow = FALSE, xanchor = "right", yanchor = "bottom",
          font = list(color = pal$muted, size = 10.5), yshift = 3
        )
      }

      # Solution markers: the current scenario gets the original prominent
      # double-ring marker; extra comparison scenarios get a smaller dot
      # in their curve's own color.
      cur_pt <- scen[scen$is_current & !is.na(scen$n), ]
      other_pts <- scen[!scen$is_current & !is.na(scen$n), ]
      if (nrow(other_pts) > 0) {
        other_pts$color <- vapply(other_pts$alpha, function(a) {
          i <- which(alphas == a)
          if (isTRUE(all.equal(a, current_alpha()))) pal$positive else cols[[((i - 2) %% length(cols)) + 1]]
        }, character(1))
        p <- p |> plotly::add_trace(
          data = other_pts, x = ~n, y = ~power_achieved,
          type = "scatter", mode = "markers",
          marker = list(color = ~color, size = 8, line = list(color = pal$bg, width = 1.5)),
          hovertemplate = paste0(
            "<b>alpha = %{customdata[0]}, power target %{customdata[1]}</b><br>N = %{x:,.0f}<br>achieved power = %{y:.1%}<extra></extra>"),
          customdata = matrix(c(format(other_pts$alpha), sprintf("%d%%", round(other_pts$power * 100))), ncol = 2),
          showlegend = FALSE
        )
      }
      if (nrow(cur_pt) > 0) {
        p <- p |> plotly::add_trace(
          x = cur_pt$n, y = cur_pt$power_achieved,
          type = "scatter", mode = "markers",
          marker = list(color = pal$positive, size = 11,
                         line = list(color = pal$bg, width = 2.5)),
          hovertemplate = paste0(
            "<b>Your solution</b><br>N = %{x:,.0f}<br>power = %{y:.1%}<extra></extra>"),
          showlegend = FALSE
        )
      }

      ax <- list(
        gridcolor = pal$rule, zeroline = FALSE,
        linecolor = pal$rule, tickcolor = pal$rule,
        tickfont = list(color = pal$muted, size = 11),
        titlefont = list(color = pal$muted, size = 12)
      )

      p |>
        plotly::layout(
          paper_bgcolor = pal$bg,
          plot_bgcolor  = pal$bg,
          font   = list(family = "Inter, -apple-system, Segoe UI, sans-serif",
                         color = pal$ink),
          margin = list(l = 58, r = 18, t = if (multi_alpha) 40 else 16, b = 46),
          xaxis  = c(ax, list(title = "Sample size (N)")),
          yaxis  = c(ax, list(title = "Statistical power",
                               range = c(0, 1.02), tickformat = ".0%")),
          legend = if (multi_alpha) list(orientation = "h", x = 0, y = 1.18,
                                          font = list(color = pal$ink, size = 11)) else list(),
          hovermode = "x unified",
          hoverlabel = list(
            bgcolor = pal$surface, bordercolor = pal$rule,
            font = list(color = pal$ink, size = 12,
                         family = "Inter, -apple-system, sans-serif")
          )
        ) |>
        plotly::config(
          displaylogo = FALSE,
          # Trim the modebar down to the tools that are actually useful
          # here; the full default bar is visual clutter on a single line.
          modeBarButtonsToRemove = list(
            "select2d", "lasso2d", "autoScale2d", "hoverClosestCartesian",
            "hoverCompareCartesian", "toggleSpikelines", "zoomIn2d", "zoomOut2d"
          ),
          # PNG export (the modebar's own camera icon) is plotly's stock,
          # purely client-side feature -- no server round trip, so it's
          # immune to the iframe/Service-Worker download issue documented
          # elsewhere in this file. SVG export is added the same way, as a
          # second custom modebar button calling plotly.js's own
          # Plotly.downloadImage() client-side API (again no server
          # involvement at all) rather than inventing a new download path.
          toImageButtonOptions = list(
            format = "png", filename = paste0("power-curve-", family), scale = 2
          ),
          modeBarButtonsToAdd = list(
            list(
              name = "Download plot as SVG",
              icon = htmlwidgets::JS("Plotly.Icons.disk"),
              click = htmlwidgets::JS(sprintf(
                "function(gd) { Plotly.downloadImage(gd, {format: 'svg', filename: %s}); }",
                jsonlite::toJSON(paste0("power-curve-", family), auto_unbox = TRUE)
              ))
            )
          )
        )
    })
  }

  # ---- Static power curve (ggplot2 fallback) ---------------------------
  # Built as a reactive returning the ggplot OBJECT (not wired to an output
  # directly) so the same plot can be both rendered on-screen (renderPlot
  # below) and exported to a file by the SVG download handler further
  # down, without building it twice.
  ggplot_curve_obj <- reactive({
    res <- tryCatch(result_r(), error = function(e) NULL)
    validate(need(!is.null(res), "Complete the previous steps to see the power curve."))

    df <- curve_data()
    scen <- scenario_grid()
    APP_PALETTE <- app_palette(theme_mode())
    alphas <- alpha_set()
    multi_alpha <- length(alphas) > 1
    cols <- cmp_alpha_palette(theme_mode())
    alpha_color <- function(a) {
      if (isTRUE(all.equal(a, current_alpha()))) APP_PALETTE$accent
      else cols[[((which(alphas == a) - 2) %% length(cols)) + 1]]
    }
    df$color <- vapply(df$alpha, alpha_color, character(1))
    df$is_current <- vapply(df$alpha, function(a) isTRUE(all.equal(a, current_alpha())), logical(1))
    df$alpha_label <- factor(sprintf("alpha = %s", format(df$alpha)),
                              levels = sprintf("alpha = %s", format(alphas)))

    # Themed from the same palette as the UI (app_palette() resolves the
    # active light/dark theme reported by the browser), so the figure sits
    # inside the page rather than looking pasted in. Font family uses R's
    # generic "sans" alias rather than a named webfont, because the plot is
    # rendered by R's graphics device (server-side, or via webR in the
    # browser-only build) and has no access to CSS-loaded fonts.
    p <- ggplot2::ggplot()

    df_cur <- df[df$is_current, ]
    if (nrow(df_cur) > 0) {
      p <- p + ggplot2::geom_area(data = df_cur, ggplot2::aes(x = n, y = power),
                                   fill = APP_PALETTE$accent, alpha = 0.13)
    }
    rng <- shared_n_range()
    for (pt in power_set()) {
      p <- p + ggplot2::geom_hline(yintercept = pt, linetype = "22",
                                    color = APP_PALETTE$muted, linewidth = 0.5)
      if (!is.null(rng)) {
        # Always label BELOW the line (never above): a target near 99-100%
        # would otherwise have its label clipped by the panel's top edge.
        p <- p + ggplot2::annotate("text", x = rng[2], y = pt - 0.035,
                                    label = sprintf("%d%% power", round(pt * 100)),
                                    hjust = 1, vjust = 1, size = 3.2,
                                    color = APP_PALETTE$muted)
      }
    }
    p <- p +
      ggplot2::geom_line(
        data = df, ggplot2::aes(x = n, y = power, color = alpha_label,
                                 linetype = is_current, linewidth = is_current)
      ) +
      ggplot2::scale_color_manual(values = stats::setNames(df$color, df$alpha_label),
                                   guide = if (multi_alpha) "legend" else "none") +
      ggplot2::scale_linetype_manual(values = c(`TRUE` = "solid", `FALSE` = "22"), guide = "none") +
      ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.3, `FALSE` = 0.9), guide = "none")

    if (nrow(scen) > 0) {
      scen_ok <- scen[!is.na(scen$n), ]
      scen_ok$color <- vapply(scen_ok$alpha, alpha_color, character(1))
      cur_pt <- scen_ok[scen_ok$is_current, ]
      other_pts <- scen_ok[!scen_ok$is_current, ]
      if (nrow(other_pts) > 0) {
        p <- p +
          ggplot2::geom_point(data = other_pts, ggplot2::aes(x = n, y = power_achieved),
                               color = APP_PALETTE$bg, size = 4) +
          ggplot2::geom_point(data = other_pts, ggplot2::aes(x = n, y = power_achieved),
                               fill = other_pts$color, shape = 21, color = APP_PALETTE$bg,
                               size = 2.8, stroke = 0.6)
      }
      if (nrow(cur_pt) > 0) {
        p <- p +
          # Drop a guide line from the solution down to the x-axis, so the
          # required N is readable straight off the axis.
          ggplot2::geom_segment(
            data = cur_pt,
            ggplot2::aes(x = n, xend = n, y = 0, yend = power_achieved),
            color = APP_PALETTE$positive, linewidth = 0.45, linetype = "22"
          ) +
          ggplot2::geom_point(data = cur_pt, ggplot2::aes(x = n, y = power_achieved),
                               color = APP_PALETTE$bg, size = 5) +
          ggplot2::geom_point(data = cur_pt, ggplot2::aes(x = n, y = power_achieved),
                               color = APP_PALETTE$positive, size = 3.2)
      }
    }

    p +
      ggplot2::labs(
        x = "Sample size (N)", y = "Statistical power", color = NULL,
        title = "Power as a function of sample size",
        subtitle = if (multi_alpha) {
          "Dashed lines = power targets; marked points = each scenario's solution"
        } else {
          sprintf("Dashed horizontal line = target power (%.2f); marked point = your solution",
                   res$power_target)
        }
      ) +
      ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0),
                                   labels = scales_percent_labels) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.02))) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        text             = ggplot2::element_text(family = "sans", color = APP_PALETTE$ink),
        legend.position  = if (multi_alpha) "top" else "none",
        legend.title     = ggplot2::element_blank(),
        legend.text      = ggplot2::element_text(color = APP_PALETTE$ink, size = 10.5),
        plot.title       = ggplot2::element_text(face = "bold",
                                                  size = 15, color = APP_PALETTE$ink,
                                                  margin = ggplot2::margin(b = 4)),
        plot.subtitle    = ggplot2::element_text(size = 11, color = APP_PALETTE$muted,
                                                  margin = ggplot2::margin(b = 14)),
        plot.background  = ggplot2::element_rect(fill = APP_PALETTE$bg, color = NA),
        panel.background = ggplot2::element_rect(fill = APP_PALETTE$bg, color = NA),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = APP_PALETTE$rule, linewidth = 0.4),
        axis.line.x      = ggplot2::element_line(color = APP_PALETTE$rule, linewidth = 0.5),
        axis.ticks.x     = ggplot2::element_line(color = APP_PALETTE$rule, linewidth = 0.4),
        axis.ticks.length = ggplot2::unit(3, "pt"),
        axis.title       = ggplot2::element_text(size = 11, color = APP_PALETTE$muted),
        axis.title.x     = ggplot2::element_text(margin = ggplot2::margin(t = 9)),
        axis.title.y     = ggplot2::element_text(margin = ggplot2::margin(r = 9)),
        axis.text        = ggplot2::element_text(size = 10.5, color = APP_PALETTE$muted),
        plot.margin      = ggplot2::margin(6, 10, 4, 4)
      )
  })

  output$power_curve_plot <- renderPlot({
    req(!isTRUE(safeguard_diverged()))
    ggplot_curve_obj()
  })

  # SVG-only download for the ggplot2 fallback path (only reachable when
  # plotly failed to load -- see HAS_PLOTLY in global.R). PNG is skipped
  # here: it would need base64 encoding plus a JS-side change to the
  # shared Blob handler (atob() before constructing the Blob), for a
  # rarely-hit code path, since plotly -- which already has working PNG
  # export via its own modebar -- is the default. Uses the same
  # 'pw-download-blob-ready' mechanism as the HTML report / Save-project
  # buttons (see the comment above the dl_html handler below); SVG is
  # plain text, so it works with that handler completely unmodified.
  if (!HAS_PLOTLY) {
    observeEvent(input$dl_plot_svg, {
      p <- tryCatch(ggplot_curve_obj(), error = function(e) NULL)
      req(p)
      tmp_svg <- tempfile(fileext = ".svg")
      on.exit(unlink(tmp_svg), add = TRUE)
      ggplot2::ggsave(tmp_svg, plot = p, device = "svg", width = 8, height = 4.2)
      content <- paste(readLines(tmp_svg, warn = FALSE), collapse = "\n")
      session$sendCustomMessage("pw-download-blob-ready", list(
        content = content,
        filename = paste0("power-curve-", family, ".svg"),
        mime = "image/svg+xml"
      ))
    }, ignoreInit = TRUE)
  }

  output$sensitivity_result <- renderUI({
    if (isTRUE(safeguard_diverged())) return(NULL)
    n_max <- suppressWarnings(as.numeric(input$budget_n))
    if (is.null(n_max) || is.na(n_max) || n_max < 2) {
      return(helpText("Enter a maximum feasible N to see the minimum detectable effect."))
    }
    out <- tryCatch(sensitivity_fn(n_max), error = function(e) NULL)
    if (is.null(out)) return(helpText("Could not compute a sensitivity result for this N (try a larger value)."))
    div(class = "well well-info",
        icon("magnifying-glass"),
        tags$p(strong(sprintf(" Minimum detectable %s at N = %d: ", out$metric, round(n_max))),
               format_stat(out$value, 4)))
  })

  # ---- Attrition / exclusion inflation --------------------------------
  # Applied AFTER the statistical solve, never inside it: losing
  # participants doesn't change what the analysis needs, it changes how
  # many you must recruit to end up with that many. Shared across every
  # family via the same params-step input, exactly like the Bonferroni
  # control.
  # ---- Safeguard branch: is the prior study informative enough? --------
  # The safeguard bound is published_estimate - z_gamma * SE. Since
  # SE ~= estimate / t, that bound is approximately estimate * (1 - z/t),
  # so it collapses toward zero as the ORIGINAL study's t statistic
  # approaches z_gamma -- and the required N, which goes as 1/effect^2,
  # diverges. At the 80% default (z = 0.84) this needs t <= 0.84 and is
  # effectively unreachable for a published result, but the confidence
  # level is a user-facing slider: at 95% the threshold rises to t <= 1.65,
  # which a marginally significant prior study genuinely can sit below,
  # especially when the user is typing numbers off an abstract.
  #
  # Without this guard the two failure modes are both silent: the solver
  # either returns an absurd N (12,968 for d = 0.40 from n = 50/group at
  # 95%) or throws a uniroot "values at end points not of opposite sign"
  # error that the surrounding tryCatch swallows, leaving a blank panel.
  # The check is deliberately on the OUTCOME rather than on any one
  # effect-size metric, so it covers all sixteen families from this one
  # place without needing a metric-specific standard error here.
  # 10,000 rather than a larger round number because this app's audience
  # recruits paid participants (hence the budget panel below): a
  # five-figure N is already far outside anything fundable here, so a
  # recommendation that large is much more likely to be signalling an
  # uninformative prior than a genuinely feasible plan.
  IMPLAUSIBLE_N <- 10000L

  # TRUE when the safeguard branch has produced an effect indistinguishable
  # from the null. Both shapes of that outcome are covered: most families
  # error out of their root-finder, but a couple (paired, correlation)
  # return a finite value in the hundreds of millions instead. Both are
  # equally useless to a user, so they are treated identically -- and,
  # critically, the NUMBER IS SUPPRESSED rather than shown alongside the
  # explanation. Displaying "784,886,053" next to a warning would repeat
  # the original failure in a new shape: a figure the user cannot account
  # for, differing only in being large rather than wrongly signed.
  safeguard_diverged <- reactive({
    if (!identical(input$es_branch %||% "sesoi", "safeguard")) return(FALSE)
    res <- tryCatch(result_r(), error = function(e) NULL)
    n_tot <- if (is.null(res)) NA_integer_ else (res$n_total %||% res$n %||% NA_integer_)
    is.null(res) || is.na(n_tot) || n_tot >= IMPLAUSIBLE_N
  })

  output$safeguard_uninformative_note <- renderUI({
    if (!isTRUE(safeguard_diverged())) return(NULL)
    cl <- as.numeric(input$sg_conf_level %||% 0.80)
    div(class = "well well-warning", icon("triangle-exclamation"),
        HTML(sprintf(
          " <strong>This prior study is too imprecise to support a safeguard input at
           %.0f%% confidence.</strong> Its reported effect is small relative to its own
           uncertainty, so the confidence bound collapses onto the null and the required
           sample size grows without limit. No sample size is shown above because there
           is no meaningful one to show: this is the safeguard procedure working as
           intended, telling you the original evidence is too weak to plan on rather than
           returning a figure you could act on.
           Either lower the confidence level (the 80%% default is less demanding), or use
           the smallest-effect-of-interest branch, which does not depend on a prior
           estimate at all.",
          cl * 100)))
  })

  # ---- Multiplicity: show what the correction costs, not just its result --
  # When more than one comparison is planned, read_params_step() hands every
  # solver a per-test alpha and the corrected N is the only number the user
  # ever sees. That hides exactly the quantity Section 5.1 of the manuscript
  # argues is easy to overlook: multiplicity is cheap to ignore precisely
  # because its cost is absorbed silently into a single figure. The same
  # objection applies here as applied to the safeguard branch, which already
  # reports the naive and corrected requirements side by side, so this
  # mirrors that: solve a second time at the NOMINAL alpha and show both.
  #
  # solve_n_fn() is the family's own solver at an arbitrary alpha (it already
  # backs the alpha/power comparison scenarios), so no family-specific code
  # is needed here; display_n() guarantees the two figures are the same
  # quantity the value box reports rather than a total against a per-cell N.
  multiplicity <- reactive({
    p <- read_params_step(input)
    if (is.null(p$n_comparisons) || is.na(p$n_comparisons) || p$n_comparisons <= 1) return(NULL)
    if (is.null(solve_n_fn)) return(NULL)
    corrected <- display_n(tryCatch(result_r(), error = function(e) NULL))
    uncorrected <- display_n(tryCatch(solve_n_fn(p$alpha_nominal, p$power),
                                       error = function(e) NULL))
    if (is.na(corrected) || is.na(uncorrected)) return(NULL)
    list(k = p$n_comparisons, alpha_nominal = p$alpha_nominal, alpha_per = p$alpha,
         mc_method = p$mc_method, corrected = corrected, uncorrected = uncorrected)
  })

  output$multiplicity_note <- renderUI({
    # Suppressed alongside everything else when the safeguard bound has
    # collapsed: a pair of figures is no more meaningful there than one.
    if (isTRUE(safeguard_diverged())) return(NULL)
    m <- multiplicity()
    if (is.null(m)) return(NULL)
    extra <- m$corrected - m$uncorrected
    pct <- if (m$uncorrected > 0) 100 * extra / m$uncorrected else NA_real_
    div(class = "well well-warning", icon("layer-group"),
        HTML(sprintf(
          " <strong>%s of that total is the cost of planning %d comparisons.</strong>
           Treating &alpha; = %s as the family-wise rate gives a %s per-test
           &alpha; = %s, which raises the requirement from <strong>%s</strong>
           to <strong>%s</strong> (%s%%). The corrected figure is the one shown
           above and the one every calculation below uses -- attrition, budget,
           and the sensitivity analysis all follow from it.",
          format(extra, big.mark = ","), m$k,
          format_stat(m$alpha_nominal, 3),
          if (identical(m$mc_method, "sidak")) "&Scaron;id&aacute;k" else "Bonferroni",
          format_stat(m$alpha_per, 4),
          format(m$uncorrected, big.mark = ","), format(m$corrected, big.mark = ","),
          if (is.na(pct)) "?" else sprintf("+%.0f", pct))))
  })

  attrition_rate <- reactive(safe_numeric(input$attrition, 0, 0.95, 0))
  n_recruit <- reactive({
    res <- tryCatch(result_r(), error = function(e) NULL)
    if (is.null(res) || is.null(res$n_total)) return(NA_integer_)
    a <- attrition_rate()
    if (a <= 0) return(res$n_total)
    round_up_n(res$n_total / (1 - a))
  })

  output$attrition_note <- renderUI({
    if (isTRUE(safeguard_diverged())) return(NULL)
    a <- attrition_rate()
    if (is.na(a) || a <= 0) return(NULL)
    res <- tryCatch(result_r(), error = function(e) NULL)
    if (is.null(res) || is.null(res$n_total)) return(NULL)
    div(class = "well well-warning", icon("user-minus"),
        sprintf(" Allowing for %.0f%% attrition/exclusions, recruit N = %d to retain the %d your analysis needs (%d extra).",
                a * 100, n_recruit(), res$n_total, n_recruit() - res$n_total))
  })

  # ---- Budget ----------------------------------------------------------
  # Experimental economics pays its participants, so the sample size a
  # power analysis produces translates directly into a line in a grant
  # application. Both directions are offered: N -> cost, and (reusing the
  # family's own sensitivity_fn) budget -> affordable N -> smallest
  # detectable effect.
  output$budget_result <- renderUI({
    if (isTRUE(safeguard_diverged())) return(NULL)
    per <- safe_numeric(input$cost_per_participant, 0, 1e6, NA_real_)
    if (is.na(per) || per <= 0) {
      return(helpText("Enter what one participant costs to see the total, and what a fixed budget could buy."))
    }
    fixed <- safe_numeric(input$cost_fixed, 0, 1e9, 0)
    sym <- unname(currency_symbols()[input$currency %||% "EUR"])
    if (is.na(sym)) sym <- ""
    money <- function(v) format_money(v, sym)
    cur <- if (is.na(n_recruit())) NA_integer_ else n_recruit()
    if (is.na(cur)) return(NULL)
    total <- cur * per + fixed
    cap <- safe_numeric(input$budget_cap, 0, 1e9, NA_real_)

    afford_txt <- NULL
    if (!is.na(cap) && cap > fixed) {
      n_afford <- floor((cap - fixed) / per)
      # Convert an affordable RECRUITED N back to the analytic N the test
      # would actually be run on, then ask what effect that supports.
      n_analytic <- floor(n_afford * (1 - attrition_rate()))
      det <- if (n_analytic >= 2) tryCatch(sensitivity_fn(n_analytic), error = function(e) NULL) else NULL
      afford_txt <- tags$p(
        icon("wallet"),
        sprintf(" A budget of %s covers %d recruited participants (%d after attrition). ",
                money(cap), n_afford, n_analytic),
        if (!is.null(det)) {
          sprintf("At that size the smallest detectable %s is %s.", det$metric, format_stat(det$value, 4))
        } else {
          "That is too few participants to compute a detectable effect for this design."
        }
      )
    }

    div(class = "well well-result",
      tags$p(icon("coins"),
             sprintf(" Estimated cost for %d participants: %s%s.",
                     cur, money(total),
                     if (fixed > 0) sprintf(" (%s per participant plus %s fixed)", money(per), money(fixed)) else "")),
      afford_txt
    )
  })

  # Injects the multiple-comparisons disclosure (see the shared
  # "Number of planned comparisons" input in params_step_ui()) into
  # whatever spec the family itself built -- centralized here so no
  # individual family's report_spec_r needs to know about it.
  with_multiplicity <- function(spec) {
    m <- max(1L, as.integer(safe_numeric(input$n_comparisons, 1, 100, 1)))
    if (m > 1) {
      spec$n_comparisons <- m
      spec$alpha_nominal <- safe_numeric(input$alpha, 0.0001, 0.5, 0.05)
      spec$mc_method <- if (identical(input$mc_method, "sidak")) "sidak" else "bonferroni"
      # Carry the uncorrected requirement through as well, so the report
      # states what the correction cost rather than only that it was
      # applied -- the same disclosure the safeguard branch already makes.
      mm <- multiplicity()
      if (!is.null(mm)) spec$n_uncorrected <- mm$uncorrected
    }
    a <- attrition_rate()
    if (a > 0) {
      spec$attrition <- a
      spec$n_recruit <- n_recruit()
    }
    spec
  }

  output$report_text <- renderText({
    # Guarded FIRST, and deliberately: this is the one output the user
    # copies into a manuscript or pre-registration, so a sample size
    # derived from a collapsed safeguard bound reaching it is worse than
    # the same figure appearing on screen, where at least the warning
    # above it is visible in the same glance.
    validate(need(
      !isTRUE(safeguard_diverged()),
      paste("No report text: the prior study entered in the effect-size step is too",
            "imprecise to support a safeguard input at this confidence level, so there",
            "is no sample size to report. Lower the confidence level, or use the",
            "smallest-effect-of-interest branch instead.")
    ))
    spec <- tryCatch(report_spec_r(), error = function(e) NULL)
    validate(need(!is.null(spec), "Complete the previous steps to generate report text."))
    build_report_text(with_multiplicity(spec))
  })

  # Deliberately NOT downloadButton()/downloadHandler(): under the
  # shinylive/webR deployment the whole app runs inside an iframe (the
  # shinylive viewer's own architecture), and downloadHandler's file is
  # served through a "session/<token>/download/..." URL that relies on a
  # Service Worker to intercept it -- verified via CDP (against a real
  # local shinylive export, not just a plain `shiny::runApp()` server,
  # which has no iframe and would not have shown this) that this download
  # is silently canceled with 0 bytes. Building the file as a Blob
  # entirely in the browser -- see the shared 'pw-download-blob-ready'
  # handler in app.R, also used by the Save-project feature -- needs no
  # server-provided download endpoint at all, so it works identically in
  # both deployment modes.
  observeEvent(input$dl_html, {
    # Same reasoning as output$report_text: a downloaded artefact outlives
    # the on-screen warning entirely, so it must not be producible at all
    # while the safeguard bound has collapsed.
    req(!isTRUE(safeguard_diverged()))
    spec <- with_multiplicity(report_spec_r())
    txt <- build_report_text(spec)
    html <- htmltools::tagList(
      tags$head(tags$title("Power analysis report")),
      tags$body(
        style = "font-family: -apple-system, Segoe UI, Roboto, sans-serif; max-width: 720px; margin: 40px auto; color: #1F2A44;",
        tags$h2("A priori power analysis report"),
        tags$pre(style = "white-space: pre-wrap; font-family: inherit; background: #fbfbfd; border: 1px solid #e4e7f0; border-radius: 10px; padding: 16px;", txt),
        tags$p(style = "color: #5A6478; font-size: 12px;",
               tags$em(paste("Generated", Sys.Date(), "with the A Priori Power Analysis Wizard.")))
      )
    )
    tmp_html <- tempfile(fileext = ".html")
    on.exit(unlink(tmp_html), add = TRUE)
    htmltools::save_html(html, tmp_html)
    content <- paste(readLines(tmp_html, warn = FALSE), collapse = "\n")
    session$sendCustomMessage("pw-download-blob-ready", list(
      content = content,
      filename = paste0("power_analysis_report_", Sys.Date(), ".html"),
      mime = "text/html"
    ))
  }, ignoreInit = TRUE)

  # No dl_pdf handler at all under webR/shinylive -- there's no download
  # button for it (see results_panel_ui() above), and rmarkdown::render()
  # would fail anyway without a pandoc/LaTeX toolchain in that runtime.
  if (!IS_SHINYLIVE) {
    output$dl_pdf <- downloadHandler(
      filename = function() paste0("power_analysis_report_", Sys.Date(), ".pdf"),
      content = function(file) {
        req(!isTRUE(safeguard_diverged()))
        spec <- with_multiplicity(report_spec_r())
        txt <- build_report_text(spec)
        tmp_rmd <- tempfile(fileext = ".Rmd")
        writeLines(c(
          "---",
          "title: \"A priori power analysis report\"",
          "output: pdf_document",
          "---",
          "",
          "```{r echo=FALSE, comment=NA}",
          "cat(report_txt)",
          "```"
        ), tmp_rmd)
        report_txt <- txt
        result <- tryCatch({
          rmarkdown::render(tmp_rmd, output_file = file, envir = environment(), quiet = TRUE)
          TRUE
        }, error = function(e) FALSE)
        if (!isTRUE(result)) {
          # Fallback: PDF rendering needs a LaTeX engine (e.g. tinytex). If
          # unavailable in the deployment environment, we still write to
          # `file` (so the download completes) but with plain-text content
          # explaining why it isn't a real PDF, rather than failing silently.
          writeLines(c(
            "PDF export requires a LaTeX engine (e.g. install tinytex::install_tinytex()).",
            "Please use the HTML export instead, or install tinytex on the server.",
            "", "---", "", txt
          ), file)
        }
      }
    )
  }
}

#' Percent-formatted axis labels without pulling in the full `scales`
#' package as a hard dependency
#' @keywords internal
scales_percent_labels <- function(x) sprintf("%d%%", round(x * 100))
