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
    h4(icon("chart-simple"), " Step: Results"),
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

    uiOutput(ns("value_boxes")),
    div(class = "well well-result", uiOutput(ns("n_summary"))),

    accordion(
      id = ns("cmp_acc"), open = FALSE,
      accordion_panel(
        title = tagList(icon("layer-group"), " Compare across alpha / power (optional)"),
        value = "compare",
        p(class = "field-hint",
          "Add extra values to see them plotted together with your current settings below.",
          "Your current alpha and power (from the Statistical parameters step) are always included.",
          "Up to 3 alpha levels can be compared at once."),
        fluidRow(
          column(6, checkboxGroupInput(ns("cmp_alpha"), "Additional alpha (significance) levels:",
            choices = c("0.10" = "0.10", "0.05" = "0.05", "0.01" = "0.01", "0.001" = "0.001"),
            selected = character(0), inline = TRUE)),
          column(6, checkboxGroupInput(ns("cmp_power"), "Additional power targets:",
            choices = c("0.80" = "0.80", "0.90" = "0.90", "0.99" = "0.99"),
            selected = character(0), inline = TRUE))
        )
      )
    ),

    h5(icon("chart-line"), " Power curve"),
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
      plotOutput(ns("power_curve_plot"), height = "340px")
    },

    hr(),
    h5(icon("magnifying-glass-dollar"), " Sensitivity analysis (inverse): what if your budget is smaller?"),
    p(class = "field-hint",
      "If you can't reach the recommended N, enter the maximum sample size you can realistically collect to see the smallest effect that design could still detect."),
    fluidRow(
      column(6, numericInput(ns("budget_n"), "Maximum feasible N (total, or per cell for factorial ANOVA)",
                              value = NA, min = 2)),
      column(6, uiOutput(ns("sensitivity_result")))
    ),

    hr(),
    div(class = "report-text-header",
        h5(icon("file-lines"), " Paste-ready report / pre-registration text"),
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
#' @param solve_n_fn optional function(sig_level, power) -> the same list
#'   shape as `result_r()`, with every other input (effect size, tails,
#'   design-specific args) held at the family's current values. Powers the
#'   "Compare across alpha / power" chart controls; if omitted, the chart
#'   silently falls back to showing only the current single scenario.
#' @export
wire_results_server <- function(input, output, session, family,
                                 result_r, curve_extra_args_r, n_solution_r,
                                 sensitivity_fn, report_spec_r, n_summary_r,
                                 solve_n_fn = NULL) {

  # The x-axis quantity a curve point represents, per family: n1 (per-arm/
  # per-pair N) for every family except ANOVA (n_per_cell) and the two
  # single-sample-style families (n_total). Mirrors n_solution_r()'s own
  # per-family definition exactly, so a comparison scenario's marker lands
  # on the same x-axis meaning as the primary solution.
  extract_curve_n <- function(res) {
    if (family == "anova_factorial") res$n_per_cell
    else if (family %in% c("regression", "logistic", "chisq", "correlation")) res$n_total
    else if (family %in% c("mcnemar", "tost")) res$n
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

  # One row per (alpha, power) combination actually being compared, each
  # solved independently via the family's own solve_n_fn (everything else
  # held at the family's current values).
  scenario_grid <- reactive({
    # result_r() has already solved the CURRENT alpha/power combination --
    # reuse it (Shiny caches the reactive, so this is free) rather than
    # asking solve_n_fn() to redo the exact same solve a second time. This
    # matters even when no extra alpha/power is being compared: without
    # this, every single results render would silently pay for the
    # (potentially expensive -- e.g. ANOVA's or an unbalanced design's
    # search loop) solver twice for no benefit.
    cur_res <- result_r()
    if (is.null(solve_n_fn)) {
      return(data.frame(alpha = current_alpha(), power = current_power(),
                         n = extract_curve_n(cur_res), power_achieved = cur_res$power_achieved,
                         is_current = TRUE))
    }
    alphas <- alpha_set(); powers <- power_set()
    grid <- expand.grid(alpha = alphas, power = powers, KEEP.OUT.ATTRS = FALSE)
    rows <- lapply(seq_len(nrow(grid)), function(i) {
      is_cur <- isTRUE(all.equal(grid$alpha[i], current_alpha())) &&
                isTRUE(all.equal(grid$power[i], current_power()))
      res <- if (is_cur) {
        cur_res
      } else {
        tryCatch(solve_n_fn(grid$alpha[i], grid$power[i]), error = function(e) NULL)
      }
      data.frame(
        alpha = grid$alpha[i], power = grid$power[i],
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

  output$value_boxes <- renderUI({
    res <- tryCatch(result_r(), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    n_label <- if (family == "anova_factorial") "N per cell" else "Total N"
    n_display <- if (family == "anova_factorial") res$n_per_cell else (res$n_total %||% n_solution_r())

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

  output$n_summary <- renderUI({ n_summary_r() })

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
          # Trim the modebar down to the two tools that are actually useful
          # here; the full default bar is visual clutter on a single line.
          modeBarButtonsToRemove = list(
            "select2d", "lasso2d", "autoScale2d", "hoverClosestCartesian",
            "hoverCompareCartesian", "toggleSpikelines", "zoomIn2d", "zoomOut2d"
          ),
          toImageButtonOptions = list(
            format = "png", filename = paste0("power-curve-", family), scale = 2
          )
        )
    })
  }

  # ---- Static power curve (ggplot2 fallback) ---------------------------
  output$power_curve_plot <- renderPlot({
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

  output$sensitivity_result <- renderUI({
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

  # Injects the multiple-comparisons disclosure (see the shared
  # "Number of planned comparisons" input in params_step_ui()) into
  # whatever spec the family itself built -- centralized here so no
  # individual family's report_spec_r needs to know about it.
  with_multiplicity <- function(spec) {
    m <- max(1L, as.integer(safe_numeric(input$n_comparisons, 1, 100, 1)))
    if (m > 1) {
      spec$n_comparisons <- m
      spec$alpha_nominal <- safe_numeric(input$alpha, 0.0001, 0.5, 0.05)
    }
    spec
  }

  output$report_text <- renderText({
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
