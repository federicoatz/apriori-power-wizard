## app.R
## -----------------------------------------------------------------------
## Top-level wizard shell. Step 1 ("main analysis") lives here; once the
## user picks a test family, the app routes to that family's module
## (modules/mod_*.R), which owns its own internal sub-wizard (design ->
## parameters -> effect size -> results). Users can return to Step 1 at
## any time via "Change analysis type", and each family module preserves
## whatever the user already entered: a family's UI and server are built
## lazily, the first time it's actually visited (see navigate_to_family()
## in the server function), and then left mounted in the hidden
## tabsetPanel for the rest of the session, so nothing is lost switching
## between families or on Back/Next navigation within one.
##
## Step 1 also hosts a "Not sure which analysis to choose?" decision
## helper for users with no prior power-analysis experience: three plain-
## language questions map to a recommended test family.
## -----------------------------------------------------------------------

source("global.R")

## Deliberately a PLAIN bs_theme() with no color/font/radius customization.
## bslib ships a precompiled default Bootstrap 5 build and only invokes a
## live Sass recompilation (via the compiled `sass`/libsass bindings) when
## you pass customization args like `primary =` or `base_font =`. That
## live recompilation is what crashed the webR/shinylive (browser-only)
## build with a low-level WebAssembly ABI error ("call_indirect to a
## signature that does not match") right as the app started. All of this
## app's actual branding (colors, spacing, fonts, cards, icons) is
## already hardcoded in plain CSS in www/styles.css via the app's own
## `--pw-*` custom properties, NOT bslib's Sass variables -- so dropping
## the customized bs_theme() here has essentially no visual effect, but
## removes a runtime Sass compile that isn't reliable inside webR.
app_theme <- bs_theme(version = 5)

# Rich HTML labels for the Step-1 analysis picker: each option is a small
# "card" with an icon, a title, and a one-line plain-language description
# of when to use it -- aimed at users who have never run a power analysis
# and don't yet know the statistical vocabulary.
analysis_choice_names <- list(
  two_means = div(class = "choice-card",
    icon("arrows-left-right", class = "choice-icon"),
    div(strong("Two independent means"),
        p(class = "choice-desc", "Compare averages between two separate groups on a continuous outcome (e.g., spending, reaction time, a survey score)."))
  ),
  anova_factorial = div(class = "choice-card",
    icon("table-cells", class = "choice-icon"),
    div(strong("Factorial ANOVA (between-subjects)"),
        p(class = "choice-desc", "Compare means across two or more manipulated factors at once, possibly including whether they interact."))
  ),
  regression = div(class = "choice-card",
    icon("chart-line", class = "choice-icon"),
    div(strong("Multiple linear regression"),
        p(class = "choice-desc", "Test whether a continuous predictor relates to a continuous outcome, controlling for other variables."))
  ),
  logistic = div(class = "choice-card",
    icon("toggle-on", class = "choice-icon"),
    div(strong("Logistic regression"),
        p(class = "choice-desc", "Test whether a predictor relates to a yes/no outcome (e.g., purchased vs. did not purchase)."))
  ),
  proportions = div(class = "choice-card",
    icon("percent", class = "choice-icon"),
    div(strong("Two proportions"),
        p(class = "choice-desc", "Compare success/conversion rates between two independent groups (e.g., 32% vs. 41% completed the task)."))
  ),
  paired_t = div(class = "choice-card",
    icon("clock-rotate-left", class = "choice-icon"),
    div(strong("Paired / repeated measures"),
        p(class = "choice-desc", "Compare two conditions measured on the SAME participants (e.g., before vs. after an intervention)."))
  ),
  clustered_rct = div(class = "choice-card",
    icon("sitemap", class = "choice-icon"),
    div(strong("Clustered: sessions / matching groups"),
        p(class = "choice-desc", "Compare two conditions when participants interact in fixed groups (matching groups, lab sessions) or when whole groups -- classrooms, clinics, stores -- rather than individuals are randomized."))
  ),
  chisq = div(class = "choice-card",
    icon("chart-pie", class = "choice-icon"),
    div(strong("Chi-square (goodness-of-fit / independence)"),
        p(class = "choice-desc", "Compare counts or proportions across categories -- e.g., whether the distribution of a categorical choice differs across three or more treatment groups."))
  ),
  correlation = div(class = "choice-card",
    icon("arrow-trend-up", class = "choice-icon"),
    div(strong("Bivariate correlation"),
        p(class = "choice-desc", "Test whether two continuous variables measured on the same participants are related (Pearson's r)."))
  ),
  mcnemar = div(class = "choice-card",
    icon("arrow-right-arrow-left", class = "choice-icon"),
    div(strong("McNemar's test"),
        p(class = "choice-desc", "Compare a yes/no outcome measured on the SAME participants under two conditions -- the paired counterpart to 'Two proportions'."))
  ),
  tost = div(class = "choice-card",
    icon("scale-balanced", class = "choice-icon"),
    div(strong("Equivalence test (TOST)"),
        p(class = "choice-desc", "Test whether two independent groups are practically equivalent, rather than whether they differ."))
  ),
  ancova = div(class = "choice-card",
    icon("filter", class = "choice-icon"),
    div(strong("ANCOVA (with a covariate)"),
        p(class = "choice-desc", "Compare group means on a continuous outcome while adjusting for a covariate, e.g. a pre-test/baseline score."))
  ),
  survival = div(class = "choice-card",
    icon("hourglass-half", class = "choice-icon"),
    div(strong("Time-to-event (log-rank test)"),
        p(class = "choice-desc", "Compare how quickly two groups reach an event -- e.g., time to exit a market/game, or attrition in a longitudinal study."))
  ),
  wilcoxon = div(class = "choice-card",
    icon("ranking-star", class = "choice-icon"),
    div(strong("Mann-Whitney (nonparametric)"),
        p(class = "choice-desc", "Compare two independent groups on a skewed, bounded, or ordinal outcome using ranks instead of means -- the usual choice for contributions, bids, and earnings."))
  ),
  rm_anova = div(class = "choice-card",
    icon("repeat", class = "choice-icon"),
    div(strong("Repeated-measures ANOVA"),
        p(class = "choice-desc", "The same participants measured 3+ times -- across conditions, rounds, or time points -- including whether that change differs between groups."))
  ),
  clustered_cat = div(class = "choice-card",
    icon("diagram-project", class = "choice-icon"),
    div(strong("Clustered: binary or categorical outcome"),
        p(class = "choice-desc", "A yes/no or multi-category outcome when participants interact in matching groups/sessions, or whole groups are assigned together -- e.g. a cooperation rate by matching group."))
  )
)

#' Small reusable header shown atop every family module: a breadcrumb-style
#' "back" link, the family's title, and Save/Share-link controls for the
#' project-state feature -- so users always know where they are and can
#' always save or share whatever they've entered so far, regardless of
#' which family they're in.
#'
#' @param id character, the family's module id (used to namespace the
#'   back-link's input id as `<id>-back_to_step1`, since family UIs are now
#'   inserted lazily and can coexist in the DOM once visited -- see
#'   `navigate_to_family()` in the server function).
family_page_header <- function(id, title) {
  ns <- NS(id)
  div(class = "family-header",
    actionLink(ns("back_to_step1"),
               tagList(icon("arrow-left"), " Change analysis type"), class = "back-link"),
    div(class = "family-header-row",
      h3(title),
      div(class = "family-header-actions",
        actionButton(ns("save_project"), tagList(icon("floppy-disk"), " Save"),
                     class = "btn-outline-secondary btn-sm"),
        actionButton(ns("share_link"), tagList(icon("link"), " Share link"),
                     class = "btn-outline-secondary btn-sm")
      )
    )
  )
}

# Lookup tables driving lazy family loading (see navigate_to_family() in the
# server function): title, UI constructor, and server constructor per family
# key. Kept as plain named lists (not vectors of function names) so the
# actual mod_<family>_ui/mod_<family>_server functions stay easy to find via
# a simple text search.
family_titles <- list(
  two_means = "Two independent means",
  anova_factorial = "Factorial ANOVA",
  regression = "Multiple linear regression",
  logistic = "Logistic regression",
  proportions = "Two proportions",
  paired_t = "Paired / repeated measures",
  clustered_rct = "Clustered: sessions / matching groups",
  chisq = "Chi-square (goodness-of-fit / independence)",
  correlation = "Bivariate correlation",
  mcnemar = "McNemar's test",
  tost = "Equivalence test (TOST)",
  ancova = "ANCOVA (with a covariate)",
  survival = "Time-to-event (log-rank test)",
  wilcoxon = "Mann-Whitney (nonparametric)",
  rm_anova = "Repeated-measures ANOVA",
  clustered_cat = "Clustered: binary or categorical outcome"
)
family_ui_fns <- list(
  two_means = mod_two_means_ui, anova_factorial = mod_anova_factorial_ui,
  regression = mod_regression_ui, logistic = mod_logistic_ui,
  proportions = mod_proportions_ui, paired_t = mod_paired_t_ui,
  clustered_rct = mod_clustered_ui, chisq = mod_chisq_ui,
  correlation = mod_correlation_ui, mcnemar = mod_mcnemar_ui, tost = mod_tost_ui,
  ancova = mod_ancova_ui, survival = mod_survival_ui,
  wilcoxon = mod_wilcoxon_ui, rm_anova = mod_rm_anova_ui,
  clustered_cat = mod_clustered_cat_ui
)
family_server_fns <- list(
  two_means = mod_two_means_server, anova_factorial = mod_anova_factorial_server,
  regression = mod_regression_server, logistic = mod_logistic_server,
  proportions = mod_proportions_server, paired_t = mod_paired_t_server,
  clustered_rct = mod_clustered_server, chisq = mod_chisq_server,
  correlation = mod_correlation_server, mcnemar = mod_mcnemar_server, tost = mod_tost_server,
  ancova = mod_ancova_server, survival = mod_survival_server,
  wilcoxon = mod_wilcoxon_server, rm_anova = mod_rm_anova_server,
  clustered_cat = mod_clustered_cat_server
)

ui <- page_fluid(
  theme = app_theme,
  title = "A Priori Power Analysis Wizard",
  tags$head(
    # Webfonts: Inter, SELF-HOSTED from www/fonts/ via @font-face rules at the
    # top of styles.css -- deliberately not loaded from Google Fonts.
    #
    # It used to be. Auditing the live site's network traffic showed that the
    # Google Fonts request was the ONLY thing this application fetched from a
    # third party, and loading a webfont from a CDN transmits the visitor's IP
    # address to that CDN on every page view. Serving the font ourselves means
    # the app now contacts nothing but its own origin, which is a much cleaner
    # position for a tool whose whole premise is that nothing you enter leaves
    # your browser -- and it removes a dependency on an external service being
    # reachable at all. Inter ships as a variable font, so the cost is two
    # files (~130KB total) against a wasm payload measured in megabytes.
    #
    # styles.css still declares system fallbacks, so if the font files fail to
    # load the app degrades to the native UI sans and stays perfectly legible;
    # `font-display: swap` means text renders immediately in that fallback and
    # re-renders once Inter arrives, rather than blocking on it.
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),

    # ------------------------------------------------------------------
    # Theme bootstrap. Runs BEFORE the body paints so the app never shows
    # a light flash before switching to dark ("FOUC"). Resolution order:
    #   1. the visitor's saved choice (localStorage)
    #   2. their OS-level preference (prefers-color-scheme)
    #   3. light
    # All the actual colours live in www/styles.css under [data-theme=...];
    # this only sets the attribute.
    # ------------------------------------------------------------------
    tags$script(HTML("
      (function () {
        var saved = null;
        try { saved = window.localStorage.getItem('pw-theme'); } catch (e) {}
        var prefersDark = window.matchMedia &&
                          window.matchMedia('(prefers-color-scheme: dark)').matches;
        var mode = saved || (prefersDark ? 'dark' : 'light');
        document.documentElement.setAttribute('data-theme', mode);
      })();
    ")),
    # Same FOUC-avoidance bootstrap as the theme script above, for the
    # Guided-mode toggle: resolved and applied to the root element before
    # first paint, purely from localStorage (defaults to off -- guided
    # mode is opt-in, so returning/experienced users see the app exactly
    # as before unless they turn it on).
    tags$script(HTML("
      (function () {
        var saved = null;
        try { saved = window.localStorage.getItem('pw-guided'); } catch (e) {}
        document.documentElement.setAttribute('data-guided', saved === 'true' ? 'true' : 'false');
      })();
    "))
  ),

  div(class = "app-header",
      div(class = "app-header-icon", icon("calculator")),
      div(
        h1("A Priori Power Analysis Wizard"),
        p("A step-by-step tool for determining sample size before you collect data —",
          "built for researchers in experimental economics and behavioral science,",
          "no prior power-analysis experience required.")
      ),
      div(class = "header-toggles",
        # Rendered as plain HTML buttons rather than Shiny inputs: theme
        # switching is purely cosmetic and entirely client-side, so routing it
        # through the server would add a needless round trip (and would feel
        # laggy in the shinylive build, where "the server" is a WebAssembly
        # R process in the same tab).
        div(class = "theme-toggle", role = "group", `aria-label` = "Color theme",
            tags$button(id = "pw-theme-light", type = "button",
                        title = "Light theme", `aria-label` = "Light theme",
                        `aria-pressed` = "false", icon("sun")),
            tags$button(id = "pw-theme-dark", type = "button",
                        title = "Dark theme", `aria-label` = "Dark theme",
                        `aria-pressed` = "false", icon("moon"))
        ),
        # Guided mode: shows extra plain-language "explain this" boxes
        # throughout every step (all pre-rendered in the DOM, toggled
        # purely via CSS on `[data-guided]` -- see styles.css -- so
        # flipping it takes effect instantly on every step and family,
        # including ones already visited in this session, with no
        # server round trip).
        tags$button(id = "pw-guided-toggle", type = "button", class = "guided-toggle-btn",
                    title = "Toggle extra beginner-friendly explanations throughout the app",
                    `aria-pressed` = "false",
                    icon("graduation-cap"), tags$span("Guided mode"))
      )
  ),

  # NOTE: family tabs (two_means, anova_factorial, ...) are NOT listed here.
  # Building all 7 families' UI (plus registering all 7 servers' reactives/
  # observers/outputs) on every single page load was the dominant cost in
  # the browser-only (shinylive/webR) build once the network-transfer side
  # was fixed -- browser devtools showed the page still busy well after
  # every network request had finished, meaning the remaining time was R
  # itself executing this startup code inside the WASM sandbox, not
  # waiting on downloads. Each family tab is instead inserted into this
  # tabsetPanel (via insertTab()) and its server instantiated (via
  # moduleServer()) lazily, the first time the user actually navigates to
  # it -- see navigate_to_family() below. Once inserted, a tab is never
  # removed, so returning to a family already visited preserves everything
  # the user entered, exactly as before.
  tabsetPanel(
    id = "main_nav", type = "hidden",

    tabPanelBody("choose_analysis",
      # Hidden by CSS the moment Guided mode is already on (including a
      # returning visitor whose choice was already saved) -- see
      # `html[data-guided="true"] .guided-prompt` in styles.css -- so
      # this only ever appears as a one-time, unmissable nudge toward the
      # (otherwise easy-to-miss, small pill in the header) toggle.
      div(class = "guided-prompt",
        icon("graduation-cap", class = "guided-prompt-icon"),
        div(
          strong("New here? "),
          "Turn on Guided mode for a plain-language explanation of what",
          "to do at every step -- no prior power-analysis experience needed."
        ),
        tags$button(id = "pw-guided-prompt-btn", type = "button",
                    class = "btn guided-prompt-btn", "Turn on Guided mode")
      ),
      div(class = "step-card",
        h4(icon("flag-checkered"), " Step 1: What is your main analysis?"),
        p(class = "step-intro",
          "This is the type of statistical test you plan to run once your data are",
          "collected. If you already know, pick it below. If not, use the helper",
          "underneath the options first."),
        radioButtons("analysis_choice", NULL,
          choiceNames = unname(analysis_choice_names),
          choiceValues = names(analysis_choice_names),
          selected = character(0)),
        div(class = "step1-actions-row",
          actionButton("start_wizard", tagList("Start", icon("arrow-right")), class = "btn-primary btn-lg mt-2"),
          div(class = "load-project",
            fileInput("load_project_file", NULL, accept = ".json",
                      buttonLabel = tagList(icon("folder-open"), " Load a saved project"),
                      placeholder = "No file selected")
          )
        )
      ),

      # Third entry point into the app, alongside "I know my test" (the
      # cards above) and "help me choose" (the decision helper below):
      # "show me one like mine". Each example loads a COMPLETE design --
      # every step populated, landing on the finished results -- because
      # what a first-time user is missing is usually not the arithmetic
      # but a sense of what a whole plausible setup looks like.
      accordion(
        id = "example_library_acc", open = FALSE,
        accordion_panel(
          title = tagList(icon("flask"), " Start from a worked example instead"),
          value = "examples",
          p(class = "field-hint",
            "Each of these loads a complete, ready-made design from a paradigm you may recognise, landing straight on the results so you can see what a finished analysis looks like. Use Back inside it to inspect every step."),
          div(class = "well well-warning", icon("triangle-exclamation"),
              strong(" Before you borrow these numbers: "), example_library_caveat()),
          div(class = "example-grid", uiOutput("example_cards"))
        )
      ),

      accordion(
        id = "decision_helper_acc", open = FALSE,
        accordion_panel(
          title = tagList(icon("compass"), " Not sure which analysis to choose? Answer a few quick questions"),
          value = "helper",
          fluidRow(
            column(4,
              radioButtons("dh_unit", "1. Do participants act independently, or in groups?",
                choices = c("Independently: each participant's outcome stands alone" = "individual",
                            "In groups: they interact in matching groups/sessions, or whole groups (classrooms, clinics, stores) are assigned together" = "cluster"),
                selected = character(0)),
              div(class = "field-hint", icon("circle-info"),
                  " This matters even before the outcome type. If your participants interact with each other -- a public goods game, a market, a bargaining task -- their observations are not independent, and treating them as if they were is one of the most common and most-criticized errors in experimental design.")
            ),
            column(4,
              conditionalPanel("input.dh_unit",
                radioButtons("dh_outcome", "2. What kind of outcome are you measuring?",
                  choices = c("A number that can take a range of values (e.g., score, time, spending)" = "continuous",
                              "A yes/no or success/failure outcome" = "binary",
                              "A choice among 3+ categories, or counts spread across groups" = "categorical",
                              "How LONG until something happens (time-to-event)" = "time_to_event"),
                  selected = character(0))
              )
            ),
            column(4,
              conditionalPanel("input.dh_unit == 'individual' && input.dh_outcome == 'continuous'",
                radioButtons("dh_continuous", "3. How is the outcome explained?",
                  choices = c("Two separate groups (e.g., treatment vs. control)" = "two_groups",
                              "The SAME participants measured twice (e.g., before/after)" = "paired",
                              "The SAME participants measured 3+ times (conditions, rounds, time points)" = "repeated",
                              "Two or more manipulated factors at once" = "factorial",
                              "A continuous predictor (possibly with other control variables)" = "predictor",
                              "Groups, adjusting for a covariate (e.g., a pre-test/baseline score)" = "ancova",
                              "Two separate groups, but the outcome is skewed/bounded/ordinal (use ranks)" = "nonparametric"),
                  selected = character(0))
              ),
              conditionalPanel("input.dh_unit == 'individual' && input.dh_outcome == 'binary'",
                radioButtons("dh_binary", "3. How is the outcome explained?",
                  choices = c("Two separate groups' success rates" = "two_groups",
                              "The SAME participants measured twice (e.g., before/after)" = "paired",
                              "A predictor (binary or continuous)" = "predictor"),
                  selected = character(0))
              ),
              conditionalPanel("input.dh_unit == 'individual' && input.dh_outcome == 'categorical'",
                div(class = "field-hint", icon("circle-check"),
                    " A categorical outcome across groups -- see the recommendation below.")
              ),
              conditionalPanel("input.dh_unit == 'individual' && input.dh_outcome == 'time_to_event'",
                div(class = "field-hint", icon("circle-check"),
                    " A time-to-event outcome -- see the recommendation below.")
              ),
              conditionalPanel("input.dh_unit == 'cluster' && input.dh_outcome == 'continuous'",
                div(class = "field-hint", icon("circle-check"),
                    " A grouped design (matching groups/sessions, or cluster-randomized) with a continuous outcome -- see the recommendation below.")
              ),
              conditionalPanel("input.dh_unit == 'cluster' && (input.dh_outcome == 'binary' || input.dh_outcome == 'categorical')",
                div(class = "field-hint", icon("circle-check"),
                    " A grouped design with a binary or categorical outcome -- see the recommendation below.")
              ),
              conditionalPanel("input.dh_unit == 'cluster' && input.dh_outcome == 'time_to_event'",
                div(class = "field-hint", icon("triangle-exclamation"),
                    " This combination isn't covered by this wizard yet -- see the note below.")
              )
            )
          ),
          uiOutput("dh_recommendation")
        )
      )
    )
  ),

  tags$footer(class = "app-footer",
    p(icon("circle-info"), " This tool performs closed-form (non-simulation) a priori power analysis.",
      "It does not replace statistical consultation for complex or non-standard designs."),
    p(icon("code-branch"), " Source code on ",
      tags$a(href = "https://github.com/federicoatz/power-analysis-app",
             target = "_blank", rel = "noopener noreferrer", "GitHub"),
      " -- formulas and method references for every analysis family are credited in the ",
      tags$a(href = "https://github.com/federicoatz/power-analysis-app#method-references",
             target = "_blank", rel = "noopener noreferrer", "README"), "."),
    p(icon("scale-balanced"), " Released under the ",
      tags$a(href = "https://github.com/federicoatz/power-analysis-app/blob/main/LICENSE",
             target = "_blank", rel = "noopener noreferrer", "MIT license"), "."),
    # Absolute URL rather than a relative one: this footer is rendered INSIDE
    # the shinylive iframe, whose base URL is a dynamically generated internal
    # path, so anything relative would resolve against that instead of the site.
    p(icon("user-shield"), " This tool sets no cookies and nothing you enter leaves your browser -- see the ",
      tags$a(href = "https://github.com/federicoatz/power-analysis-app/blob/main/PRIVACY.md",
             target = "_blank", rel = "noopener noreferrer", "privacy notice"), "."),
    # NOTE: the Zenodo DOI is deliberately omitted right now. The previous
    # records were withdrawn (tombstoned) in August 2026 when the repository
    # history was rebuilt, and the GitHub-Zenodo integration has not yet been
    # re-established for the new repository -- so there is currently no live
    # DOI to point at. Linking the old one would send readers to a dead
    # record. Restore the DOI link here as soon as a new archive is minted.
    p(icon("quote-left"), " If you use this app in your research, please cite: ",
      tags$em("Atzori, F. (2026). A Priori Power Analysis Wizard (Version v0.15.0) [Computer software]."),
      " ",
      tags$a(href = "https://github.com/federicoatz/power-analysis-app",
             target = "_blank", rel = "noopener noreferrer",
             "https://github.com/federicoatz/power-analysis-app"))
  ),

  # =====================================================================
  # Client-side behaviour: theme toggle + animated result counters.
  # Placed at the end of the body so every element it binds to exists.
  # =====================================================================
  tags$script(HTML("
    (function () {

      /* ---------------- Theme toggle ---------------------------------
         The initial data-theme was already set by the inline script in
         <head> (before first paint, to avoid a flash). This part wires
         the two buttons, persists the choice, and -- importantly --
         tells R which theme is active, because the power curve is drawn
         server-side (or in webR) and needs matching colours. */
      var root  = document.documentElement;
      var bLight = document.getElementById('pw-theme-light');
      var bDark  = document.getElementById('pw-theme-dark');

      function currentTheme() {
        return root.getAttribute('data-theme') === 'dark' ? 'dark' : 'light';
      }

      function notifyShiny(mode) {
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue('pw_theme', mode, { priority: 'event' });
        }
      }

      function applyTheme(mode, persist) {
        root.setAttribute('data-theme', mode);
        if (bLight) bLight.setAttribute('aria-pressed', String(mode === 'light'));
        if (bDark)  bDark.setAttribute('aria-pressed',  String(mode === 'dark'));
        if (persist) {
          try { window.localStorage.setItem('pw-theme', mode); } catch (e) {}
        }
        notifyShiny(mode);
      }

      if (bLight) bLight.addEventListener('click', function () { applyTheme('light', true); });
      if (bDark)  bDark.addEventListener('click',  function () { applyTheme('dark',  true); });

      /* Reflect the already-applied theme in the buttons straight away. */
      applyTheme(currentTheme(), false);

      /* Shiny isn't listening yet when the code above first runs, so send
         the initial value again once the session is up. */
      document.addEventListener('shiny:connected', function () {
        notifyShiny(currentTheme());
      });

      /* If the visitor has never made an explicit choice, follow their OS
         when it changes (e.g. macOS auto light/dark at sunset). */
      if (window.matchMedia) {
        var mq = window.matchMedia('(prefers-color-scheme: dark)');
        var onChange = function (e) {
          var saved = null;
          try { saved = window.localStorage.getItem('pw-theme'); } catch (err) {}
          if (!saved) applyTheme(e.matches ? 'dark' : 'light', false);
        };
        if (mq.addEventListener) mq.addEventListener('change', onChange);
        else if (mq.addListener) mq.addListener(onChange);
      }

      /* ---------------- Guided mode toggle -----------------------------
         Purely client-side (unlike the theme, nothing server-side needs
         to know about it): flips a `data-guided` attribute on the root
         element, which styles.css uses to show/hide every `.guided-
         explain` box already present (but hidden) in the DOM across
         every step and family -- including ones already mounted earlier
         in this session, since CSS re-evaluates instantly on the
         attribute change with no Shiny round trip or re-render needed. */
      var guidedBtn = document.getElementById('pw-guided-toggle');
      function currentGuided() {
        return root.getAttribute('data-guided') === 'true';
      }
      function applyGuided(on, persist) {
        root.setAttribute('data-guided', String(on));
        if (guidedBtn) guidedBtn.setAttribute('aria-pressed', String(on));
        if (persist) {
          try { window.localStorage.setItem('pw-guided', String(on)); } catch (e) {}
        }
      }
      if (guidedBtn) {
        guidedBtn.addEventListener('click', function () { applyGuided(!currentGuided(), true); });
        applyGuided(currentGuided(), false);
      }
      /* Step-1 banner button: same action as the header toggle, just a
         second, unmissable entry point to it (see the CSS rule that
         hides the banner once guided mode is already on). */
      var guidedPromptBtn = document.getElementById('pw-guided-prompt-btn');
      if (guidedPromptBtn) {
        guidedPromptBtn.addEventListener('click', function () { applyGuided(true, true); });
      }

      /* ---------------- Selection highlighting ------------------------
         Radio/checkbox selection state is marked by adding a
         `pw-selected` class to the wrapper element, and keyboard focus by
         `pw-focus`. This is done in JS rather than with a CSS sibling
         selector (`input:checked ~ .card`) on purpose: Shiny's generated
         markup for a radio group differs between Bootstrap 3 and 5 and
         between Shiny versions -- sometimes the <input> is a sibling of
         the <label>, sometimes it's nested inside it -- so any structural
         CSS selector is a coin flip. Reading `.checked` and setting a
         class works in every layout.

         `label.radio-inline`/`label.checkbox-inline` (radioButtons/
         checkboxGroupInput with inline = TRUE -- the ANOVA factor-count
         radio, the alpha/power comparison checkboxes) is a DIFFERENT
         shape again: Shiny puts <input> directly inside that label, no
         wrapping .radio/.form-check div at all, so it needs its own
         entry in this selector list or it silently matches nothing (see
         www/styles.css for the matching chip-style CSS). */
      function markSelection() {
        var groups = document.querySelectorAll(
          '.shiny-input-radiogroup, .shiny-input-checkboxgroup');
        groups.forEach(function (g) {
          g.querySelectorAll('input[type=\"radio\"], input[type=\"checkbox\"]')
            .forEach(function (inp) {
              var item = inp.closest('.radio, .checkbox, .form-check, label.radio-inline, label.checkbox-inline');
              if (item) item.classList.toggle('pw-selected', inp.checked);
            });
        });
      }

      /* User clicks / keyboard activation. Delegated, so it keeps working
         for controls Shiny inserts later. */
      document.addEventListener('change', function (e) {
        var t = e.target;
        if (!t || (t.type !== 'radio' && t.type !== 'checkbox')) return;
        markSelection();
      });

      /* Visible focus ring on the card itself, since the real radio input
         is visually hidden (it stays in the DOM and stays focusable -- it
         is NOT display:none -- so keyboard and screen-reader users can
         still operate the group normally). */
      document.addEventListener('focusin', function (e) {
        var t = e.target;
        if (!t || (t.type !== 'radio' && t.type !== 'checkbox')) return;
        var item = t.closest('.radio, .checkbox, .form-check, label.radio-inline, label.checkbox-inline');
        if (item) item.classList.add('pw-focus');
      });
      document.addEventListener('focusout', function (e) {
        var t = e.target;
        if (!t || (t.type !== 'radio' && t.type !== 'checkbox')) return;
        var item = t.closest('.radio, .checkbox, .form-check, label.radio-inline, label.checkbox-inline');
        if (item) item.classList.remove('pw-focus');
      });

      /* Programmatic updates (updateRadioButtons -- e.g. the Step-1
         decision helper's \"Use this and start\" button) don't fire a
         change event, so listen for Shiny's own update event too. These
         are jQuery events, hence the jQuery path. */
      if (window.jQuery) {
        window.jQuery(document).on('shiny:updateinput shiny:value', function () {
          setTimeout(function () { markSelection(); }, 0);
        });
      }

      /* ---------------- Copy report text to clipboard -----------------
         Delegated (like markSelection above), so it works for every
         family's copy button without needing per-family JS. Reads the
         rendered <pre> text directly from the DOM rather than round-
         tripping through Shiny -- the text is already there and correct
         the moment report_text has rendered. */
      function fallbackCopy(text) {
        var ta = document.createElement('textarea');
        ta.value = text;
        ta.setAttribute('readonly', '');
        ta.style.position = 'fixed';
        ta.style.opacity = '0';
        document.body.appendChild(ta);
        ta.focus(); ta.select();
        var ok = false;
        try { ok = document.execCommand('copy'); } catch (e) { ok = false; }
        document.body.removeChild(ta);
        return ok;
      }

      document.addEventListener('click', function (e) {
        var btn = e.target.closest('.copy-report-btn');
        if (!btn) return;
        var el = document.getElementById(btn.getAttribute('data-target'));
        var label = btn.querySelector('.copy-report-label');
        if (!el || !label) return;
        var text = el.textContent;
        var original = label.textContent;

        function announce(ok) {
          label.textContent = ok ? ' Copied!' : ' Could not copy';
          btn.classList.toggle('copy-report-btn--done', ok);
          setTimeout(function () {
            label.textContent = original;
            btn.classList.remove('copy-report-btn--done');
          }, 1600);
        }

        if (navigator.clipboard && navigator.clipboard.writeText && window.isSecureContext) {
          navigator.clipboard.writeText(text).then(
            function () { announce(true); },
            function () { announce(fallbackCopy(text)); }
          );
        } else {
          announce(fallbackCopy(text));
        }
      });

      /* ---------------- Client-side Blob download -----------------------
         Shared by Save-project (JSON) and Download HTML report --
         deliberately NOT downloadButton()/downloadHandler(): under the
         shinylive/webR deployment the whole app runs inside an iframe
         (the viewer's own architecture), and downloadHandler's file is
         served through a 'session/<token>/download/...' URL that relies
         on a Service Worker to intercept it -- verified via CDP (against
         a real local shinylive export, not just a plain
         `shiny::runApp()` server, which has no iframe and would not have
         shown this) that the download is silently canceled with 0 bytes,
         for every family and both report formats. Building the file as a
         Blob entirely here needs no server-provided download endpoint at
         all, so it works identically in both deployment modes. */
      Shiny.addCustomMessageHandler('pw-download-blob-ready', function (msg) {
        var blob = new Blob([msg.content], { type: msg.mime || 'text/plain' });
        var url = URL.createObjectURL(blob);
        var a = document.createElement('a');
        a.href = url;
        a.download = msg.filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        setTimeout(function () { URL.revokeObjectURL(url); }, 1000);
      });

      /* ---------------- Share-link copy feedback -----------------------
         Targets `window.parent` rather than `window`: under the
         shinylive viewer this app runs inside an iframe with its own
         dynamically generated, non-shareable URL, so the address bar the
         user actually sees and copies from is the PARENT window's --
         same-origin, so directly accessible. `window.parent` is simply
         `window` itself when this page is not embedded in any frame
         (e.g. a plain `shiny::runApp()` deployment), so the same code
         works unchanged there too. */
      Shiny.addCustomMessageHandler('pw-share-link-ready', function (msg) {
        var btn = document.getElementById(msg.btn_id);
        if (!btn) return;
        var url;
        try {
          var loc = new URL(window.parent.location.href);
          loc.searchParams.set('state', msg.query);
          window.parent.history.replaceState(null, '', loc.toString());
          url = loc.toString();
        } catch (e) {
          url = window.location.href;
        }
        var original = btn.innerHTML;

        function announce(ok) {
          btn.innerHTML = ok ? ' Link copied!' : ' Could not copy';
          setTimeout(function () { btn.innerHTML = original; }, 1600);
        }

        if (navigator.clipboard && navigator.clipboard.writeText && window.isSecureContext) {
          navigator.clipboard.writeText(url).then(
            function () { announce(true); },
            function () { announce(fallbackCopy(url)); }
          );
        } else {
          announce(fallbackCopy(url));
        }
      });

      /* ---------------- Restore from a shared link ----------------------
         Reads `?state=` from the PARENT window's URL (see the share-link
         handler above for why `window.parent` rather than `window`) once
         Shiny has connected, and forwards it to the server as a root-
         level input -- the same pattern already used for `pw_theme`
         below. */
      $(document).on('shiny:sessioninitialized', function () {
        try {
          var params = new URLSearchParams(window.parent.location.search);
          var state = params.get('state');
          if (state) Shiny.setInputValue('pw_initial_share_state', state, { priority: 'event' });
        } catch (e) { /* cross-origin parent should not happen (same-origin) */ }
      });

      /* ---------------- Animated result counters ---------------------
         When a value box's number changes, count up to it instead of
         snapping. Purely decorative: the DOM already holds the final,
         correct string before the animation starts, and the animation
         always ends on exactly that string -- so nothing here can alter
         a displayed result. Honours prefers-reduced-motion. */
      var reduceMotion = window.matchMedia &&
        window.matchMedia('(prefers-reduced-motion: reduce)').matches;

      function animateNumber(el) {
        if (reduceMotion) return;
        var finalText = el.textContent;
        var m = finalText.match(/-?[\\d,]*\\.?\\d+/);
        if (!m) return;

        var numText  = m[0];
        var target   = parseFloat(numText.replace(/,/g, ''));
        if (!isFinite(target)) return;

        var prefix   = finalText.slice(0, m.index);
        var suffix   = finalText.slice(m.index + numText.length);
        var decimals = (numText.split('.')[1] || '').length;
        var grouped  = numText.indexOf(',') !== -1;

        function fmt(v) {
          var s = v.toFixed(decimals);
          if (grouped) {
            var parts = s.split('.');
            parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
            s = parts.join('.');
          }
          return prefix + s + suffix;
        }

        var start = null, DUR = 620;
        function frame(ts) {
          if (start === null) start = ts;
          var k = Math.min((ts - start) / DUR, 1);
          var eased = 1 - Math.pow(1 - k, 3);
          if (k < 1) {
            el.textContent = fmt(target * eased);
            requestAnimationFrame(frame);
          } else {
            el.textContent = finalText;   /* land on the exact original */
          }
        }
        requestAnimationFrame(frame);
      }

      /* Value boxes are re-rendered wholesale by renderUI, so watch for
         inserted nodes rather than trying to bind to elements directly. */
      var seen = new WeakSet();
      function scan(node) {
        if (!node.querySelectorAll) return;
        node.querySelectorAll('.value-box-value').forEach(function (el) {
          if (seen.has(el)) return;
          seen.add(el);
          animateNumber(el);
        });
      }

      new MutationObserver(function (muts) {
        var sawNodes = false;
        muts.forEach(function (mut) {
          if (mut.addedNodes.length) sawNodes = true;
          mut.addedNodes.forEach(scan);
        });
        /* Newly inserted radio groups need their selection state marked
           too (conditionalPanel content, re-rendered UI, etc.). */
        if (sawNodes) markSelection();
      }).observe(document.body, { childList: true, subtree: true });

      /* Initial pass: mark whatever is already checked on page load. */
      markSelection();
      document.addEventListener('shiny:connected', function () {
        setTimeout(function () { markSelection(); }, 0);
      });

    })();
  "))
)

server <- function(input, output, session) {

  # ---- Active colour theme, shared with every family module -----------
  # The browser reports the theme as a root-level input (see the
  # theme-toggle script at the end of the UI). Module sessions cannot read
  # root-level inputs directly -- `input$pw_theme` inside a module would
  # resolve to a namespaced id that doesn't exist -- so it's parked in
  # `session$userData`, which Shiny shares across all module sessions
  # while keeping it isolated per user session. The power curve reads it
  # via app_palette() so the figure matches the surrounding UI.
  session$userData$theme <- reactive({
    mode <- input$pw_theme
    if (is.null(mode) || !mode %in% c("light", "dark")) "light" else mode
  })

  # ---- Lazy family loading ---------------------------------------------
  # The first time a family is navigated to: insert its tab (UI) and
  # instantiate its module server, then remember it's done via
  # `initialized_families` so a later visit just switches tabs instead of
  # rebuilding anything (which would also have reset whatever the user had
  # already entered). See the comment above the tabsetPanel definition for
  # why this exists.
  initialized_families <- reactiveValues()

  navigate_to_family <- function(fam) {
    if (!isTRUE(initialized_families[[fam]])) {
      insertTab(
        inputId = "main_nav",
        tab = tabPanelBody(fam,
          family_page_header(fam, family_titles[[fam]]),
          family_ui_fns[[fam]](fam)
        ),
        target = "choose_analysis", position = "after", select = FALSE,
        session = session
      )
      family_server_fns[[fam]](fam)
      observeEvent(input[[NS(fam)("back_to_step1")]], {
        updateTabsetPanel(session, "main_nav", selected = "choose_analysis")
      }, ignoreInit = TRUE)

      # ---- Save / Share-link (see R/project_state.R for the pure
      # capture/serialize logic) -- registered once per family, here,
      # rather than inside that family's own moduleServer, since both
      # need `reactiveValuesToList(input)` at the TOP-LEVEL session to
      # see every namespaced input regardless of which family it belongs
      # to.
      #
      # Deliberately NOT downloadHandler()/downloadButton(): under the
      # shinylive/webR deployment the whole app runs inside an iframe
      # (the shinylive viewer's own architecture), and downloadHandler's
      # file is served through a "session/<token>/download/..." URL that
      # relies on a Service Worker to intercept it -- verified via CDP
      # against the live site that this download is silently canceled
      # (0 bytes) for every family, and the SAME failure reproduces for
      # the "Download HTML report" button (now fixed the same way, see
      # common_results.R), so it is not specific to this feature. Building
      # the file as a Blob entirely in the browser (see the shared
      # 'pw-download-blob-ready' handler below) needs no server-provided
      # download endpoint at all, so it sidesteps the problem completely
      # rather than depending on a fix to that mechanism.
      observeEvent(input[[NS(fam)("save_project")]], {
        st <- capture_family_state(reactiveValuesToList(input), fam)
        session$sendCustomMessage("pw-download-blob-ready", list(
          content = state_to_json(st),
          filename = sprintf("power_analysis_project_%s_%s.json", fam, Sys.Date()),
          mime = "application/json"
        ))
      }, ignoreInit = TRUE)

      # Share-link: also avoids updateQueryString(), which under the same
      # iframe architecture only rewrites the IFRAME's own (dynamically
      # generated, non-shareable) internal URL, not the address bar the
      # user actually sees and copies -- verified via CDP that opening a
      # link built that way 404s. The JS handler below targets
      # `window.parent` instead, which is same-origin and resolves to the
      # real top-level page (and is simply `window` itself, a no-op
      # self-reference, when NOT running inside that iframe -- e.g. a
      # plain `shiny::runApp()` server deployment -- so the same code
      # works correctly in both).
      observeEvent(input[[NS(fam)("share_link")]], {
        st <- capture_family_state(reactiveValuesToList(input), fam)
        q <- json_to_query(state_to_json(st))
        session$sendCustomMessage("pw-share-link-ready", list(btn_id = NS(fam)("share_link"), query = q))
      }, ignoreInit = TRUE)

      initialized_families[[fam]] <- TRUE
    }
    updateTabsetPanel(session, "main_nav", selected = fam)
  }

  observeEvent(input$start_wizard, {
    req(input$analysis_choice)
    navigate_to_family(input$analysis_choice)
  })

  # ---- Load a saved project (file upload) / restore from a shareable
  # link (URL query string) -- both funnel through the same
  # navigate-then-restore logic. Restoring inputs has to wait until AFTER
  # the target family's tab has actually been inserted (see
  # navigate_to_family() above); `session$onFlushed(once = TRUE)` defers
  # the restore until the CURRENT reactive flush (which includes that
  # insertTab() call) has been sent to the client, so the input elements
  # already exist by the time the update messages for them arrive.
  apply_saved_state <- function(st) {
    if (is.null(st) || is.null(st$family) || !st$family %in% names(family_ui_fns)) return(invisible(NULL))
    fam <- st$family
    navigate_to_family(fam)
    updateRadioButtons(session, "analysis_choice", selected = fam)
    session$onFlushed(function() {
      for (nm in names(st$inputs)) {
        full_id <- paste0(fam, "-", nm)
        val <- st$inputs[[nm]]
        if (identical(nm, "wiz")) {
          updateTabsetPanel(session, full_id, selected = val)
        } else {
          session$sendInputMessage(full_id, list(value = val))
        }
      }
    }, once = TRUE)
  }

  observeEvent(input$load_project_file, {
    req(input$load_project_file)
    txt <- tryCatch(paste(readLines(input$load_project_file$datapath, warn = FALSE), collapse = "\n"),
                     error = function(e) NULL)
    st <- tryCatch(json_to_state(txt), error = function(e) NULL)
    if (is.null(st)) {
      showNotification("Could not read that file -- is it a project JSON saved by this app?",
                        type = "error")
    } else {
      apply_saved_state(st)
    }
  })

  # Restoring from a shared link: `session$clientData$url_search` reflects
  # the IFRAME's own URL under the shinylive viewer (a fresh, dynamically
  # generated path with no query string at all -- verified via CDP that
  # the viewer does NOT forward the outer page's query string into it),
  # so it can never see a `?state=` the user's real address bar has. The
  # 'pw-initial-share-state' input is set from JS instead, which reads
  # `window.parent.location.search` directly (same-origin; see the script
  # at the end of the UI) -- this also just works, unchanged, for a plain
  # `shiny::runApp()` deployment with no iframe, since `window.parent` is
  # `window` itself in that case.
  observeEvent(input$pw_initial_share_state, {
    st <- tryCatch(json_to_state(query_to_json(input$pw_initial_share_state)), error = function(e) NULL)
    if (!is.null(st)) apply_saved_state(st)
  }, once = TRUE)

  # ---- Step-1 example library ------------------------------------------
  # An example is just a hard-coded instance of the same state object the
  # Save/Load and share-link features serialize, so loading one reuses
  # apply_saved_state() with no new plumbing (see R/example_library.R).
  output$example_cards <- renderUI({
    lib <- example_library()
    lapply(names(lib), function(key) {
      ex <- lib[[key]]
      div(class = "example-card",
        div(class = "example-card-body",
          strong(ex$title),
          p(class = "example-card-blurb", ex$blurb),
          span(class = "example-card-family",
               icon("arrow-turn-down"), " ", family_titles[[ex$family]])
        ),
        actionButton(paste0("example_load_", key),
                     tagList(icon("play"), " Load"),
                     class = "btn-outline-primary btn-sm example-card-btn")
      )
    })
  })

  # One observer per example. Registered once at startup (the library is a
  # fixed, code-defined list, so there is nothing dynamic to re-register)
  # and each is a plain hand-off to the shared state restorer.
  lapply(names(example_library()), function(key) {
    observeEvent(input[[paste0("example_load_", key)]], {
      ex <- example_library()[[key]]
      req(ex)
      apply_saved_state(ex$state)
    }, ignoreInit = TRUE)
  })

  # ---- Step-1 decision helper: maps plain-language answers to a family.
  # The unit-of-assignment question (individual vs. cluster) is asked
  # FIRST, before outcome type, because it changes which sub-questions
  # even make sense -- a cluster-randomized design in this app only has a
  # closed-form calculator for a continuous outcome (see clustered_rct).
  dh_unsupported <- reactive({
    identical(input$dh_unit, "cluster") &&
      identical(input$dh_outcome %||% "", "time_to_event")
  })

  recommended_family <- reactive({
    unit <- input$dh_unit %||% ""
    if (identical(unit, "cluster")) {
      switch(input$dh_outcome %||% "",
        continuous = "clustered_rct",
        binary = "clustered_cat",
        categorical = "clustered_cat",
        NULL)
    } else if (identical(unit, "individual")) {
      if (identical(input$dh_outcome, "continuous")) {
        switch(input$dh_continuous %||% "",
          two_groups = "two_means", paired = "paired_t", factorial = "anova_factorial",
          predictor = "regression", ancova = "ancova",
          nonparametric = "wilcoxon", repeated = "rm_anova", NULL)
      } else if (identical(input$dh_outcome, "binary")) {
        switch(input$dh_binary %||% "",
          two_groups = "proportions", paired = "mcnemar", predictor = "logistic", NULL)
      } else if (identical(input$dh_outcome, "categorical")) {
        "chisq"
      } else if (identical(input$dh_outcome, "time_to_event")) {
        "survival"
      } else {
        NULL
      }
    } else {
      NULL
    }
  })

  output$dh_recommendation <- renderUI({
    if (isTRUE(dh_unsupported())) {
      return(div(class = "well well-warning", icon("triangle-exclamation"),
        strong(" Not yet supported: "),
        "This wizard doesn't currently include a closed-form calculator for a clustered design with a time-to-event outcome -- combining the design effect with the log-rank test's events-based sample size is meaningfully more involved than the cases implemented here. Consider consulting a statistician, or looking at a dedicated package such as R's clusterPower for this specific scenario."))
    }
    fam <- recommended_family()
    if (is.null(fam)) {
      return(helpText("Answer the question(s) above to get a recommendation."))
    }
    label <- switch(fam,
      two_means = "Two independent means", anova_factorial = "Factorial ANOVA (between-subjects)",
      regression = "Multiple linear regression", logistic = "Logistic regression",
      proportions = "Two proportions", paired_t = "Paired / repeated measures",
      clustered_rct = "Clustered: sessions / matching groups",
      chisq = "Chi-square (goodness-of-fit / independence)",
      mcnemar = "McNemar's test",
      ancova = "ANCOVA (with a covariate)",
      survival = "Time-to-event (log-rank test)",
      wilcoxon = "Mann-Whitney (nonparametric)",
      rm_anova = "Repeated-measures ANOVA",
      clustered_cat = "Clustered: binary or categorical outcome")
    div(class = "well well-result",
      p(strong("Recommended analysis: "), label),
      actionButton("dh_use_recommendation", tagList(icon("check"), " Use this and start"), class = "btn-success")
    )
  })

  observeEvent(input$dh_use_recommendation, {
    fam <- recommended_family()
    req(fam)
    updateRadioButtons(session, "analysis_choice", selected = fam)
    navigate_to_family(fam)
  })
}

shinyApp(ui, server)
