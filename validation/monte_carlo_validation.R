## validation/monte_carlo_validation.R
## -----------------------------------------------------------------------
## Reproducible Monte Carlo validation for the closed-form power formulas
## that have no existing reference implementation to check against
## directly (McNemar's test, TOST, Wilcoxon-Mann-Whitney, the time-to-event
## log-rank family) or that benefit from an independent simulation-based
## check (repeated-measures ANOVA), plus a direct formula-vs-formula
## cross-check of factorial ANOVA against the `pwr` package for the
## one-way case. See VALIDATION.md for the full picture across all sixteen
## families, including the nine validated purely against `pwr`/published
## tables (no simulation needed there).
##
## This is a companion to, not a replacement for, the pinned regression
## tests in tests/testthat/ -- those check exact reference values (fast,
## deterministic, run in CI on every push); this script re-derives those
## claims from scratch with fresh seeded simulations, on demand, as public
## evidence of methodology rather than as part of the automated suite
## (the reps below take a few minutes total; testthat needs to stay fast).
##
## Usage (from the project root):
##   Rscript validation/monte_carlo_validation.R
##
## Every simulation below is seeded (set.seed(20260804), reset per family
## so each is independently reproducible) and reports the formula's power,
## the simulated rejection rate, and their difference against a rough
## Monte Carlo margin (2 * SE, SE = sqrt(p(1-p)/reps)) for the LARGER of
## the two power estimates. A difference outside that margin is flagged
## "WATCH" rather than "FAIL": for McNemar and the log-rank family in
## particular, the closed-form relation is a documented large-sample /
## proportional-hazards APPROXIMATION (see R/power_mcnemar.R and
## R/power_survival.R), so some configurations -- small n, or (for
## log-rank) a strong hazard ratio combined with unbalanced allocation --
## are EXPECTED to disagree by more than pure sampling noise would predict.
## That is the formula's known behavior, not a bug in this script.
## -----------------------------------------------------------------------

if (!file.exists("app.R")) {
  stop("Run this from the project root (the folder containing app.R), e.g.:\n",
       "  Rscript validation/monte_carlo_validation.R")
}
source("global.R")

SEED <- 20260804

.report <- function(family, cfg_txt, formula_p, sim_p, reps) {
  se <- sqrt(max(formula_p, sim_p) * (1 - max(formula_p, sim_p)) / reps)
  diff <- formula_p - sim_p
  flag <- if (abs(diff) <= 2 * se) "OK   " else "WATCH"
  cat(sprintf("  [%s] %-28s %-38s formula=%.4f sim=%.4f diff=%+.4f (2*SE=%.4f, reps=%d)\n",
              flag, family, cfg_txt, formula_p, sim_p, diff, 2 * se, reps))
}

cat("A Priori Power Analysis Wizard -- Monte Carlo validation\n")
cat("App version:", APP_VERSION, "| R version:", R.version.string, "\n\n")

## ---- McNemar's test ----------------------------------------------------
## Simulates discordant-pair counts directly from a 4-category multinomial
## (11/10/01/00) and runs stats::mcnemar.test() (uncorrected, matching the
## formula's derivation) on the resulting 2x2 table.
cat("McNemar's test (Connor, 1987)\n")
set.seed(SEED)
sim_mcnemar <- function(n, p10, p01, sig_level = 0.05, reps = 8000) {
  p11 <- p00 <- (1 - p10 - p01) / 2
  probs <- c(p11, p10, p01, p00)
  rej <- 0L
  for (i in seq_len(reps)) {
    draws <- sample.int(4, size = n, replace = TRUE, prob = probs)
    x10 <- sum(draws == 2L); x01 <- sum(draws == 3L)
    tab <- matrix(c(0, x01, x10, 0), 2, 2)
    p <- tryCatch(mcnemar.test(tab, correct = FALSE)$p.value, error = function(e) NA_real_)
    if (!is.na(p) && p < sig_level) rej <- rej + 1L
  }
  rej / reps
}
for (cfg in list(list(n = 100, p10 = 0.25, p01 = 0.10),
                  list(n = 200, p10 = 0.30, p01 = 0.15),
                  list(n = 60,  p10 = 0.35, p01 = 0.10))) {
  f <- power_mcnemar_at_n(cfg$n, cfg$p10, cfg$p01)
  s <- sim_mcnemar(cfg$n, cfg$p10, cfg$p01)
  .report("McNemar", sprintf("n=%d p10=%.2f p01=%.2f", cfg$n, cfg$p10, cfg$p01), f, s, 8000)
}
cat("\n")

## ---- TOST equivalence test ----------------------------------------------
## Simulates two independent normal samples and applies the two one-sided
## tests directly (sample SE, not the population formula), matching how a
## researcher would actually run TOST on real data.
cat("TOST equivalence test (Schuirmann, 1987; Phillips, 1990)\n")
set.seed(SEED)
sim_tost <- function(n, delta_eq, theta = 0, sig_level = 0.05, reps = 8000) {
  df <- 2 * n - 2
  tcrit <- qt(1 - sig_level, df)
  rej <- 0L
  for (i in seq_len(reps)) {
    g1 <- rnorm(n, 0, 1); g2 <- rnorm(n, theta, 1)
    diff <- mean(g2) - mean(g1)
    sp <- sqrt(((n - 1) * var(g1) + (n - 1) * var(g2)) / df)
    se <- sp * sqrt(2 / n)
    t1 <- (diff + delta_eq) / se; t2 <- (diff - delta_eq) / se
    if (t1 > tcrit && t2 < -tcrit) rej <- rej + 1L
  }
  rej / reps
}
for (cfg in list(list(n = 60, delta_eq = 0.5, theta = 0),
                  list(n = 150, delta_eq = 0.3, theta = 0),
                  list(n = 80, delta_eq = 0.4, theta = 0.1))) {
  f <- power_tost_at_n(cfg$n, cfg$delta_eq, cfg$theta)
  s <- sim_tost(cfg$n, cfg$delta_eq, cfg$theta)
  .report("TOST", sprintf("n=%d delta=%.2f theta=%.2f", cfg$n, cfg$delta_eq, cfg$theta), f, s, 8000)
}
cat("\n")

## ---- Wilcoxon-Mann-Whitney -----------------------------------------------
## Normal-parent case: shifts the mean so P(X<Y) = p_sup exactly
## (p_sup = pnorm(mu / sqrt(2))), then runs stats::wilcox.test().
cat("Wilcoxon-Mann-Whitney (Noether, 1987)\n")
set.seed(SEED)
sim_wilcoxon <- function(n1, p_sup, sig_level = 0.05, reps = 6000) {
  mu <- qnorm(p_sup) * sqrt(2)
  rej <- 0L
  for (i in seq_len(reps)) {
    x <- rnorm(n1); y <- rnorm(n1) + mu
    p <- suppressWarnings(wilcox.test(y, x)$p.value)
    if (p < sig_level) rej <- rej + 1L
  }
  rej / reps
}
for (cfg in list(list(n1 = 40, p_sup = 0.65),
                  list(n1 = 60, p_sup = 0.62),
                  list(n1 = 30, p_sup = 0.70))) {
  f <- power_wilcoxon_at_n(cfg$n1, cfg$p_sup)
  s <- sim_wilcoxon(cfg$n1, cfg$p_sup)
  .report("Wilcoxon-Mann-Whitney", sprintf("n1=%d p_sup=%.2f", cfg$n1, cfg$p_sup), f, s, 6000)
}
cat("\n")

## ---- Wilcoxon-Mann-Whitney: robustness to the PARENT DISTRIBUTION -------
## The point of parameterizing this family on p = P(X<Y) rather than on
## Cohen's d is that p is what a rank test is actually sensitive to, and
## needs no normality assumption to be well defined. This block checks that
## claim the only way that is not circular: hold p_sup FIXED and vary the
## shape of the parent distribution. Holding d fixed instead would be
## near-tautological -- heavier tails at fixed d simply imply a different p,
## so any power difference would just be that reparameterization showing up.
##
## For each parent the location shift is solved numerically so that
## P(X < Y) equals the target EXACTLY: with a symmetric parent,
##   P(X < Y) = int f(x) F(x + delta) dx,
## which is inverted for delta by uniroot(). All three parents are scaled to
## unit variance (irrelevant to a rank test, but it keeps the shifts
## comparable). Laplace and logistic are the standard heavier-tailed
## reference shapes; the outcomes that motivate reaching for a rank test in
## the first place (contributions, bids, earnings) are heavier-tailed than
## normal in exactly this direction.
cat("Wilcoxon-Mann-Whitney: parent-shape robustness at fixed p_sup\n")
set.seed(SEED)

wmw_parents <- list(
  normal   = list(r = function(n) rnorm(n),
                  d = function(x) dnorm(x),
                  p = function(x) pnorm(x)),
  logistic = list(r = function(n) rlogis(n, scale = sqrt(3) / pi),
                  d = function(x) dlogis(x, scale = sqrt(3) / pi),
                  p = function(x) plogis(x, scale = sqrt(3) / pi)),
  laplace  = list(r = function(n) { b <- 1 / sqrt(2); u <- runif(n) - 0.5
                                    -b * sign(u) * log(1 - 2 * abs(u)) },
                  d = function(x) { b <- 1 / sqrt(2); exp(-abs(x) / b) / (2 * b) },
                  p = function(x) { b <- 1 / sqrt(2)
                                    ifelse(x < 0, 0.5 * exp(x / b),
                                           1 - 0.5 * exp(-x / b)) })
)

# location shift giving exactly the requested P(X < Y) under this parent
wmw_delta_for <- function(par, target) {
  p_sup_of <- function(delta)
    stats::integrate(function(x) par$d(x) * par$p(x + delta), -Inf, Inf)$value
  stats::uniroot(function(dl) p_sup_of(dl) - target, c(0, 6))$root
}

sim_wilcoxon_parent <- function(par, n1, delta, sig_level = 0.05, reps = 6000) {
  rej <- 0L
  for (i in seq_len(reps)) {
    x <- par$r(n1); y <- par$r(n1) + delta
    if (suppressWarnings(wilcox.test(y, x)$p.value) < sig_level) rej <- rej + 1L
  }
  rej / reps
}

for (cfg in list(list(n1 = 40, p_sup = 0.65), list(n1 = 40, p_sup = 0.70))) {
  f <- power_wilcoxon_at_n(cfg$n1, cfg$p_sup)
  for (nm in names(wmw_parents)) {
    par <- wmw_parents[[nm]]
    dl <- wmw_delta_for(par, cfg$p_sup)
    s <- sim_wilcoxon_parent(par, cfg$n1, dl)
    .report("Wilcoxon-MW parent",
            sprintf("%s n1=%d p_sup=%.2f (shift=%.3f)", nm, cfg$n1, cfg$p_sup, dl),
            f, s, 6000)
  }
}
cat("\n")

## ---- Repeated-measures ANOVA (within main effect, compound symmetry) ---
## Condition means are spaced so their POPULATION sd equals f_target
## exactly; participant and residual variance are split so that
## Var(participant) + Var(residual) = 1 and their ratio gives the target
## rho -- i.e. Cohen's f here, like d_z for the paired t-test, is defined
## relative to the TOTAL outcome variance, not the within-subject residual
## alone, which is exactly what the formula's 1/(1-rho) term corrects for.
## Manual sum-of-squares F-test (not aov()'s Error() parsing) so every step
## is auditable.
cat("Repeated-measures ANOVA, within main effect (Cohen, 1988, ch. 8; G*Power 3 formulation)\n")
set.seed(SEED)
sim_rm_anova_within <- function(n, m, f_target, rho, sig_level = 0.05, reps = 4000) {
  raw <- seq_len(m) - mean(seq_len(m))
  means <- raw / sqrt(sum(raw^2) / m) * f_target
  sigma_p <- sqrt(rho); sigma_e <- sqrt(1 - rho)
  df_cond <- m - 1; df_err <- (n - 1) * (m - 1)
  rej <- 0L
  for (i in seq_len(reps)) {
    p_eff <- rnorm(n, 0, sigma_p)
    err <- matrix(rnorm(n * m, 0, sigma_e), n, m)
    y <- matrix(means, n, m, byrow = TRUE) + p_eff + err
    grand <- mean(y); cond_means <- colMeans(y); subj_means <- rowMeans(y)
    ss_cond <- n * sum((cond_means - grand)^2)
    ss_subj <- m * sum((subj_means - grand)^2)
    ss_err <- sum((y - grand)^2) - ss_cond - ss_subj
    Fstat <- (ss_cond / df_cond) / (ss_err / df_err)
    if (pf(Fstat, df_cond, df_err, lower.tail = FALSE) < sig_level) rej <- rej + 1L
  }
  rej / reps
}
for (cfg in list(list(n = 20, m = 3, f = 0.25, rho = 0.5),
                  list(n = 15, m = 4, f = 0.25, rho = 0.6),
                  list(n = 25, m = 3, f = 0.30, rho = 0.3))) {
  fpow <- power_rm_anova_at_n(cfg$n, cfg$m, cfg$f, cfg$rho, effect = "within")
  s <- sim_rm_anova_within(cfg$n, cfg$m, cfg$f, cfg$rho)
  .report("RM-ANOVA (within)", sprintf("n=%d m=%d f=%.2f rho=%.2f", cfg$n, cfg$m, cfg$f, cfg$rho),
          fpow, s, 4000)
}
cat("\n")

## ---- Time-to-event (log-rank test) --------------------------------------
## Exponential survival times per arm (hazard ratio HR relative to arm 1),
## fixed-time administrative censoring solved numerically so the blended
## event probability matches p_event, survival::survdiff() for the test.
## NOTE: the `survival` package is used here only for this offline
## validation script -- it is NOT a runtime dependency of the app itself
## (see README, "Browser-only deployment").
cat("Time-to-event log-rank test (Schoenfeld, 1983)\n")
if (!requireNamespace("survival", quietly = TRUE)) {
  cat("  (skipped: the 'survival' package is not installed -- install.packages(\"survival\") to run this section)\n\n")
} else {
  library(survival)
  set.seed(SEED)
  sim_survival <- function(n_total, hr, p_event, alloc_ratio = 1, sig_level = 0.05, reps = 3000) {
    k <- alloc_ratio
    w1 <- 1 / (1 + k); w2 <- k / (1 + k)
    n1 <- round(n_total * w1); n2 <- n_total - n1
    lambda1 <- 1; lambda2 <- hr * lambda1
    Tcens <- uniroot(function(T) w1 * (1 - exp(-lambda1 * T)) + w2 * (1 - exp(-lambda2 * T)) - p_event,
                      c(1e-6, 1000))$root
    rej <- 0L
    for (i in seq_len(reps)) {
      t1 <- rexp(n1, lambda1); t2 <- rexp(n2, lambda2)
      time <- c(pmin(t1, Tcens), pmin(t2, Tcens))
      event <- c(as.integer(t1 <= Tcens), as.integer(t2 <= Tcens))
      arm <- c(rep(1, n1), rep(2, n2))
      sd_fit <- survival::survdiff(survival::Surv(time, event) ~ arm)
      if (1 - pchisq(sd_fit$chisq, df = 1) < sig_level) rej <- rej + 1L
    }
    rej / reps
  }
  for (cfg in list(list(n = 300, hr = 0.6, p_event = 0.5, k = 1),
                    list(n = 200, hr = 1.8, p_event = 0.6, k = 1),
                    list(n = 250, hr = 0.7, p_event = 0.4, k = 1.5))) {
    fpow <- power_survival_at_n(cfg$n, cfg$hr, cfg$p_event, alloc_ratio = cfg$k)
    s <- sim_survival(cfg$n, cfg$hr, cfg$p_event, alloc_ratio = cfg$k)
    .report("Log-rank", sprintf("n=%d hr=%.2f p_event=%.2f k=%.1f", cfg$n, cfg$hr, cfg$p_event, cfg$k),
            fpow, s, 3000)
  }
  cat("\n")
}

## ---- Factorial ANOVA: formula-vs-formula cross-check (one-way case) ----
## No simulation needed here: the one-way case is exactly what
## pwr::pwr.anova.test() computes, so this is a direct, exact comparison
## (this app's own factorial-ANOVA family is implemented in base R, not
## via pwr, precisely so it also covers two-way designs and specific
## focal contrasts that pwr::pwr.anova.test() cannot -- see
## R/power_anova_factorial.R).
cat("Factorial ANOVA, one-way case, vs. pwr::pwr.anova.test() (Cohen, 1988, ch. 8)\n")
for (cfg in list(list(k = 3, n = 20, f = 0.25),
                  list(k = 4, n = 15, f = 0.40),
                  list(k = 5, n = 10, f = 0.30))) {
  ours <- power_anova_factorial_at_n(cfg$n, levels_a = cfg$k, focal = "A", f_target = cfg$f)
  ref <- pwr::pwr.anova.test(k = cfg$k, n = cfg$n, f = cfg$f, sig.level = 0.05)$power
  cat(sprintf("  [%s] %-28s %-38s ours=%.6f pwr=%.6f diff=%.2e\n",
              if (abs(ours - ref) < 1e-6) "OK   " else "WATCH",
              "Factorial ANOVA (1-way)", sprintf("k=%d n=%d f=%.2f", cfg$k, cfg$n, cfg$f),
              ours, ref, ours - ref))
}
cat("\nDone. See VALIDATION.md for how this fits into the full sixteen-family picture.\n")
