## validation/eq2_audit.R
## -----------------------------------------------------------------------
## Audits the closed-form safeguard-markup relationship the manuscript
## calls Equation 2,
##
##     N_safeguard / N_SESOI  ~=  (1 - z_gamma / t)^-2 ,
##
## against the sample sizes this application's solvers actually return.
## It exists because the claim attached to that equation is quantitative
## ("agrees to within X%"), and a quantitative claim in a paper should be
## reproducible from the repository rather than asserted -- the same
## standard validation/monte_carlo_validation.R applies to the formulas
## themselves.
##
## WHAT IS BEING MEASURED, AND WHY IT IS NOT ONE NUMBER. Equation 2 has
## two steps. The first is an identity and is exact for every metric:
## t is DEFINED as delta_hat/SE(delta_hat), so the safeguard bound
## delta_hat - z_gamma*SE equals delta_hat*(1 - z_gamma/t) whatever the
## standard error happens to be. All the slippage lives in the second
## step, N proportional to 1/delta^2, and there the metrics genuinely
## differ:
##
##   h, w, log HR, p   the solver IS a constant over the squared effect,
##                     so the step is exact and the only residual is
##                     integer rounding;
##   r                 N = [(z_a+z_b)/z_r]^2 + 3 carries an additive term,
##                     whose relative weight falls as N grows;
##   d                 the application solves the exact noncentral t, not
##                     the normal approximation the proportionality
##                     assumes, and the gap between those two grows with
##                     the effect size.
##
## Reporting a single tolerance across all six therefore understates the
## accuracy of the first group and overstates it for the second. This
## script measures each separately.
##
## HOW ROUNDING IS ELIMINATED. Every required N is rounded up to a whole
## participant, which for a small N moves the ratio by more than the
## quantity being measured. Two independent routes are used so that no
## reported figure depends on rounding:
##   (1) solve at an extreme alpha/power so N lands in the 10^4-10^6
##       range, where a single participant is < 0.01%; and
##   (2) recompute on FRACTIONAL n via pwr, where rounding is exactly
##       zero. Route (2) is the one the reported per-metric figures come
##       from; route (1) confirms the same picture from the app's own
##       solvers end to end.
## Each printed line also carries its own rounding floor, so a reader can
## see directly that the residual exceeds it rather than taking it on
## trust.
##
## DIAGNOSTIC. The final block re-derives the d column under the pure
## normal approximation. If the gap really is the noncentral-t solve
## rather than a flaw in Equation 2's first step, that recomputation must
## return exactly zero error -- which is the difference between "the
## equation is approximate" and "the equation is right and the solver is
## more exact than the equation assumes".
##
## Usage (from the project root):
##   Rscript validation/eq2_audit.R
## Informational: it prints a table and always exits 0. It is an audit of
## a claim in the manuscript, not a property of the software, so there is
## no pass/fail threshold to gate on -- unlike scenario_validation.R,
## where a decision-helper mapping is simply right or wrong.
## -----------------------------------------------------------------------

source("global.R")
options(warn = -1)

GAMMA <- 0.80
Z_GAMMA <- stats::qnorm(GAMMA)

#' Equation 2's predicted markup at a given original-study t
eq2 <- function(t) (1 - Z_GAMMA / t)^(-2)

hr <- function(ch = "-") cat(strrep(ch, 78), "\n")
row_fmt <- function(...) cat(sprintf(...))

# ---- Part 1: fractional n (zero rounding), at the app's own defaults ----
# pwr returns a fractional n, so these figures contain no rounding at all.
# This is the route the manuscript's per-metric numbers come from.

cat("\n")
hr("=")
cat("PART 1  Deviation from Equation 2 on FRACTIONAL n (rounding exactly zero)\n")
cat("        evaluated at the application's defaults, alpha = .05, power = .80\n")
hr("=")

cat("\n-- d: two independent means (interval on the d scale, Hedges-Olkin SE) --\n")
row_fmt("  %-7s %-8s %12s %12s %10s\n", "d", "t", "predicted", "actual", "error")
d_err <- c()
for (d in c(0.20, 0.30, 0.40, 0.60, 0.80, 1.00, 1.345, 2.00, 3.07)) {
  sg <- safeguard_ci_d(d, 50, 50, conf_level = GAMMA)
  t <- d / sg$se
  if (t <= Z_GAMMA) next            # bound collapses; N diverges by design
  a <- pwr::pwr.t.test(d = d, sig.level = 0.05, power = 0.80, type = "two.sample")$n
  b <- pwr::pwr.t.test(d = sg$d_safeguard, sig.level = 0.05, power = 0.80,
                       type = "two.sample")$n
  e <- 100 * ((b / a) / eq2(t) - 1)
  d_err <- c(d_err, e)
  row_fmt("  %-7.3f %-8.2f %12.5f %12.5f %+9.3f%%\n", d, t, eq2(t), b / a, e)
}

cat("\n-- r: correlation (interval on the Fisher-z scale) --\n")
row_fmt("  %-7s %-8s %12s %12s %10s\n", "r", "t", "predicted", "actual", "error")
r_err <- c()
for (cfg in list(c(0.05, 400), c(0.10, 200), c(0.20, 100), c(0.30, 67),
                 c(0.40, 50), c(0.50, 40))) {
  r <- cfg[1]; n0 <- cfg[2]
  sg <- safeguard_ci_r(r, n0, conf_level = GAMMA)
  t <- atanh(r) / sg$se_z
  if (t <= Z_GAMMA) next
  a <- pwr::pwr.r.test(r = r, sig.level = 0.05, power = 0.80)$n
  b <- pwr::pwr.r.test(r = sg$r_safeguard, sig.level = 0.05, power = 0.80)$n
  e <- 100 * ((b / a) / eq2(t) - 1)
  r_err <- c(r_err, e)
  row_fmt("  %-7.2f %-8.2f %12.5f %12.5f %+9.3f%%   (n_orig = %d)\n",
          r, t, eq2(t), b / a, e, n0)
}

# ---- Part 2: the app's own solvers, at N large enough to ignore rounding --
# Same question asked a second way, end to end through the shipped solvers
# rather than through pwr, and covering the metrics pwr has no function for.

AL <- 1e-6      # extreme, to push N into the 10^4-10^6 range
PW <- 0.999

audit <- function(label, cases, solve_pair) {
  cat(sprintf("\n-- %s --\n", label))
  row_fmt("  %-30s %-8s %10s %12s %14s\n",
          "configuration", "t", "error", "N_sesoi", "rounding floor")
  errs <- c()
  for (cs in cases) {
    o <- tryCatch(solve_pair(cs), error = function(e) NULL)
    if (is.null(o) || o$t <= Z_GAMMA) next
    e <- 100 * ((o$b / o$a) / eq2(o$t) - 1)
    errs <- c(errs, e)
    row_fmt("  %-30s %-8.3f %+9.4f%% %12s %13.4f%%\n",
            o$cfg, o$t, e, format(o$a, big.mark = ","), 100 / o$a)
  }
  invisible(max(abs(errs)))
}

cat("\n")
hr("=")
cat("PART 2  Same audit through the shipped solvers, at alpha = 1e-6 / power = .999\n")
cat("        so that N is large and the rounding floor (last column) is negligible\n")
hr("=")

worst <- list()

worst$h <- audit("h: two proportions  [exact second step]",
  list(c(0.50, 120), c(0.30, 200), c(0.20, 300), c(0.10, 1200), c(0.05, 4000)),
  function(g) {
    h <- g[1]; n <- g[2]
    sg <- safeguard_ci_h(h, n / 2, n / 2, conf_level = GAMMA)
    list(cfg = sprintf("h=%.2f, n_orig=%d", h, n), t = h / sg$se,
         a = power_proportions_n(h, AL, PW)$n_total,
         b = power_proportions_n(sg$h_safeguard, AL, PW)$n_total)
  })

worst$w <- audit("w: chi-square  [exact second step]",
  list(c(0.30, 200, 3), c(0.20, 500, 3), c(0.10, 2000, 3),
       c(0.10, 2000, 6), c(0.05, 8000, 10)),
  function(g) {
    w <- g[1]; n <- g[2]; df <- g[3]
    sg <- safeguard_ci_w(w, n, df = df, conf_level = GAMMA)
    list(cfg = sprintf("w=%.2f, n_orig=%d, df=%d", w, n, df), t = w / sg$se,
         a = power_chisq_n(w, df = df, sig_level = AL, power = PW)$n_total,
         b = power_chisq_n(sg$w_safeguard, df = df, sig_level = AL, power = PW)$n_total)
  })

worst$hr <- audit("log HR: time-to-event  [exact second step, on the log scale]",
  list(c(0.50, 80), c(0.65, 150), c(0.80, 300), c(0.90, 1200), c(1.50, 120)),
  function(g) {
    h <- g[1]; D <- g[2]
    sg <- safeguard_ci_logHR(h, events_published = D, conf_level = GAMMA)
    list(cfg = sprintf("HR=%.2f, events=%d", h, D),
         t = abs(log(h)) / sg$se_log_hr,
         a = power_survival_n(h, p_event = 0.8, sig_level = AL, power = PW)$n_total,
         b = power_survival_n(sg$hr_safeguard, p_event = 0.8,
                              sig_level = AL, power = PW)$n_total)
  })

worst$p <- audit("p = P(X<Y): Wilcoxon-Mann-Whitney  [exact second step]",
  list(c(0.75, 30), c(0.70, 40), c(0.64, 80), c(0.60, 150),
       c(0.56, 300), c(0.54, 900)),
  function(g) {
    pv <- g[1]; n <- g[2]
    sg <- safeguard_ci_auc(pv, n / 2, n / 2, conf_level = GAMMA)
    list(cfg = sprintf("p=%.2f, n_orig=%d", pv, n), t = (pv - 0.5) / sg$se,
         a = power_wilcoxon_n(pv, AL, PW)$n_total,
         b = power_wilcoxon_n(sg$p_safeguard, AL, PW)$n_total)
  })

# ---- Part 3: is the d gap really the noncentral-t solve? -----------------
# If it is, redoing the SAME comparison under the pure normal approximation
# (where N is a constant over d^2 by construction) must return exactly zero.

cat("\n")
hr("=")
cat("PART 3  Diagnostic: recomputing the d column under the pure normal\n")
cat("        approximation. Exact zeros confirm the gap is the noncentral-t\n")
cat("        solve, not a defect in Equation 2 itself.\n")
hr("=")
cat("\n")
n_normal <- function(d, al, pw) {
  4 * (stats::qnorm(1 - al / 2) + stats::qnorm(pw))^2 / d^2   # total, balanced
}
row_fmt("  %-8s %16s %18s\n", "d", "error (app)", "error (normal approx)")
for (d in c(0.40, 0.80, 1.345, 3.07)) {
  sg <- safeguard_ci_d(d, 50, 50, conf_level = GAMMA)
  t <- d / sg$se
  a1 <- power_two_means_n(d, 1e-4, 0.999)$n_total
  b1 <- power_two_means_n(sg$d_safeguard, 1e-4, 0.999)$n_total
  a2 <- n_normal(d, 1e-4, 0.999)
  b2 <- n_normal(sg$d_safeguard, 1e-4, 0.999)
  row_fmt("  %-8.3f %15.3f%% %17.4f%%\n", d,
          100 * ((b1 / a1) / eq2(t) - 1), 100 * ((b2 / a2) / eq2(t) - 1))
}

# ---- Summary -------------------------------------------------------------

cat("\n")
hr("=")
cat("SUMMARY\n")
hr("=")
cat(sprintf("\n  Metrics whose second step is exact (h, w, log HR, p):\n"))
for (k in c("h", "w", "hr", "p")) {
  cat(sprintf("      %-8s max |deviation| = %.4f%%\n", k, worst[[k]]))
}
# Rounded UP (never to nearest): "within X%" is a bound, and round-to-nearest
# can silently understate one -- 0.0929% prints as "0.09%" under %.2f, which
# reads as a claim the true 0.0929% figure actually violates.
worst_exact <- max(unlist(worst[c("h", "w", "hr", "p")]))
cat(sprintf("      -> all within %.3f%%\n", ceiling(worst_exact * 1000) / 1000))
cat(sprintf("\n  Metrics whose second step is approximate, on fractional n:\n"))
cat(sprintf("      %-8s %.3f%% at d = 0.20 rising to %.3f%% at d = 3.07\n",
            "d", abs(d_err[1]), abs(d_err[length(d_err)])))
cat(sprintf("      %-8s %.3f%% at r = 0.05 rising to %.3f%% at r = 0.50\n",
            "r", abs(r_err[1]), abs(r_err[length(r_err)])))
cat("\n  These two are systematic, not rounding: see the rounding-floor column\n")
cat("  in Part 2 and the exact zeros in Part 3.\n\n")
