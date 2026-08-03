## power_correlation.R
## -----------------------------------------------------------------------
## Analysis family: test of a single bivariate Pearson correlation against
## zero. Wraps pwr::pwr.r.test(). Effect size is the correlation
## coefficient r itself (Cohen, 1988, ch. 3).
## -----------------------------------------------------------------------

#' Sample size for a bivariate correlation test
#'
#' @param r numeric, the correlation coefficient to detect (-1 < r < 1,
#'   r != 0)
#' @param sig_level,power as in other families
#' @param alternative "two.sided", "greater", or "less"
#' @export
power_correlation_n <- function(r, sig_level = 0.05, power = 0.80,
                                 alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  stopifnot(sig_level > 0, sig_level < 1, power > 0, power < 1)
  r <- abs(r)
  stopifnot(r > 0, r < 1)

  fit <- pwr::pwr.r.test(r = r, sig.level = sig_level, power = power,
                          alternative = alternative)
  n_total <- round_up_n(fit$n)
  power_achieved <- power_correlation_at_n(n_total, r = r, sig_level = sig_level,
                                            alternative = alternative)

  list(
    n_total = n_total, r = r,
    sig_level = sig_level, power_target = power, power_achieved = power_achieved,
    alternative = alternative,
    method = "Bivariate correlation, pwr::pwr.r.test"
  )
}

#' @export
power_correlation_at_n <- function(n_total, r, sig_level = 0.05,
                                    alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  pwr::pwr.r.test(n = n_total, r = abs(r), sig.level = sig_level,
                   alternative = alternative)$power
}

#' @export
power_correlation_min_r <- function(n_total, sig_level = 0.05, power = 0.80,
                                     alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  pwr::pwr.r.test(n = n_total, sig.level = sig_level, power = power,
                   alternative = alternative)$r
}
