## power_proportions.R
## -----------------------------------------------------------------------
## Analysis family: comparison of two independent proportions.
## Wraps pwr::pwr.2p.test() (balanced) / pwr::pwr.2p2n.test() (unbalanced).
## Effect size is Cohen's h = 2*asin(sqrt(p1)) - 2*asin(sqrt(p2)).
## -----------------------------------------------------------------------

#' Cohen's h from two proportions
#' @export
proportions_to_h <- function(p1, p2) {
  stopifnot(p1 > 0, p1 < 1, p2 > 0, p2 < 1)
  pwr::ES.h(p1, p2)
}

#' Sample size for a two-proportions comparison
#'
#' @param h numeric, Cohen's h (use [proportions_to_h()] if you have raw
#'   proportions instead)
#' @param sig_level,power,alternative as in other families
#' @param allocation_ratio numeric, n2/n1
#' @export
power_proportions_n <- function(h, sig_level = 0.05, power = 0.80,
                                 alternative = c("two.sided", "greater", "less"),
                                 allocation_ratio = 1) {
  alternative <- match.arg(alternative)
  stopifnot(sig_level > 0, sig_level < 1, power > 0, power < 1)
  h <- abs(h)

  if (isTRUE(all.equal(allocation_ratio, 1))) {
    fit <- pwr::pwr.2p.test(h = h, sig.level = sig_level, power = power,
                             alternative = alternative)
    n1 <- round_up_n(fit$n)
    n2 <- n1
  } else {
    n1 <- NA_integer_
    for (cand in 2:100000) {
      n2_cand <- round_up_n(cand * allocation_ratio)
      p <- pwr::pwr.2p2n.test(h = h, n1 = cand, n2 = n2_cand,
                               sig.level = sig_level,
                               alternative = alternative)$power
      if (p >= power) { n1 <- cand; break }
    }
    if (is.na(n1)) stop("No feasible N found below 100000 per group; check inputs.")
    n2 <- round_up_n(n1 * allocation_ratio)
    fit <- pwr::pwr.2p2n.test(h = h, n1 = n1, n2 = n2, sig.level = sig_level,
                               alternative = alternative)
  }

  list(
    n1 = n1, n2 = n2, n_total = n1 + n2,
    h = h, sig_level = sig_level, power_target = power,
    power_achieved = fit$power,
    alternative = alternative, allocation_ratio = allocation_ratio,
    method = "Two independent proportions, pwr::pwr.2p.test/pwr.2p2n.test"
  )
}

#' @export
power_proportions_at_n <- function(n1, n2 = n1, h, sig_level = 0.05,
                                    alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  if (n1 == n2) {
    pwr::pwr.2p.test(h = h, n = n1, sig.level = sig_level,
                      alternative = alternative)$power
  } else {
    pwr::pwr.2p2n.test(h = h, n1 = n1, n2 = n2, sig.level = sig_level,
                        alternative = alternative)$power
  }
}

#' @export
power_proportions_min_h <- function(n1, n2 = n1, sig_level = 0.05, power = 0.80,
                                     alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  if (n1 == n2) {
    fit <- pwr::pwr.2p.test(n = n1, sig.level = sig_level, power = power,
                             alternative = alternative)
  } else {
    fit <- pwr::pwr.2p2n.test(n1 = n1, n2 = n2, sig.level = sig_level,
                               power = power, alternative = alternative)
  }
  fit$h
}
