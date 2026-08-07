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
    # Closed-form starting point rather than a linear scan from n1 = 2.
    # The arcsine transform is variance-stabilizing, so with n2 = k*n1 the
    # standard error of the difference is sqrt(1/n1 + 1/n2) =
    # sqrt((1 + 1/k)/n1); setting h/SE equal to the required z gives
    #   n1 = (z_alpha + z_beta)^2 * (1 + 1/k) / h^2.
    # The previous implementation walked `for (cand in 2:100000)` calling
    # pwr::pwr.2p2n.test() once per step. That was correct where it
    # terminated but had three defects: it could run to a hundred thousand
    # pwr calls (a frozen tab under webR, where the whole R runtime is in
    # the browser); `n1 * n2` overflowed integer range on the way; and for
    # any allocation ratio below about 0.6 the very first candidate made
    # n2 = 1, which pwr rejects outright -- so an entirely ordinary design
    # ("my control arm is twice my treatment arm", ratio = 0.5) failed with
    # an opaque error from inside pwr. The UI permits ratios in [0.1, 10].
    za <- if (identical(alternative, "two.sided")) {
      stats::qnorm(1 - sig_level / 2)
    } else {
      stats::qnorm(1 - sig_level)
    }
    n1_exact <- (za + stats::qnorm(power))^2 * (1 + 1 / allocation_ratio) / h^2
    assert_solvable_n(n1_exact, "effect size h")

    # Both arms need at least 2 observations before pwr will evaluate the
    # design at all, which for a ratio below 1 binds on n2, not on n1.
    n1 <- max(round_up_n(n1_exact), 2L, round_up_n(2 / allocation_ratio))

    # n1/n2 are passed as doubles on purpose: pwr computes n1 * n2
    # internally in whatever type it receives, and with integer arguments
    # that product silently overflows past ~2.1e9 and returns NA power,
    # which then propagates into the comparison below as "missing value
    # where TRUE/FALSE needed" -- an opaque failure in place of the clean
    # one assert_solvable_n() is there to give.
    achieved_at <- function(cand) {
      n2_cand <- max(round_up_n(cand * allocation_ratio), 2L)
      pwr::pwr.2p2n.test(h = h,
                          n1 = as.double(cand), n2 = as.double(n2_cand),
                          sig.level = sig_level,
                          alternative = alternative)$power
    }
    # Refine in whichever direction the closed form missed by, so the
    # result is still the SMALLEST n1 meeting the target -- the property
    # the old scan had, preserved without the scan.
    not_reached <- function(cand) {
      pw <- achieved_at(cand)
      !is.finite(pw) || pw < power
    }
    while (not_reached(n1)) {
      n1 <- n1 + 1L
      assert_solvable_n(n1, "effect size h")
    }
    min_n1 <- max(2L, round_up_n(2 / allocation_ratio))
    while (n1 > min_n1 && achieved_at(n1 - 1L) >= power) n1 <- n1 - 1L

    n2 <- max(round_up_n(n1 * allocation_ratio), 2L)
  }

  # Recompute achieved power at the rounded (integer) n1/n2 -- same pattern
  # used by every other family. In the balanced branch pwr.2p.test() was
  # asked to SOLVE for n, so its $power is just the target handed back
  # unchanged (h = 0.5 gives 0.8013 at n = 63, not 0.8000); the unbalanced
  # branch already refined against integer arms, and recomputing here costs
  # one extra pwr call and keeps a single definition of the reported number.
  power_achieved <- power_proportions_at_n(n1 = n1, n2 = n2, h = h,
                                            sig_level = sig_level,
                                            alternative = alternative)

  list(
    n1 = n1, n2 = n2, n_total = n1 + n2,
    h = h, sig_level = sig_level, power_target = power,
    power_achieved = power_achieved,
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
    pwr::pwr.2p2n.test(h = h, n1 = as.double(n1), n2 = as.double(n2),
                        sig.level = sig_level,
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
    fit <- pwr::pwr.2p2n.test(n1 = as.double(n1), n2 = as.double(n2),
                               sig.level = sig_level,
                               power = power, alternative = alternative)
  }
  fit$h
}
