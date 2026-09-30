#' Probability cutoffs of the BOP2 and BOP2-FE designs
#'
#' Internal helper that evaluates the futility cutoff
#' \code{C_F(t) = lambda t^gamma} of Zhou et al. (2017) and the efficacy cutoff
#' of Xu et al. (2026), either the power function
#' \code{C_E(t) = 1 - (1 - lambda) t^eta} or the O'Brien-Fleming type function
#' \code{C_E(t) = 2 Phi(z / sqrt(t)) - 1} with \code{z} the
#' \code{(1 + lambda) / 2} quantile of the standard normal distribution. The
#' information fraction \code{t} is capped at one. The arguments are
#' vectorised in the usual way, so a scalar \code{t} may be combined with a
#' vector of \code{gamma} or \code{eta} values.
#'
#' @param t A numeric vector of information fractions.
#' @param lambda A single number between 0 and 1.
#' @param gamma A numeric vector of positive numbers.
#' @param eta A numeric vector of positive numbers, or \code{NULL} when not
#'   used.
#' @param type Either \code{"BOP2-FE"} or \code{"BOP2"}.
#' @param method Either \code{"power"} or \code{"OF"}.
#'
#' @return A list with numeric vectors \code{futility} and \code{efficacy}. The
#'   efficacy cutoff is \code{NA} for the BOP2 design.
#'
#' @importFrom stats pnorm qnorm
#' @noRd
flex_cutoff <- function(t, lambda, gamma, eta = NULL, type = "BOP2-FE",
                        method = "power") {
  t <- pmin(t, 1)
  futility <- lambda * t^gamma
  if (type == "BOP2") {
    efficacy <- rep(NA_real_, length(futility))
  } else if (method == "power") {
    efficacy <- 1 - (1 - lambda) * t^eta
  } else {
    efficacy <- 2 * stats::pnorm(stats::qnorm((1 + lambda) / 2) / sqrt(t)) - 1
  }
  list(futility = futility, efficacy = efficacy)
}
