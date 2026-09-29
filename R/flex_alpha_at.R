#' Evaluate the implied error spending function
#'
#' Internal helper that interpolates linearly between the points returned by
#' \code{flex_spending()}. Information fractions above one are set to one.
#'
#' @param t A numeric vector of information fractions.
#' @param spending A data frame returned by \code{flex_spending()}.
#'
#' @return A numeric vector of cumulative type I error rates.
#'
#' @importFrom stats approx
#' @noRd
flex_alpha_at <- function(t, spending) {
  stats::approx(spending$t, spending$alpha_cum, xout = pmin(t, 1))$y
}
