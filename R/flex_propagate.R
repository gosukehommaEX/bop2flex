#' Propagate the distribution of the number of responses
#'
#' Internal helper that convolves each row of \code{dist}, a sub-probability
#' distribution of the cumulative number of responses, with the binomial
#' distribution of the responses among \code{inc} additional patients.
#'
#' @param dist A numeric matrix; row \code{i} holds the probabilities of 0 to
#'   \code{ncol(dist) - 1} responses for design \code{i}.
#' @param inc A single non-negative whole number of additional patients.
#' @param p A single response rate.
#'
#' @return A numeric matrix with \code{nrow(dist)} rows and
#'   \code{ncol(dist) + inc} columns.
#'
#' @importFrom stats dbinom
#' @noRd
flex_propagate <- function(dist, inc, p) {
  if (inc == 0) return(dist)
  pmf <- stats::dbinom(0:inc, inc, p)
  width <- ncol(dist)
  out <- matrix(0, nrow(dist), width + inc)
  for (j in 0:inc) {
    idx <- seq_len(width) + j
    out[, idx] <- out[, idx, drop = FALSE] + pmf[j + 1] * dist
  }
  out
}
