#' Futility boundary from posterior probabilities
#'
#' Internal helper returning, for each cutoff, the largest number of responses
#' whose posterior probability is below the cutoff, or -1 when there is none.
#'
#' @param post A numeric vector of posterior probabilities for 0 to \code{m}
#'   responses.
#' @param cutoff A numeric vector of cutoffs.
#'
#' @return An integer vector with one boundary per cutoff.
#'
#' @noRd
flex_futility_count <- function(post, cutoff) {
  vapply(cutoff, function(cc) {
    w <- which(post < cc)
    if (length(w) > 0) max(w) - 1L else -1L
  }, integer(1))
}
