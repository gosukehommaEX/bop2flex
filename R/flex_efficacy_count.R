#' Efficacy boundary from posterior probabilities
#'
#' Internal helper returning, for each cutoff, the smallest number of
#' responses whose posterior probability is at least the cutoff, or \code{m + 1}
#' when there is none or when the cutoff is \code{NA}.
#'
#' @param post A numeric vector of posterior probabilities for 0 to \code{m}
#'   responses.
#' @param cutoff A numeric vector of cutoffs.
#'
#' @return An integer vector with one boundary per cutoff.
#'
#' @noRd
flex_efficacy_count <- function(post, cutoff) {
  m <- length(post) - 1L
  vapply(cutoff, function(cc) {
    if (is.na(cc)) return(m + 1L)
    w <- which(post >= cc)
    if (length(w) > 0) min(w) - 1L else m + 1L
  }, integer(1))
}
