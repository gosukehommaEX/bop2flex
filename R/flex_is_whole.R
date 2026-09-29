#' Check that values are finite whole numbers
#'
#' Internal helper used by the argument checks. Values are compared with their
#' rounded values with a small tolerance, so that sample sizes computed in
#' floating point (for example \code{0.3 * 10}) are accepted.
#'
#' @param x A numeric vector.
#' @param tol A single non-negative tolerance.
#'
#' @return A single logical value, \code{TRUE} when \code{x} is a non-empty
#'   numeric vector whose elements are all finite and within \code{tol} of a
#'   whole number.
#'
#' @noRd
flex_is_whole <- function(x, tol = 1e-8) {
  is.numeric(x) && length(x) > 0 && all(is.finite(x)) &&
    all(abs(x - round(x)) < tol)
}
