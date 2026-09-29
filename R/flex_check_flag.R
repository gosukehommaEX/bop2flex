#' Check a single logical value
#'
#' Internal helper that stops unless \code{x} is \code{TRUE} or \code{FALSE}.
#'
#' @param x The value to check.
#' @param name A single character string, the argument name used in messages.
#'
#' @return \code{x}, invisibly.
#'
#' @noRd
flex_check_flag <- function(x, name) {
  if (!(is.logical(x) && length(x) == 1 && !is.na(x))) {
    stop(sprintf("'%s' must be TRUE or FALSE.", name), call. = FALSE)
  }
  invisible(x)
}
