#' Check a single number against a range
#'
#' Internal helper that stops with an informative message unless \code{x} is a
#' single finite number within the range given by \code{lower} and
#' \code{upper}.
#'
#' @param x The value to check.
#' @param name A single character string, the argument name used in messages.
#' @param lower,upper Single numbers, the limits of the range.
#' @param open_lower,open_upper Single logical values; \code{TRUE} excludes the
#'   corresponding limit from the range.
#'
#' @return \code{x}, invisibly.
#'
#' @noRd
flex_check_number <- function(x, name, lower = -Inf, upper = Inf,
                              open_lower = FALSE, open_upper = FALSE) {
  ok <- length(x) == 1 && is.numeric(x) && is.finite(x)
  if (ok) {
    ok <- (if (open_lower) x > lower else x >= lower) &&
      (if (open_upper) x < upper else x <= upper)
  }
  if (!ok) {
    stop(sprintf("'%s' must be a single number in the range %s%s, %s%s.",
                 name, if (open_lower) "(" else "[", format(lower),
                 format(upper), if (open_upper) ")" else "]"),
         call. = FALSE)
  }
  invisible(x)
}
