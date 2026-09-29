#' Check a design object
#'
#' Internal helper that stops unless \code{design} was created by
#' \code{flex_design()} or \code{flex_plan()}.
#'
#' @param design The object to check.
#'
#' @return \code{design}, invisibly.
#'
#' @noRd
flex_check_design <- function(design) {
  if (!inherits(design, "flex_design")) {
    stop("'design' must be an object created by flex_design() or flex_plan().",
         call. = FALSE)
  }
  invisible(design)
}
