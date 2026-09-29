#' Print boundaries at the actual analyses
#'
#' @param x An object of class \code{"flex_bounds"}.
#' @param digits A single positive whole number of significant digits for the
#'   information fractions and error rates.
#' @param ... Not used.
#'
#' @return \code{x}, invisibly.
#'
#' @examples
#' design <- flex_plan(p0 = 0.2, n = c(10, 15, 20, 25, 30, 35, 40),
#'                     alpha = 0.1, lambda = 0.87, gamma = 0.94, eta = 2.4)
#' flex_update(design, m = c(12, 16, 19), final = FALSE)
#'
#' @export
print.flex_bounds <- function(x, digits = 4, ...) {
  if (x$method == "spending") {
    cat("Boundaries from the implied error spending function of the",
        "planned design\n")
  } else {
    cat("Naive boundaries (planned probability cutoffs at the actual",
        "sample sizes)\n")
  }
  cat(sprintf("  nominal alpha = %s, %s futility, planned N = %d\n",
              format(x$design$alpha),
              if (x$design$binding) "binding" else "non-binding",
              x$design$N))
  b <- x$bounds
  is_dbl <- vapply(b, is.double, logical(1))
  b[is_dbl] <- lapply(b[is_dbl], signif, digits = digits)
  print(b, row.names = FALSE)
  invisible(x)
}
