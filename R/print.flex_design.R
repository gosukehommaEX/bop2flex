#' Print a planned design
#'
#' @param x An object of class \code{"flex_design"}.
#' @param digits A single positive whole number of significant digits for the
#'   operating characteristics.
#' @param ... Not used.
#'
#' @return \code{x}, invisibly.
#'
#' @examples
#' flex_plan(p0 = 0.2, n = c(10, 15, 20, 25, 30, 35, 40), alpha = 0.1,
#'           lambda = 0.87, gamma = 0.94, eta = 2.4, p1 = 0.4)
#'
#' @export
print.flex_design <- function(x, digits = 4, ...) {
  label <- if (identical(x$type, "BOP2")) {
    "BOP2 design (futility stopping only)"
  } else if (identical(x$method, "OF")) {
    "BOP2-FE design (O'Brien-Fleming type efficacy cutoff)"
  } else if (identical(x$method, "power")) {
    "BOP2-FE design (power efficacy cutoff)"
  } else {
    "BOP2-FE design"
  }
  cat("Planned", label, "\n")
  cat(sprintf("  p0 = %s, p1 = %s, alpha = %s, N = %d, %s futility\n",
              format(x$p0), if (is.null(x$p1)) "NA" else format(x$p1),
              format(x$alpha), x$N,
              if (x$binding) "binding" else "non-binding"))
  if (!is.null(x$lambda)) {
    cat(sprintf("  lambda = %s, gamma = %s%s\n", format(x$lambda),
                format(x$gamma),
                if (is.null(x$eta)) "" else paste0(", eta = ",
                                                   format(x$eta))))
  }
  cat("  Specified by:", x$source, "\n\n")
  cat("Boundaries: stop for futility if responses <= futility; stop for",
      "efficacy\n(reject H0 at the final analysis) if responses >= efficacy\n")
  print(x$boundary, row.names = FALSE, digits = digits)
  cat("\nOperating characteristics:\n")
  print(signif(x$oc, digits), row.names = FALSE)
  invisible(x)
}
