#' Implied error spending function of a planned design
#'
#' Computes the cumulative type I error rate spent by the efficacy boundaries
#' of a planned design up to each planned analysis, under the null response
#' rate and with the futility rule (binding or non-binding) of the design. The
#' implied error spending function used by \code{\link{flex_update}} joins
#' these points linearly in the information fraction \code{t}, starting from
#' zero at \code{t = 0}, and stays at its value at \code{t = 1} beyond the
#' planned maximum sample size. Because it is built from the design's own
#' boundaries, applying it on the planned schedule recovers those boundaries.
#'
#' For the BOP2 design without interim efficacy stopping, the function is zero
#' at every planned interim analysis and reaches the planned type I error rate
#' at \code{t = 1}; by linear interpolation it is positive between the last
#' planned interim analysis and the final analysis.
#'
#' @param design An object of class \code{"flex_design"}.
#'
#' @return A data frame with columns \code{look} (0 for the start of the
#'   trial), \code{n}, \code{t} and \code{alpha_cum}, the cumulative type I
#'   error rate spent up to that analysis.
#'
#' @examples
#' design <- flex_plan(p0 = 0.2, n = c(10, 15, 20, 25, 30, 35, 40),
#'                     alpha = 0.1, lambda = 0.87, gamma = 0.94, eta = 2.4)
#' flex_spending(design)
#'
#' @export
flex_spending <- function(design) {
  flex_check_design(design)
  n <- design$n
  num_looks <- length(n)
  o <- flex_oc_engine(n, matrix(design$boundary$futility, nrow = 1),
                      matrix(design$boundary$efficacy, nrow = 1),
                      design$p0, design$binding)
  data.frame(look = 0:num_looks, n = c(0L, n), t = c(0, n / design$N),
             alpha_cum = c(0, cumsum(o$efficacy_by_look[1, ])))
}
