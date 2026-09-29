#' Naive boundaries at the actual analyses
#'
#' Evaluates the planned probability cutoffs of the design at the actual
#' sample sizes, with the information fraction \code{t = m / N} relative to
#' the planned maximum sample size \code{N} (capped at one), and uses the final
#' cutoff \code{lambda} at the actual final sample size. This is the
#' intuitive way of handling sample size deviations, which reads the cutoff
#' \code{C(n)} of Zhou et al. (2017) as applicable at any sample size. It does
#' not control the type I error rate, and it is provided for comparison with
#' \code{\link{flex_update}}. The cumulative type I error rate actually spent
#' is reported.
#'
#' @param design An object of class \code{"flex_design"} specified by its
#'   tuning parameters.
#' @param m A numeric vector of strictly increasing actual cumulative sample
#'   sizes.
#' @param final A single logical value, \code{TRUE} when the last element of
#'   \code{m} is the final analysis. Defaults to \code{FALSE}.
#' @param y An optional numeric vector of observed cumulative numbers of
#'   responses at the analyses in \code{m}.
#'
#' @return An object of class \code{"flex_bounds"} with \code{method} equal to
#'   \code{"naive"}; see \code{\link{flex_update}}. The column
#'   \code{alpha_target} is \code{NA}.
#'
#' @references
#' Zhou, H., Lee, J. J., and Yuan, Y. (2017). BOP2: Bayesian optimal design for
#' phase II clinical trials with simple and complex endpoints.
#' \emph{Statistics in Medicine}, 36(21), 3302-3314.
#' \doi{10.1002/sim.7338}
#'
#' @examples
#' design <- flex_plan(p0 = 0.2, n = c(10, 15, 20, 25, 30, 35, 40),
#'                     alpha = 0.1, lambda = 0.87, gamma = 0.94, eta = 2.4)
#' # The type I error rate exceeds 0.1 on this schedule
#' flex_naive(design, m = c(12, 16, 19, 25, 31, 37, 39), final = TRUE)
#'
#' @export
flex_naive <- function(design, m, final = FALSE, y = NULL) {
  flex_check_design(design)
  if (is.null(design$lambda)) {
    stop(paste0("The naive boundaries need the tuning parameters of the ",
                "design; a design given by its boundary table has no ",
                "probability cutoffs to evaluate at other sample sizes."),
         call. = FALSE)
  }
  m <- flex_check_looks(m, "m")
  flex_check_flag(final, "final")
  num <- length(m)
  method <- if (is.na(design$method)) "power" else design$method
  bd <- flex_boundary(m, design$N, design$p0, design$lambda, design$gamma,
                      design$eta, design$type, method,
                      final = seq_len(num) == num & final,
                      a = design$prior[["a"]], b = design$prior[["b"]])
  o <- flex_oc_engine(m, matrix(bd$futility, nrow = 1),
                      matrix(bd$efficacy, nrow = 1), design$p0,
                      design$binding)
  bounds <- data.frame(look = seq_len(num), m = m, t = m / design$N,
                       final = bd$final, futility = bd$futility,
                       efficacy = bd$efficacy, alpha_target = NA_real_,
                       alpha_spent = cumsum(o$efficacy_by_look[1, ]))
  if (!is.null(y)) bounds <- flex_decide(bounds, y)
  structure(list(method = "naive", design = design, final = final,
                 bounds = bounds), class = "flex_bounds")
}
