#' Exact operating characteristics of boundaries on a schedule
#'
#' Computes, without simulation, the probability of rejecting the null
#' hypothesis, the probabilities of early stopping and the expected sample
#' size for given boundaries on the cumulative number of responses at the
#' cumulative sample sizes \code{n}. The distribution of the number of
#' responses is propagated across the analyses, and the probability of the
#' counts that meet a boundary is removed at each analysis.
#'
#' At an interim analysis the trial stops for futility when the number of
#' responses is at most the futility boundary and stops for efficacy
#' (rejecting the null hypothesis) when it is at least the efficacy boundary.
#' At the last element of \code{n}, the final analysis, the null hypothesis is
#' rejected when the number of responses is at least the efficacy boundary.
#'
#' @param n A numeric vector of strictly increasing cumulative sample sizes.
#' @param futility A numeric vector of futility boundaries, one per interim
#'   analysis, optionally followed by the final one, which must then be
#'   \code{NA} or the final efficacy boundary minus one.
#' @param efficacy A numeric vector of efficacy boundaries, one per analysis.
#'   An interim value of \code{NA} means no efficacy stopping at that analysis.
#' @param p A numeric vector of true response rates between 0 and 1.
#' @param binding A single logical value. With \code{TRUE} (the default) the
#'   trial stops at the futility boundaries. With \code{FALSE} they are
#'   ignored, so that \code{reject} evaluated at the null response rate is the
#'   type I error rate under non-binding futility stopping.
#'
#' @return A data frame with one row per element of \code{p} and columns
#'   \code{p}, \code{reject} (probability of rejecting the null hypothesis),
#'   \code{early_efficacy} and \code{early_futility} (probabilities of
#'   stopping at an interim analysis for each reason) and \code{en} (expected
#'   sample size).
#'
#' @examples
#' # BOP2 boundaries of Zhou et al. (2017, Table I, Example 1)
#' flex_oc(n = c(10, 15, 20, 25, 30, 35, 40),
#'         futility = c(1, 2, 4, 5, 7, 9),
#'         efficacy = c(rep(NA, 6), 11),
#'         p = c(0.2, 0.4))
#'
#' @export
flex_oc <- function(n, futility, efficacy, p, binding = TRUE) {
  n <- flex_check_looks(n, "n")
  bd <- flex_check_boundary(n, futility, efficacy)
  if (!(is.numeric(p) && length(p) > 0 && all(is.finite(p)) &&
        all(p >= 0) && all(p <= 1))) {
    stop("'p' must contain numbers between 0 and 1.", call. = FALSE)
  }
  flex_check_flag(binding, "binding")
  fut <- matrix(bd$futility, nrow = 1)
  eff <- matrix(bd$efficacy, nrow = 1)
  res <- lapply(p, function(pp) {
    o <- flex_oc_engine(n, fut, eff, pp, binding)
    data.frame(p = pp, reject = o$reject, early_efficacy = o$early_efficacy,
               early_futility = o$early_futility, en = o$en)
  })
  do.call(rbind, res)
}
