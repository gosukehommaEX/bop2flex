#' Exact operating characteristics for several boundary sets
#'
#' Internal engine that propagates the distribution of the cumulative number
#' of responses across the analyses and removes the probability mass that
#' stops at each analysis. Each row of \code{futility} and \code{efficacy} is a
#' separate design evaluated on the same schedule. At an interim analysis the
#' trial stops for efficacy when the count is at least the efficacy boundary
#' and, when \code{binding = TRUE}, for futility when it is at most the
#' futility boundary. At the last analysis the null hypothesis is rejected
#' when the count is at least the efficacy boundary. With
#' \code{binding = FALSE} the futility boundaries are ignored.
#'
#' @param n An integer vector of cumulative sample sizes.
#' @param futility,efficacy Integer matrices with one row per design and one
#'   column per analysis.
#' @param p A single response rate.
#' @param binding A single logical value.
#'
#' @return A list with numeric vectors \code{reject}, \code{early_efficacy},
#'   \code{early_futility} and \code{en} (expected sample size), and matrices
#'   \code{efficacy_by_look} and \code{futility_by_look} of stopping
#'   probabilities at each analysis.
#'
#' @noRd
flex_oc_engine <- function(n, futility, efficacy, p, binding = TRUE) {
  num_looks <- length(n)
  num_designs <- nrow(efficacy)
  dist <- matrix(1, num_designs, 1)
  prev <- 0L
  eff_look <- matrix(0, num_designs, num_looks)
  fut_look <- matrix(0, num_designs, num_looks)
  en <- numeric(num_designs)
  for (k in seq_len(num_looks)) {
    dist <- flex_propagate(dist, n[k] - prev, p)
    prev <- n[k]
    s <- 0:n[k]
    at_eff <- outer(efficacy[, k], s, function(e, x) x >= e)
    eff_look[, k] <- rowSums(dist * at_eff)
    if (k < num_looks) {
      if (binding) {
        at_fut <- outer(futility[, k], s, function(f, x) x <= f) & !at_eff
        fut_look[, k] <- rowSums(dist * at_fut)
        dist <- dist * !(at_eff | at_fut)
      } else {
        dist <- dist * !at_eff
      }
      en <- en + n[k] * (eff_look[, k] + fut_look[, k])
    } else {
      en <- en + n[k] * rowSums(dist)
    }
  }
  list(reject = rowSums(eff_look),
       early_efficacy = rowSums(eff_look[, -num_looks, drop = FALSE]),
       early_futility = rowSums(fut_look),
       en = en,
       efficacy_by_look = eff_look,
       futility_by_look = fut_look)
}
