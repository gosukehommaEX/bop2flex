#' Check and normalise a boundary table
#'
#' Internal helper for boundaries supplied as counts. At an interim analysis
#' the trial stops for futility when the cumulative number of responses is at
#' most the futility boundary and stops for efficacy when it is at least the
#' efficacy boundary. At the final analysis the null hypothesis is rejected
#' when the number of responses is at least the efficacy boundary, so the
#' futility boundary there is the efficacy boundary minus one. A missing
#' interim efficacy boundary means no efficacy stopping and is replaced by the
#' sample size plus one.
#'
#' @param n An integer vector of cumulative sample sizes.
#' @param futility A numeric vector of futility boundaries, of length
#'   \code{length(n) - 1} or \code{length(n)}. The final element, when given,
#'   may be \code{NA}.
#' @param efficacy A numeric vector of efficacy boundaries of length
#'   \code{length(n)}. Interim elements may be \code{NA}.
#'
#' @return A list with integer vectors \code{futility} and \code{efficacy}, each
#'   of length \code{length(n)}.
#'
#' @noRd
flex_check_boundary <- function(n, futility, efficacy) {
  num_looks <- length(n)
  if (length(futility) == num_looks - 1) futility <- c(futility, NA)
  if (length(futility) != num_looks || length(efficacy) != num_looks) {
    stop(paste0("'futility' must have one element per interim analysis, ",
                "optionally followed by one for the final analysis, and ",
                "'efficacy' one element per analysis."), call. = FALSE)
  }
  if (!(is.numeric(futility) || all(is.na(futility))) ||
      !(is.numeric(efficacy) || all(is.na(efficacy)))) {
    stop("'futility' and 'efficacy' must be numeric.", call. = FALSE)
  }
  futility <- as.numeric(futility)
  efficacy <- as.numeric(efficacy)
  interim <- seq_len(num_looks) < num_looks
  if (is.na(efficacy[num_looks])) {
    stop("The efficacy boundary at the final analysis must be supplied.",
         call. = FALSE)
  }
  no_eff <- interim & is.na(efficacy)
  efficacy[no_eff] <- n[no_eff] + 1
  if (anyNA(futility[interim])) {
    stop("The futility boundaries at the interim analyses must not be NA.",
         call. = FALSE)
  }
  if (!is.na(futility[num_looks]) &&
      abs(futility[num_looks] - (efficacy[num_looks] - 1)) > 1e-8) {
    stop(paste0("At the final analysis the futility boundary must equal the ",
                "efficacy boundary minus one."), call. = FALSE)
  }
  futility[num_looks] <- efficacy[num_looks] - 1
  if (!(flex_is_whole(futility) && flex_is_whole(efficacy))) {
    stop("The boundaries must be whole numbers.", call. = FALSE)
  }
  futility <- as.integer(round(futility))
  efficacy <- as.integer(round(efficacy))
  if (any(futility < -1) || any(futility > n) || any(efficacy < 0) ||
      any(efficacy > n + 1)) {
    stop(paste0("Each futility boundary must lie between -1 and the sample ",
                "size, and each efficacy boundary between 0 and the sample ",
                "size plus one."), call. = FALSE)
  }
  if (any(futility[interim] >= efficacy[interim])) {
    stop(paste0("At each interim analysis the futility boundary must be ",
                "smaller than the efficacy boundary."), call. = FALSE)
  }
  list(futility = futility, efficacy = efficacy)
}
