#' Check cumulative sample sizes at the analyses
#'
#' Internal helper that checks that \code{n} contains strictly increasing
#' positive whole numbers, the cumulative numbers of evaluable patients at the
#' analyses, and returns them as integers.
#'
#' @param n A numeric vector of cumulative sample sizes.
#' @param name A single character string, the argument name used in messages.
#'
#' @return An integer vector.
#'
#' @noRd
flex_check_looks <- function(n, name) {
  if (!(flex_is_whole(n) && all(n >= 1))) {
    stop(sprintf("'%s' must contain positive whole numbers.", name),
         call. = FALSE)
  }
  n <- as.integer(round(n))
  if (length(n) > 1 && any(diff(n) <= 0)) {
    stop(sprintf(paste0("'%s' must contain strictly increasing cumulative ",
                        "sample sizes (for example c(10, 15, 20)), not the ",
                        "numbers of patients added at each analysis."), name),
         call. = FALSE)
  }
  n
}
