#' Random schedules of actual sample sizes around the plan
#'
#' Generates schedules of actual cumulative sample sizes by shifting each
#' planned interim sample size by an integer drawn uniformly from
#' \code{-interim_shift} to \code{interim_shift} and the planned maximum sample
#' size by an integer drawn uniformly from \code{-final_shift} to
#' \code{final_shift}. The shifted sizes are made non-decreasing by a running
#' maximum and at least one, and repeated sizes are merged, so a schedule can
#' have fewer analyses than planned. The last element of each schedule is the
#' final analysis. The shifts are drawn independently of any trial data, as
#' required for the type I error guarantee of \code{\link{flex_update}}.
#'
#' @param design An object of class \code{"flex_design"}.
#' @param n_sim A single positive whole number of schedules.
#' @param interim_shift A single non-negative whole number.
#' @param final_shift A single non-negative whole number.
#' @param seed An optional single number passed to \code{set.seed()}.
#'
#' @return A list of \code{n_sim} integer vectors.
#'
#' @examples
#' design <- flex_plan(p0 = 0.2, n = c(10, 15, 20, 25, 30, 35, 40),
#'                     alpha = 0.1, lambda = 0.87, gamma = 0.94, eta = 2.4)
#' flex_schedules(design, n_sim = 3, seed = 1)
#'
#' @export
flex_schedules <- function(design, n_sim = 1000, interim_shift = 2,
                           final_shift = 4, seed = NULL) {
  flex_check_design(design)
  if (!(length(n_sim) == 1 && flex_is_whole(n_sim) && n_sim >= 1)) {
    stop("'n_sim' must be a single positive whole number.", call. = FALSE)
  }
  if (!(length(interim_shift) == 1 && flex_is_whole(interim_shift) &&
        interim_shift >= 0)) {
    stop("'interim_shift' must be a single non-negative whole number.",
         call. = FALSE)
  }
  if (!(length(final_shift) == 1 && flex_is_whole(final_shift) &&
        final_shift >= 0)) {
    stop("'final_shift' must be a single non-negative whole number.",
         call. = FALSE)
  }
  if (!is.null(seed)) {
    flex_check_number(seed, "seed")
    set.seed(seed)
  }
  n <- design$n
  num_looks <- length(n)
  width_i <- 2L * as.integer(round(interim_shift)) + 1L
  width_f <- 2L * as.integer(round(final_shift)) + 1L
  lapply(seq_len(round(n_sim)), function(i) {
    shift_i <- sample.int(width_i, num_looks - 1L, replace = TRUE) - 1L -
      as.integer(round(interim_shift))
    shift_f <- sample.int(width_f, 1L) - 1L - as.integer(round(final_shift))
    s <- c(n[-num_looks] + shift_i, n[num_looks] + shift_f)
    s <- pmax(cummax(s), 1L)
    as.integer(sort(unique(s)))
  })
}
