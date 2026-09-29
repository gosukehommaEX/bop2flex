#' Compare the naive and spending approaches over actual schedules
#'
#' For each schedule of actual cumulative sample sizes, whose last element is
#' the final analysis, computes the boundaries by \code{\link{flex_naive}} and
#' \code{\link{flex_update}} and their exact operating characteristics.
#'
#' @param design An object of class \code{"flex_design"}.
#' @param schedules A list of numeric vectors of strictly increasing actual
#'   cumulative sample sizes, or a single such vector.
#' @param methods A character vector containing \code{"naive"},
#'   \code{"spending"} or both (the default).
#'
#' @return A data frame with one row per schedule and method and columns
#'   \code{schedule} (the index in \code{schedules}), \code{looks} (the sample
#'   sizes separated by commas), \code{method}, \code{type1} (computed with the
#'   futility rule of the design), \code{power}, \code{en0} and \code{en1}
#'   (computed with the futility boundaries applied; \code{NA} when the design
#'   has no \code{p1}) and \code{exceeds_alpha} (whether \code{type1} is above
#'   the nominal level).
#'
#' @examples
#' design <- flex_plan(p0 = 0.2, n = c(10, 15, 20, 25, 30, 35, 40),
#'                     alpha = 0.1, lambda = 0.87, gamma = 0.94, eta = 2.4,
#'                     p1 = 0.4)
#' flex_compare(design, list(c(10, 15, 20, 25, 30, 35, 44),
#'                           c(12, 16, 19, 25, 31, 37, 39)))
#'
#' @export
flex_compare <- function(design, schedules, methods = c("naive", "spending")) {
  flex_check_design(design)
  if (is.numeric(schedules)) schedules <- list(schedules)
  if (!is.list(schedules) || length(schedules) == 0) {
    stop("'schedules' must be a list of cumulative sample size vectors.",
         call. = FALSE)
  }
  methods <- match.arg(methods, several.ok = TRUE)
  out <- vector("list", length(schedules) * length(methods))
  r <- 0
  for (i in seq_along(schedules)) {
    s <- flex_check_looks(schedules[[i]], "schedules")
    for (meth in methods) {
      bd <- if (meth == "naive") {
        flex_naive(design, s, final = TRUE)
      } else {
        flex_update(design, s, final = TRUE)
      }
      fut <- matrix(bd$bounds$futility, nrow = 1)
      eff <- matrix(bd$bounds$efficacy, nrow = 1)
      o0 <- flex_oc_engine(s, fut, eff, design$p0, design$binding)
      o0_applied <- flex_oc_engine(s, fut, eff, design$p0, TRUE)
      if (is.null(design$p1)) {
        power <- NA_real_
        en1 <- NA_real_
      } else {
        o1 <- flex_oc_engine(s, fut, eff, design$p1, TRUE)
        power <- o1$reject
        en1 <- o1$en
      }
      r <- r + 1
      out[[r]] <- data.frame(schedule = i, looks = paste(s, collapse = ","),
                             method = meth, type1 = o0$reject, power = power,
                             en0 = o0_applied$en, en1 = en1,
                             exceeds_alpha = o0$reject >
                               design$alpha * (1 + 1e-9),
                             stringsAsFactors = FALSE)
    }
  }
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}
