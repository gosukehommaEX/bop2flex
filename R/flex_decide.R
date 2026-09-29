#' Add decisions to a boundary table
#'
#' Internal helper that compares observed cumulative numbers of responses
#' with the boundaries at each analysis. Once a stopping boundary is met, the
#' decisions at later analyses are \code{NA}.
#'
#' @param bounds A data frame with columns \code{m}, \code{final},
#'   \code{futility} and \code{efficacy}.
#' @param y A numeric vector of cumulative numbers of responses, one per row
#'   of \code{bounds}.
#'
#' @return \code{bounds} with the columns \code{responses} and
#'   \code{decision} added.
#'
#' @noRd
flex_decide <- function(bounds, y) {
  num <- nrow(bounds)
  if (length(y) != num) {
    stop("'y' must have one element per analysis in 'm'.", call. = FALSE)
  }
  if (!(flex_is_whole(y) && all(y >= 0) && all(y <= bounds$m + 1e-8))) {
    stop(paste0("'y' must contain whole numbers between 0 and the ",
                "corresponding element of 'm'."), call. = FALSE)
  }
  y <- as.integer(round(y))
  if (num > 1 && (any(diff(y) < 0) || any(diff(y) > diff(bounds$m)))) {
    stop(paste0("'y' must contain cumulative numbers of responses, which ",
                "cannot decrease or grow by more than the number of patients ",
                "added."), call. = FALSE)
  }
  decision <- character(num)
  stopped <- FALSE
  for (k in seq_len(num)) {
    if (stopped) {
      decision[k] <- NA_character_
    } else if (bounds$final[k]) {
      decision[k] <- if (y[k] >= bounds$efficacy[k]) "go (reject H0)" else
        "no-go"
    } else if (y[k] >= bounds$efficacy[k]) {
      decision[k] <- "stop for efficacy"
      stopped <- TRUE
    } else if (y[k] <= bounds$futility[k]) {
      decision[k] <- "stop for futility"
      stopped <- TRUE
    } else {
      decision[k] <- "continue"
    }
  }
  if (anyNA(decision)) {
    warning(paste0("A stopping boundary was met before the last analysis; ",
                   "the decisions at later analyses are NA."), call. = FALSE)
  }
  bounds$responses <- y
  bounds$decision <- decision
  bounds
}
