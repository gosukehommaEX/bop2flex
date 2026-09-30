#' Boundaries at the actual analyses by the implied error spending function
#'
#' Computes stopping boundaries when the numbers of evaluable patients at the
#' interim and final analyses differ from those planned. The efficacy
#' boundary at each analysis is determined by an exact type I error
#' calculation that conditions on the boundaries actually applied at the
#' earlier analyses. When the trial follows its plan, the planned boundaries
#' are recovered.
#'
#' Let \code{alpha(t)} be the implied error spending function of the planned
#' design (\code{\link{flex_spending}}), with \code{t = m / N} for the planned
#' maximum sample size \code{N}. At the \code{k}-th actual analysis with
#' \code{m_k} patients, the distribution of the number of responses under the
#' null response rate among trials still running is computed exactly from the
#' boundaries applied at the earlier analyses. The efficacy boundary is the
#' smallest count for which the cumulative type I error rate does not exceed
#' \code{alpha(m_k / N)} at an interim analysis, or \code{alpha} at the final
#' analysis, so that the final analysis uses the error rate not spent
#' earlier. The efficacy boundary is never lower than the smallest count whose
#' posterior probability \code{Pr(p > p0 | data)} reaches the final cutoff
#' \code{lambda} of the design, so that efficacy is never claimed with weaker
#' posterior evidence than the planned final analysis requires; for a design
#' given by its boundary table, the smallest posterior probability at the
#' planned efficacy boundaries that do not exceed the planned sample sizes is
#' used instead of \code{lambda}. For a table obtained from
#' \code{\link{flex_design}}, this is at least the \code{lambda} of every grid
#' point that gives the table. This floor only raises boundaries; with the
#' tuning parameters, or with a table obtained from \code{\link{flex_design}},
#' it leaves the planned boundaries unchanged. The
#' futility boundary is the planned futility cutoff \code{C_F(m_k / N)} (see
#' \code{\link{flex_boundary}}) unless given in \code{futility}, and is lowered
#' when needed to lie below the efficacy boundary.
#'
#' For any sequence of analysis sample sizes chosen without regard to the
#' observed responses, the type I error rate of the procedure is at most
#' \code{alpha}, up to a relative tolerance of \code{1e-9} used to absorb
#' rounding in the comparison with the spending function. With non-binding
#' futility (\code{binding = FALSE} in the design) the guarantee holds whether
#' or not the futility boundaries are followed.
#'
#' The function is meant to be called at each analysis with all actual sample
#' sizes so far; the boundaries at earlier analyses depend only on the earlier
#' sample sizes and are therefore reproduced unchanged.
#'
#' Another spending function can be supplied through \code{spending}, for
#' example \code{alpha t^rho}, for comparison. The type I error guarantee holds
#' for any non-decreasing spending function that starts at zero and does not
#' exceed \code{alpha}, but only the implied spending function recovers the
#' planned boundaries. The floor on the efficacy boundary can be removed with
#' \code{efficacy_floor = FALSE}, which also keeps the guarantee.
#'
#' @param design An object of class \code{"flex_design"}.
#' @param m A numeric vector of strictly increasing actual cumulative sample
#'   sizes up to the current analysis.
#' @param final A single logical value, \code{TRUE} when the last element of
#'   \code{m} is the final analysis. Defaults to \code{FALSE}.
#' @param y An optional numeric vector of observed cumulative numbers of
#'   responses at the analyses in \code{m}. When supplied, the decision at each
#'   analysis is added.
#' @param futility An optional numeric vector with one element per analysis
#'   giving the futility boundary actually applied; \code{NA} elements use the
#'   default. It is required at unplanned interim analyses of a design given
#'   by its boundary table.
#' @param spending An optional data frame with columns \code{t} and
#'   \code{alpha_cum} defining another spending function by linear
#'   interpolation, with \code{t} strictly increasing from 0 to 1 and
#'   \code{alpha_cum} non-decreasing from 0 to at most \code{alpha}. The
#'   default \code{NULL} uses \code{\link{flex_spending}(design)}.
#' @param efficacy_floor A single logical value. With \code{TRUE} (the default)
#'   the efficacy boundary is never below the smallest count whose posterior
#'   probability reaches the final cutoff of the design.
#'
#' @return An object of class \code{"flex_bounds"}, a list with elements
#'   \code{method} (\code{"spending"}), \code{design}, \code{final} and
#'   \code{bounds}, a data frame with columns \code{look}, \code{m}, \code{t},
#'   \code{final}, \code{futility}, \code{efficacy}, \code{alpha_target} (the
#'   spending function value used) and \code{alpha_spent} (the cumulative type
#'   I error rate up to the analysis), and with \code{responses} and
#'   \code{decision} when \code{y} is supplied.
#'
#' @examples
#' design <- flex_plan(p0 = 0.2, n = c(10, 15, 20, 25, 30, 35, 40),
#'                     alpha = 0.1, lambda = 0.87, gamma = 0.94, eta = 2.4)
#' # Planned schedule: the planned boundaries are recovered
#' flex_update(design, m = c(10, 15, 20, 25, 30, 35, 40), final = TRUE)
#' # Actual schedule that differs from the plan
#' flex_update(design, m = c(12, 16, 19, 25, 31, 37, 39), final = TRUE)
#'
#' @export
flex_update <- function(design, m, final = FALSE, y = NULL, futility = NULL,
                        spending = NULL, efficacy_floor = TRUE) {
  flex_check_design(design)
  m <- flex_check_looks(m, "m")
  flex_check_flag(final, "final")
  flex_check_flag(efficacy_floor, "efficacy_floor")
  if (!is.null(spending)) {
    ok <- is.data.frame(spending) &&
      all(c("t", "alpha_cum") %in% names(spending)) && nrow(spending) >= 2
    if (ok) {
      tt <- spending$t
      aa <- spending$alpha_cum
      ok <- is.numeric(tt) && is.numeric(aa) && all(is.finite(tt)) &&
        all(is.finite(aa)) && abs(tt[1]) < 1e-12 && abs(aa[1]) < 1e-12 &&
        all(diff(tt) > 0) && all(diff(aa) >= 0) &&
        abs(tt[length(tt)] - 1) < 1e-12 &&
        aa[length(aa)] <= design$alpha * (1 + 1e-9)
    }
    if (!ok) {
      stop(paste0("'spending' must be a data frame with columns 't', ",
                  "strictly increasing from 0 to 1, and 'alpha_cum', ",
                  "non-decreasing from 0 to at most the alpha of the ",
                  "design."), call. = FALSE)
    }
  }
  num <- length(m)
  if (!is.null(futility)) {
    if (length(futility) != num) {
      stop(paste0("'futility' must have one element per analysis in 'm' ",
                  "(NA keeps the default)."), call. = FALSE)
    }
    given <- !is.na(futility)
    if (any(given) && !(flex_is_whole(futility[given]) &&
                        all(futility[given] >= -1) &&
                        all(futility[given] <= m[given]))) {
      stop(paste0("'futility' must contain whole numbers between -1 and the ",
                  "corresponding element of 'm', or NA."), call. = FALSE)
    }
  }
  p0 <- design$p0
  N <- design$N
  a <- design$prior[["a"]]
  b <- design$prior[["b"]]
  has_par <- !is.null(design$lambda)
  # Posterior evidence required for a claim of efficacy
  final_cut <- if (has_par) {
    design$lambda
  } else {
    # Weakest posterior evidence at which the planned design claims efficacy
    reach <- design$boundary$efficacy <= design$n
    min(vapply(which(reach), function(j) {
      flex_posterior(design$boundary$efficacy[j], design$n[j], p0, a, b)
    }, numeric(1)))
  }
  if (is.null(spending)) spending <- flex_spending(design)
  dist <- 1
  prev <- 0L
  spent <- 0
  fut <- integer(num)
  eff <- integer(num)
  target <- numeric(num)
  spent_by_look <- numeric(num)
  for (k in seq_len(num)) {
    dist <- as.vector(flex_propagate(matrix(dist, nrow = 1), m[k] - prev, p0))
    prev <- m[k]
    is_final <- final && k == num
    target[k] <- if (is_final) design$alpha else flex_alpha_at(m[k] / N,
                                                               spending)
    # tail[j] is the null probability of at least j - 1 responses among
    # trials still running
    tail <- c(rev(cumsum(rev(dist))), 0)
    ok <- which(spent + tail <= target[k] * (1 + 1e-9))
    post <- flex_posterior(0:m[k], m[k], p0, a, b)
    e_floor <- if (efficacy_floor) {
      flex_efficacy_count(post, final_cut)
    } else {
      0L
    }
    eff[k] <- max(min(ok) - 1L, e_floor)
    spent <- spent + tail[eff[k] + 1L]
    spent_by_look[k] <- spent
    if (is_final) {
      fut[k] <- eff[k] - 1L
    } else {
      if (!is.null(futility) && !is.na(futility[k])) {
        f <- as.integer(round(futility[k]))
      } else if (has_par) {
        cut <- flex_cutoff(m[k] / N, design$lambda, design$gamma, design$eta,
                           design$type, design$method)
        f <- flex_futility_count(post, cut$futility)
      } else {
        j <- match(m[k], design$n)
        if (is.na(j)) {
          stop(sprintf(paste0("The design is given by its boundary table, so ",
                              "the futility boundary at %d patients, which is ",
                              "not a planned analysis, must be supplied ",
                              "through 'futility'."), m[k]), call. = FALSE)
        }
        f <- design$boundary$futility[j]
      }
      fut[k] <- min(f, eff[k] - 1L)
      s <- 0:m[k]
      stop_now <- s >= eff[k]
      if (design$binding) stop_now <- stop_now | s <= fut[k]
      dist[stop_now] <- 0
    }
  }
  bounds <- data.frame(look = seq_len(num), m = m, t = m / N,
                       final = seq_len(num) == num & final,
                       futility = fut, efficacy = eff,
                       alpha_target = target, alpha_spent = spent_by_look)
  if (!is.null(y)) bounds <- flex_decide(bounds, y)
  structure(list(method = "spending", design = design, final = final,
                 bounds = bounds), class = "flex_bounds")
}
