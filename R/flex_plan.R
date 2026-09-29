#' Specify a planned BOP2 or BOP2-FE design
#'
#' Creates a planned design from given tuning parameters or from a table of
#' boundaries, for example a design obtained from another program or taken
#' from a trial protocol. \code{\link{flex_design}} calibrates the tuning
#' parameters and calls this function.
#'
#' With \code{lambda} and \code{gamma} (and \code{eta} for the BOP2-FE design
#' with the power efficacy cutoff) the boundaries are computed by
#' \code{\link{flex_boundary}} at the planned sample sizes \code{n}. With
#' \code{futility} and \code{efficacy} the boundaries are used as given and
#' the design type is inferred from whether any interim efficacy boundary can
#' be reached. A design given by its boundaries has no probability cutoffs, so
#' \code{\link{flex_naive}} cannot be applied to it and
#' \code{\link{flex_update}} needs the futility boundary at any unplanned
#' interim analysis.
#'
#' The type I error rate of the planned design must not exceed \code{alpha},
#' because \code{\link{flex_update}} spends at most \code{alpha} over the
#' whole trial.
#'
#' @param p0 A single number strictly between 0 and 1, the null response rate.
#' @param n A numeric vector of strictly increasing planned cumulative sample
#'   sizes, at least two, the last being the maximum sample size. Note that
#'   these are cumulative totals, for example \code{c(10, 15, 20)}, not the
#'   numbers of patients added at each analysis.
#' @param alpha A single number strictly between 0 and 1, the nominal type I
#'   error rate.
#' @param lambda,gamma,eta Tuning parameters; see \code{\link{flex_boundary}}.
#' @param type Either \code{"BOP2-FE"} or \code{"BOP2"}. Ignored when the
#'   boundaries are supplied.
#' @param method Either \code{"power"} or \code{"OF"}; see
#'   \code{\link{flex_boundary}}. Ignored when the boundaries are supplied.
#' @param futility,efficacy Boundary table in the format of
#'   \code{\link{flex_oc}}, as an alternative to the tuning parameters.
#' @param p1 An optional single number, the response rate under the
#'   alternative hypothesis, larger than \code{p0}.
#' @param binding A single logical value. With \code{TRUE} (the default, as in
#'   Zhou et al. 2017 and Xu et al. 2025) the type I error rate is computed
#'   with binding futility stopping; with \code{FALSE} the futility boundaries
#'   are ignored in the type I error rate.
#' @param a,b Positive shape parameters of the beta prior. Default to
#'   \code{p0} and \code{1 - p0}.
#'
#' @return An object of class \code{"flex_design"}, a list with elements
#'   \code{type}, \code{method}, \code{p0}, \code{p1}, \code{alpha},
#'   \code{binding}, \code{n}, \code{N}, \code{lambda}, \code{gamma},
#'   \code{eta}, \code{prior}, \code{boundary} (a data frame with columns
#'   \code{look}, \code{n}, \code{t}, \code{futility} and \code{efficacy}),
#'   \code{oc} (a data frame with the type I error rate \code{type1}, the
#'   power \code{power}, the expected sample sizes \code{en0} and \code{en1}
#'   under the null and alternative response rates, and the probability of
#'   early stopping under the null \code{early_stop0}) and \code{source}. The
#'   power, the expected sample sizes and the early stopping probability are
#'   computed with the futility boundaries applied.
#'
#' @examples
#' # BOP2-FE-1 design of Xu et al. (2025, Table 1) from its tuning parameters
#' flex_plan(p0 = 0.2, n = c(10, 15, 20, 25, 30, 35, 40), alpha = 0.1,
#'           lambda = 0.87, gamma = 0.94, eta = 2.4, p1 = 0.4)
#'
#' # The same design from its boundary table
#' flex_plan(p0 = 0.2, n = c(10, 15, 20, 25, 30, 35, 40), alpha = 0.1,
#'           futility = c(1, 2, 3, 5, 7, 8),
#'           efficacy = c(6, 8, 9, 9, 10, 11, 12), p1 = 0.4)
#'
#' @export
flex_plan <- function(p0, n, alpha, lambda = NULL, gamma = NULL, eta = NULL,
                      type = c("BOP2-FE", "BOP2"), method = c("power", "OF"),
                      futility = NULL, efficacy = NULL, p1 = NULL,
                      binding = TRUE, a = p0, b = 1 - p0) {
  type <- match.arg(type)
  method <- match.arg(method)
  flex_check_number(p0, "p0", 0, 1, open_lower = TRUE, open_upper = TRUE)
  n <- flex_check_looks(n, "n")
  if (length(n) < 2) {
    stop("'n' must contain at least one interim and the final sample size.",
         call. = FALSE)
  }
  flex_check_number(alpha, "alpha", 0, 1, open_lower = TRUE,
                    open_upper = TRUE)
  if (!is.null(p1)) {
    flex_check_number(p1, "p1", p0, 1, open_lower = TRUE, open_upper = TRUE)
  }
  flex_check_flag(binding, "binding")
  flex_check_number(a, "a", 0, Inf, open_lower = TRUE)
  flex_check_number(b, "b", 0, Inf, open_lower = TRUE)
  num_looks <- length(n)
  N <- n[num_looks]
  by_parameters <- !is.null(lambda) || !is.null(gamma)
  by_table <- !is.null(futility) || !is.null(efficacy)
  if (by_parameters == by_table) {
    stop(paste0("Supply either the tuning parameters 'lambda' and 'gamma' ",
                "(with 'eta' when required) or the boundaries 'futility' and ",
                "'efficacy', but not both."), call. = FALSE)
  }
  if (by_parameters) {
    if (is.null(lambda) || is.null(gamma)) {
      stop("Both 'lambda' and 'gamma' must be supplied.", call. = FALSE)
    }
    if (type == "BOP2" || method == "OF") eta <- NULL
    bd <- flex_boundary(n, N, p0, lambda, gamma, eta, type, method,
                        final = seq_len(num_looks) == num_looks, a = a, b = b)
    fut <- bd$futility
    eff <- bd$efficacy
    source <- "parameters"
    if (type == "BOP2") method <- NA_character_
  } else {
    if (is.null(futility) || is.null(efficacy)) {
      stop("Both 'futility' and 'efficacy' must be supplied.", call. = FALSE)
    }
    bd <- flex_check_boundary(n, futility, efficacy)
    fut <- bd$futility
    eff <- bd$efficacy
    interim <- seq_len(num_looks - 1)
    type <- if (all(eff[interim] == n[interim] + 1L)) "BOP2" else "BOP2-FE"
    method <- NA_character_
    lambda <- NULL
    gamma <- NULL
    eta <- NULL
    source <- "table"
  }
  fut_mat <- matrix(fut, nrow = 1)
  eff_mat <- matrix(eff, nrow = 1)
  oc0 <- flex_oc_engine(n, fut_mat, eff_mat, p0, binding)
  oc0_applied <- flex_oc_engine(n, fut_mat, eff_mat, p0, TRUE)
  if (oc0$reject > alpha + 1e-12) {
    stop(sprintf(paste0("The type I error rate of the planned design (%.5f) ",
                        "exceeds 'alpha' (%.5f)."), oc0$reject, alpha),
         call. = FALSE)
  }
  if (is.null(p1)) {
    power <- NA_real_
    en1 <- NA_real_
  } else {
    oc1 <- flex_oc_engine(n, fut_mat, eff_mat, p1, TRUE)
    power <- oc1$reject
    en1 <- oc1$en
  }
  design <- list(
    type = type,
    method = method,
    p0 = p0,
    p1 = p1,
    alpha = alpha,
    binding = binding,
    n = n,
    N = N,
    lambda = lambda,
    gamma = gamma,
    eta = eta,
    prior = c(a = a, b = b),
    boundary = data.frame(look = seq_len(num_looks), n = n, t = n / N,
                          futility = fut, efficacy = eff),
    oc = data.frame(type1 = oc0$reject, power = power,
                    en0 = oc0_applied$en, en1 = en1,
                    early_stop0 = oc0_applied$early_efficacy +
                      oc0_applied$early_futility),
    source = source
  )
  class(design) <- "flex_design"
  design
}
