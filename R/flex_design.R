#' Calibrate a BOP2 or BOP2-FE design
#'
#' Chooses the tuning parameters of the BOP2 design (Zhou et al. 2017) or the
#' BOP2-FE design (Xu et al. 2026) for a single binary endpoint by a grid
#' search, and returns the planned design.
#'
#' For each value of \code{lambda}, the distinct boundary sets produced by the
#' grids of \code{gamma} and \code{eta} are combined. Among the boundary sets
#' whose exact type I error rate is at most \code{alpha}, the one with the
#' largest power is chosen, with ties broken by the smaller expected sample
#' size under the null hypothesis and then by the order of the grids. The
#' reported tuning parameters are the first values in grid order that produce
#' the chosen boundaries, that is, the smallest \code{lambda}, then the
#' smallest \code{gamma} and \code{eta}. Other grid points often give the
#' same boundaries on the planned schedule; they define the same planned
#' design but different probability cutoffs at other sample sizes, and are
#' listed in the element \code{calibration$equivalent}. This follows the
#' procedure of Zhou et al. (2017, Section 2.3) and Xu et al. (2026, Section
#' 2.3), with the operating characteristics computed exactly rather than by
#' simulation. The default
#' grids cover the ranges of Xu et al. (2026) in steps of 0.01 for
#' \code{lambda} (0 to 1) and \code{gamma} (0.01 to 1) and 0.1 for \code{eta}
#' (0.1 to 3).
#'
#' @param p0 A single number strictly between 0 and 1, the null response rate.
#' @param p1 A single number between \code{p0} and 1, the response rate under
#'   the alternative hypothesis.
#' @param n A numeric vector of strictly increasing planned cumulative sample
#'   sizes, at least two, the last being the maximum sample size.
#' @param alpha A single number strictly between 0 and 1, the nominal type I
#'   error rate. Defaults to 0.1.
#' @param type Either \code{"BOP2-FE"} (the default) or \code{"BOP2"}.
#' @param method Either \code{"power"} (the default) or \code{"OF"}; see
#'   \code{\link{flex_boundary}}.
#' @param binding A single logical value; see \code{\link{flex_plan}}.
#' @param lambda_grid A numeric vector of candidate \code{lambda} values
#'   between 0 and 1.
#' @param gamma_grid A numeric vector of positive candidate \code{gamma}
#'   values.
#' @param eta_grid A numeric vector of positive candidate \code{eta} values,
#'   used only for the BOP2-FE design with \code{method = "power"}.
#' @param a,b Positive shape parameters of the beta prior. Default to
#'   \code{p0} and \code{1 - p0}.
#'
#' @return An object of class \code{"flex_design"}; see
#'   \code{\link{flex_plan}}. It also contains the element
#'   \code{calibration}, a list with the grids, the number of distinct
#'   boundary sets evaluated, the number meeting the type I error
#'   constraint, and \code{equivalent}, a data frame with columns
#'   \code{lambda}, \code{gamma} and \code{eta} (\code{NA} when not used)
#'   listing, in grid order, every grid point that gives the selected
#'   boundaries. Its first row holds the reported tuning parameters.
#'
#' @references
#' Zhou, H., Lee, J. J., and Yuan, Y. (2017). BOP2: Bayesian optimal design for
#' phase II clinical trials with simple and complex endpoints.
#' \emph{Statistics in Medicine}, 36(21), 3302-3314.
#' \doi{10.1002/sim.7338}
#'
#' Xu, X., Hashimoto, A., Yimer, B. B., and Takeda, K. (2026). BOP2-FE:
#' Bayesian optimal phase II design with futility and efficacy-stopping
#' boundaries. \emph{Journal of Biopharmaceutical Statistics}, 36(5),
#' 1100-1117.
#' \doi{10.1080/10543406.2025.2558142}
#'
#' @examples
#' # BOP2 design of Zhou et al. (2017, Table I, Example 1)
#' flex_design(p0 = 0.2, p1 = 0.4, n = c(10, 15, 20, 25, 30, 35, 40),
#'             type = "BOP2")
#'
#' \donttest{
#' # BOP2-FE-1 design of Xu et al. (2026, Table 1)
#' flex_design(p0 = 0.2, p1 = 0.4, n = c(10, 15, 20, 25, 30, 35, 40))
#' }
#'
#' @export
flex_design <- function(p0, p1, n, alpha = 0.1, type = c("BOP2-FE", "BOP2"),
                        method = c("power", "OF"), binding = TRUE,
                        lambda_grid = (0:100) / 100,
                        gamma_grid = (1:100) / 100,
                        eta_grid = (1:30) / 10,
                        a = p0, b = 1 - p0) {
  type <- match.arg(type)
  method <- match.arg(method)
  flex_check_number(p0, "p0", 0, 1, open_lower = TRUE, open_upper = TRUE)
  flex_check_number(p1, "p1", p0, 1, open_lower = TRUE, open_upper = TRUE)
  n <- flex_check_looks(n, "n")
  if (length(n) < 2) {
    stop("'n' must contain at least one interim and the final sample size.",
         call. = FALSE)
  }
  flex_check_number(alpha, "alpha", 0, 1, open_lower = TRUE,
                    open_upper = TRUE)
  flex_check_flag(binding, "binding")
  flex_check_number(a, "a", 0, Inf, open_lower = TRUE)
  flex_check_number(b, "b", 0, Inf, open_lower = TRUE)
  use_eta <- type == "BOP2-FE" && method == "power"
  grid_ok <- function(x, lower, upper, open_lower) {
    is.numeric(x) && length(x) > 0 && all(is.finite(x)) &&
      all(if (open_lower) x > lower else x >= lower) && all(x <= upper)
  }
  if (!grid_ok(lambda_grid, 0, 1, FALSE)) {
    stop("'lambda_grid' must contain numbers between 0 and 1.", call. = FALSE)
  }
  if (!grid_ok(gamma_grid, 0, Inf, TRUE)) {
    stop("'gamma_grid' must contain positive numbers.", call. = FALSE)
  }
  if (use_eta && !grid_ok(eta_grid, 0, Inf, TRUE)) {
    stop("'eta_grid' must contain positive numbers.", call. = FALSE)
  }

  num_looks <- length(n)
  N <- n[num_looks]
  t <- n / N
  interim <- seq_len(num_looks - 1)
  post <- lapply(n, function(m) flex_posterior(0:m, m, p0, a, b))

  # Candidate boundary sets for each lambda: distinct futility boundaries over
  # the gamma grid combined with distinct efficacy boundaries over the eta grid
  cand <- vector("list", length(lambda_grid))
  # Boundaries of every grid point, kept to list the equivalent grid points
  e_final_all <- integer(length(lambda_grid))
  f_full <- vector("list", length(lambda_grid))
  e_full <- vector("list", length(lambda_grid))
  eta_full <- vector("list", length(lambda_grid))
  for (i in seq_along(lambda_grid)) {
    lam <- lambda_grid[i]
    e_final <- flex_efficacy_count(post[[num_looks]], lam)
    f_mat <- matrix(-1L, length(gamma_grid), num_looks - 1)
    for (k in interim) {
      f_mat[, k] <- flex_futility_count(post[[k]], lam * t[k]^gamma_grid)
    }
    e_final_all[i] <- e_final
    f_full[[i]] <- f_mat
    keep_f <- !duplicated(f_mat)
    f_uni <- f_mat[keep_f, , drop = FALSE]
    g_uni <- gamma_grid[keep_f]
    if (use_eta) {
      e_mat <- matrix(0L, length(eta_grid), num_looks - 1)
      for (k in interim) {
        e_mat[, k] <- flex_efficacy_count(post[[k]],
                                          1 - (1 - lam) * t[k]^eta_grid)
      }
      e_full[[i]] <- e_mat
      eta_full[[i]] <- eta_grid
      keep_e <- !duplicated(e_mat)
      e_uni <- e_mat[keep_e, , drop = FALSE]
      et_uni <- eta_grid[keep_e]
    } else if (type == "BOP2-FE") {
      ce <- flex_cutoff(t[interim], lam, 1, NULL, "BOP2-FE", "OF")$efficacy
      e_uni <- matrix(vapply(interim, function(k) {
        flex_efficacy_count(post[[k]], ce[k])
      }, integer(1)), nrow = 1)
      et_uni <- NA_real_
      e_full[[i]] <- e_uni
      eta_full[[i]] <- NA_real_
    } else {
      e_uni <- matrix(n[interim] + 1L, nrow = 1)
      et_uni <- NA_real_
      e_full[[i]] <- e_uni
      eta_full[[i]] <- NA_real_
    }
    fi <- rep(seq_len(nrow(f_uni)), each = nrow(e_uni))
    ei <- rep(seq_len(nrow(e_uni)), times = nrow(f_uni))
    e_int <- e_uni[ei, , drop = FALSE]
    f_int <- pmin(f_uni[fi, , drop = FALSE], e_int - 1L)
    cand[[i]] <- list(
      fut = cbind(f_int, e_final - 1L),
      eff = cbind(e_int, e_final),
      par = data.frame(lambda = rep(lam, length(fi)), gamma = g_uni[fi],
                       eta = et_uni[ei])
    )
  }
  fut_all <- do.call(rbind, lapply(cand, function(x) x$fut))
  eff_all <- do.call(rbind, lapply(cand, function(x) x$eff))
  par_all <- do.call(rbind, lapply(cand, function(x) x$par))
  keep <- !duplicated(cbind(fut_all, eff_all))
  fut_all <- fut_all[keep, , drop = FALSE]
  eff_all <- eff_all[keep, , drop = FALSE]
  par_all <- par_all[keep, , drop = FALSE]

  # Exact type I error rate, then power and expected sample size of the
  # feasible boundary sets
  oc0 <- flex_oc_engine(n, fut_all, eff_all, p0, binding)
  feasible <- which(oc0$reject <= alpha + 1e-12)
  if (length(feasible) == 0) {
    stop("No design in the grid has a type I error rate at most 'alpha'.",
         call. = FALSE)
  }
  fut_f <- fut_all[feasible, , drop = FALSE]
  eff_f <- eff_all[feasible, , drop = FALSE]
  par_f <- par_all[feasible, , drop = FALSE]
  power <- flex_oc_engine(n, fut_f, eff_f, p1, TRUE)$reject
  en0 <- flex_oc_engine(n, fut_f, eff_f, p0, TRUE)$en
  near <- which(power >= max(power) - 1e-12)
  best <- near[which(en0[near] <= min(en0[near]) + 1e-12)[1]]

  design <- flex_plan(p0 = p0, n = n, alpha = alpha,
                      lambda = par_f$lambda[best], gamma = par_f$gamma[best],
                      eta = if (use_eta) par_f$eta[best] else NULL,
                      type = type, method = method, p1 = p1,
                      binding = binding, a = a, b = b)
  if (!identical(as.integer(design$boundary$futility),
                 as.integer(fut_f[best, ])) ||
      !identical(as.integer(design$boundary$efficacy),
                 as.integer(eff_f[best, ]))) {
    stop("Internal error: the selected boundaries could not be reproduced.",
         call. = FALSE)
  }
  # Every grid point that gives the selected boundaries
  sel_fut <- as.integer(fut_f[best, interim])
  sel_eff <- as.integer(eff_f[best, ])
  equiv <- list()
  for (i in seq_along(lambda_grid)) {
    if (e_final_all[i] != sel_eff[num_looks]) next
    e_mat <- e_full[[i]]
    e_ok <- which(apply(e_mat, 1, function(x) all(x == sel_eff[interim])))
    for (r in e_ok) {
      cap <- matrix(e_mat[r, ] - 1L, nrow(f_full[[i]]), num_looks - 1,
                    byrow = TRUE)
      f_int <- pmin(f_full[[i]], cap)
      g_ok <- which(apply(f_int, 1, function(x) all(x == sel_fut)))
      if (length(g_ok) > 0) {
        equiv[[length(equiv) + 1]] <- data.frame(
          lambda = lambda_grid[i], gamma = gamma_grid[g_ok],
          eta = eta_full[[i]][r])
      }
    }
  }
  equivalent <- do.call(rbind, equiv)
  equivalent <- equivalent[order(match(equivalent$lambda, lambda_grid),
                                 match(equivalent$gamma, gamma_grid),
                                 match(equivalent$eta, eta_grid)), ]
  rownames(equivalent) <- NULL
  if (!isTRUE(all.equal(equivalent$lambda[1], par_f$lambda[best])) ||
      !isTRUE(all.equal(equivalent$gamma[1], par_f$gamma[best])) ||
      (use_eta && !isTRUE(all.equal(equivalent$eta[1], par_f$eta[best])))) {
    stop("Internal error: the equivalent grid points are inconsistent.",
         call. = FALSE)
  }

  design$source <- "calibration"
  design$calibration <- list(lambda_grid = lambda_grid,
                             gamma_grid = gamma_grid,
                             eta_grid = if (use_eta) eta_grid else NULL,
                             n_candidates = nrow(fut_all),
                             n_feasible = length(feasible),
                             equivalent = equivalent)
  design
}
