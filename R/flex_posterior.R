#' Posterior probability that the response rate exceeds the null value
#'
#' Computes the posterior probability \code{Pr(p > p0 | S_m = y)} that the
#' response rate \code{p} exceeds the null response rate \code{p0}, given
#' \code{y} responses among \code{m} evaluable patients, under a beta prior.
#' This is the quantity monitored by the BOP2 design of Zhou et al. (2017) and
#' the BOP2-FE design of Xu et al. (2026) for a single binary endpoint.
#'
#' With a \code{Beta(a, b)} prior the posterior distribution of \code{p} is
#' \code{Beta(a + y, b + m - y)}, so the probability is an upper tail of that
#' beta distribution. Zhou et al. (2017) and Xu et al. (2026) use a Dirichlet
#' prior whose parameters sum to one, which for a binary endpoint is a beta
#' prior with \code{a + b = 1}; the values of \code{a} and \code{b} are not
#' fixed in the articles. The default \code{a = p0} and \code{b = 1 - p0}
#' satisfies this condition and centres the prior at the null response rate.
#' The probability is increasing in \code{y} (Zhou et al. 2017, Lemma 1), which
#' is why the stopping rules can be written as boundaries on the number of
#' responses.
#'
#' @param y A numeric vector of numbers of responses. Each element must be a
#'   whole number between 0 and \code{m}.
#' @param m A single whole number, the number of evaluable patients.
#' @param p0 A single numeric value strictly between 0 and 1, the response rate
#'   under the null hypothesis.
#' @param a A single positive numeric value, the first shape parameter of the
#'   beta prior. Defaults to \code{p0}.
#' @param b A single positive numeric value, the second shape parameter of the
#'   beta prior. Defaults to \code{1 - p0}.
#'
#' @return A numeric vector of the same length as \code{y} containing
#'   \code{Pr(p > p0 | S_m = y)}.
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
#' # Posterior probabilities after 10 patients with a null response rate of 0.2
#' flex_posterior(y = 0:10, m = 10, p0 = 0.2)
#'
#' # Uniform prior
#' flex_posterior(y = 3, m = 10, p0 = 0.2, a = 1, b = 1)
#'
#' @importFrom stats pbeta
#' @export
flex_posterior <- function(y, m, p0, a = p0, b = 1 - p0) {
  if (!(length(p0) == 1 && is.numeric(p0) && is.finite(p0) &&
        p0 > 0 && p0 < 1)) {
    stop("'p0' must be a single number strictly between 0 and 1.",
         call. = FALSE)
  }
  if (!(length(m) == 1 && flex_is_whole(m) && m >= 0)) {
    stop("'m' must be a single non-negative whole number.", call. = FALSE)
  }
  if (!(flex_is_whole(y) && all(y >= 0) && all(y <= m + 1e-8))) {
    stop("'y' must contain whole numbers between 0 and 'm'.", call. = FALSE)
  }
  if (!(length(a) == 1 && is.numeric(a) && is.finite(a) && a > 0)) {
    stop("'a' must be a single positive number.", call. = FALSE)
  }
  if (!(length(b) == 1 && is.numeric(b) && is.finite(b) && b > 0)) {
    stop("'b' must be a single positive number.", call. = FALSE)
  }
  y <- round(y)
  m <- round(m)
  # Upper tail of the Beta(a + y, b + m - y) posterior at p0
  stats::pbeta(p0, a + y, b + m - y, lower.tail = FALSE)
}
