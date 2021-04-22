# TODO
# Unit Test to check if it really doesn't make any difference whether none, some or all ranks
#   are present in contingency table for ts_variance

#' Calculate Sen-Slope
#'
#' @description Calculation of Sen's slope. See Details.
#'
#' @param t A vector of ranks (numeric, or coercible to numeric).
#'   For Time Series the observation dates.
#' @param Y A vector of ranks. For Time Series the values observed at each
#'   \emph{t}.
#'
#' @return
#' @export
#'
#' @details \loadmathjax Sen's slope is calculated as followed:
#'   \mjsdeqn{b = \frac{x_{n/2} + x_{n/2 + 1}}{2}}
#'   with \eqn{x} being the differences of every \mjseqn{\binom{n}{2}} value pairs:
#'   \mjsdeqn{x = (Y_{j} - Y_{i})/(t_{j} - t_{i}), 1 \le \, i < \, j \, \le n}
#'
#' @references Sen, Pranab Kumar (1968): Estimates of the Regression
#'   Coefficient Based on Kendall's Tau. In: Journal of the American
#'   Statistical Association 63 (324), P. 1379-1389. DOI: 10.2307/2285891
#'
#' @seealso \code{\link{ts_test}}
ts_slope <- function(t = NULL, Y) {
  N <- length(Y)
  t <- check_sequence(t, N)

  comb_t <- utils::combn(t, 2)
  comb_Y <- utils::combn(Y, 2)

  stats::median(
    # aufgrund der von Sen genannten Bediungungen zu t_j und t_i kann ich nicht einfach so die Summe bilden, auch wenn
    # das vielleicht schneller wäre.
    purrr::pmap_dbl(
      list(comb_Y[, 2], comb_Y[, 1], comb_t[, 2], comb_t[, 1]),
      function(Yj, Yi, tj, ti) {
        if (tj <= ti | tj - ti == 0) {
          return(NA)
        }
        (Yj - Yi) / (tj - ti)
      }
    ),
    na.rm = TRUE
  )
}


#' Calculate Test Statistic for Sen-Slope
#'
#' Calculate Test Statistic of Sen's Slope.
#'
#' @param t A vector of ranks (numeric, or coercible to numeric).
#'   For Time Series the observation dates.
#' @param Y A vector of ranks (numeric, or coercible to numeric).
#'   For Time Series the observation dates.
#' @param slope Slope of Rankings.
#'
#' @return
#' @export
#'
#' @seealso \code{\link{ts_test}}
ts_score <- function(t = NULL, Y, slope) {
  N <- length(Y)
  t <- check_sequence(t, N)

  coeff_T <- t * slope

  z <- Y - coeff_T

  result <- sgn(t, z)

  list("coeff" = coeff_T, "score" = result)
}

#' Calculate Variance of Sen's Test Statistic
#'
#' Calculates variance Sen's Test Statistic Z.
#'
#' @param n A numeric. Length of the ranking.
#' @param u Contingency table of ranks in one ranking.
#'   Will be coerced to numeric vector.
#'
#' @return
#' @export
#'
#' @details This is equivalent to the calculation of the
#'   variance Kendall's test statistic, accounting for
#'   duplicates in only one of the rankings:
#'   \deqn{\sigma^{2}_{S} = \frac{n(n-1)(2n+5) \times \sum_{t \in g_{e}}t(t-1)(2t+5)}{18}}
#'
#' @seealso \code{\link{ts_test}}
ts_variance <- function(n, u = rep(1, length.out = n)) {
  ((n * (n - 1) * (2 * n + 5)) - sum(u * (u - 1) * (2 * u + 5))) / 18
}

#' Calculate Theil-Sen Equivalent of Kendall's Tau
#'
#' @description Sen denotes this as \emph{U}. It's
#'   functionally equivalent to Kendall's Tau,
#'   however their calculations differ.
#'
#' @param t A vector of ranks (numeric, or coercible to numeric).
#'   For Time Series the observation dates.
#' @param n Length of ranking.
#' @param score Sen's Test statistic.
#'
#' @return
#' @export
#'
#' @seealso \code{\link{ts_test}}
ts_tau <- function(t = NULL, n, score) {
  t <- check_sequence(t, n)

  N <- onesided_sgn(t)

  (1 / sqrt(N * choose(n, 2))) * score
}

#' Test Sen-Slope
#'
#' @description Tests if Sen's slope is significant. It's effectively
#'   a wrapper around all functions in this package starting
#'   with \code{ts_}.
#'
#' @param t A vector of ranks (numeric, or coercible to numeric).
#'   For Time Series the observation dates.
#' @param Y A vector of ranks. For Time Series the values observed at each
#'   \emph{t}.
#' @param alpha_val Level of significance used for testing.
#'
#' @return Built in tests like the \code{t.test} use a class "htest".
#'   Hopefully this will be my return value/class as well.
#'
#' @export
#' @seealso \code{\link{ts_slope}} \code{\link{ts_variance}}
#'   \code{\link{ts_tau}} \code{\link{ts_score}}
ts_test <- function(t = NULL, Y, alpha_val = 0.05) {

}
