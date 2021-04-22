#' Calculate Mann-Kendall Score
#'
#' @param t A vector of ranks (numeric, or coercible to numeric).
#'   For Time Series the observation dates.
#' @param Y A vector of ranks. For Time Series the values observed at each
#'   \emph{t}.
#'
#' @export
#'
#' @details \loadmathjax
#'   \mjsdeqn{S = \sum_{i = 1}^{n - 1}\sum_{j = i + 1}^{n} \textrm{sgn}(X_{j} - X_{i})}
#'
#' @seealso \code{\link{mk_test}}
mk_score <- function(t = NULL, Y) {
  N <- length(Y)
  t <- check_sequence(t, N)

  sgn(t, Y)
}

#' Calculate Variance of Mann-Kendall Test Statistic
#'
#' @param n Length of ranking/time series.
#' @param t Contingency table of ranking \mjseqn{R_{t}}. If \code{NULL} (the default),
#'   assumes that no duplications are present.
#' @param u Contingency table of ranking \mjseqn{R_{u}}. If \code{NULL} (the default),
#'   assumes that no duplications are present.
#'
#' @return Variance of Kendall's test statistic
#' @export
#'
#' @details \loadmathjax \mjsdeqn{\sigma_S^2 = \frac{(n(n - 1)(2n + 5) -
#' \sum_{t \in g_t}t(t - 1)(2t + 5) - \sum_{u \in g_u}
#' u(u - 1)(2u + 5)) + \frac{1}{9n(n-1)(n-2)} \times
#' \sum_{t \in g_t} t(t-1)(t-2) \times \sum_{u \in g_u} u(u-1)(u-2) +
#' \frac{1}{2n(n-1)} \times \sum_{t \in g_t} t(t-1) \times
#' \sum_{u \in g_u} u(u-1)}{18}}
#' @seealso \code{\link{mk_test}}
mk_variance <- function(n, t = NULL, u = NULL) {
  t <- check_sequence(t, n)
  u <- check_sequence(u, n)

  (((n * (n - 1) * (2 * n + 5)) - sum(t * (t - 1) * (2 * t + 5)) - sum(u * (u - 1) * (2 * u + 5))) +
    ((1 / (9 * n * (n - 1) * (n - 2))) * sum(t * (t - 1) * (t - 2)) * sum(u * (u - 1) * (u - 2))) +
    ((1 / (2 * n * (n - 1))) * sum(t * (t - 1)) * sum(u * (u - 1)))) / 18
}


#' Calculate Kendall's Tau
#'
#' @param score Man Kendall Score, see \code{\link{mk_score}}.
#' @param n Length of ranking/time series.
#' @param t Contingency table of ranking \mjseqn{R_{t}}. If \code{NULL} (the default),
#'   assumes that no duplications are present.
#' @param u Contingency table of ranking \mjseqn{R_{u}}. If \code{NULL} (the default),
#'   assumes that no duplications are present.
#'
#' @return A named vector (Tau a or Tau b, depending on the input).
#' @export
#'
#' @details \loadmathjax
#'   \mjsdeqn{\tau_{a} = \frac{S}{\frac{1}{2}n(n-1)} = \frac{S}{\binom{n}{2}}}
#'   \mjsdeqn{\tau_{b} = \frac{S}{\sqrt{\frac{1}{2}n(n-1) - \frac{1}{2}
#'   \sum_{t \in g_t} t(t-1)} \times
#'   \sqrt{\frac{1}{2}n(n-1) - \frac{1}{2}\sum_{u \in g_u} u(u-1)}}}
#' @seealso \code{\link{mk_test}}
mk_tau <- function(score, n, t = NULL, u = NULL) {
  if (is.null(t) & is.null(u)) {
    result <- score / choose(n, 2)

    names(result) <- "Tau a"

    return(result)
  } else {
    t <- check_sequence(t)
    u <- check_sequence(u)
  }

  result <- score / (sqrt(choose(n, 2) - (0.5 * sum(t * (t - 1)))) * sqrt(choose(n, 2) - (0.5 * sum(u * (u - 1)))))

  names(result) <- "Tau b"

  return(result)
}

#' Mann-Kendall Test
#'
#' @param t A vector of ranks (numeric, or coercible to numeric).
#'   For Time Series the observation dates.
#' @param Y A vector of ranks. For Time Series the values observed at each
#'   \emph{t}.
#' @param alpha_val Level of significance used for testing.
#' @param alternative What should \mjseqn{H_{1}} be?
#'
#' @return Built in tests like the \code{t.test} use a class "htest".
#'   Hopefully this will be my return value/class as well.
#' @export
#' @seealso \code{\link{mk_score}} \code{\link{mk_variance}}
#'   \code{\link{mk_tau}}
mk_test <- function(t = NULL, Y, alpha_val = 0.05, alternative = c("two.sided", "less", "greater")) {

}

# Calculate standardized test statistic
mk_statisitc <- function(score, variance) {
  if (score > 0) {
    return((score - 1) / sqrt(variance))
  } else if (score < 0) {
    return((score + 1) / sqrt(variance))
  } else {
    return(0)
  }
}
