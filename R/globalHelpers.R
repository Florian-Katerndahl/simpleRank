#' Calculate sum of signs of two vectors
#'
#' @description
#' Takes two vectors as input and returns summed product of the form
#' \eqn{\textrm{sign}(z_j - z_i) \* \textrm{sign}(t_j - t_i)}.
#'
#' @param t A vector of ranks.
#' @param z A vector of ranks.
#'
#' @return An Integer.
#' @export
#' 
#' @details \loadmathjax
#'   \mjsdeqn{\textrm{sgn}(x) =  
#'   \begin{cases} 1, &\textrm{if}\; x > 0 \\ 0, 
#'   &\textrm{if}\; x = 0 \\ -1, 
#'   &\textrm{if}\; x < 0 \end{cases}}
#'
#' @references
#' Kendall, M. G. (1975): Rank Correlation Methods. 4th Edition London,
#' High Wycombe: Charles Griffin & Company Ltd.
#'
#' @seealso \code{\link{onesided_sgn}}
sgn <- function(t, z) {
  comb_t <- utils::combn(t, 2)
  comb_z <- utils::combn(z, 2)
  sum(
    purrr::pmap_dbl(
      list(comb_t[, 2], comb_t[, 1], comb_z[, 2], comb_z[, 1]),
      ~ sign(..1 - ..2) * sign(..3 - ..4)
    )
  )
}

#' Calculate sum of signs of one vector
#'
#' @description
#' This fuction takes a vector of ranks as
#' input and returns the summed product of
#' the signs of \eqn{\binom {n} {2}} permutations.
#'
#' @param t A vector of ranks.
#'
#' @return An Integer.
#' @export
#'
#' @details
#' \code{onesided_sgn} operates like \code{\link{sgn}} but takes only
#' a single vector as input. In the context of the Kendall's Rank Correlation
#' Coefficient this assumes, that the provided vector represents entries in
#' their natural order.
#'
#' @references
#' Kendall, M. G. (1975): Rank Correlation Methods. 4th Edition London, High Wycombe: Charles Griffin & Company Ltd.
#'
#' @seealso \code{\link{sgn}}
onesided_sgn <- function(t) {
  comb_t <- utils::combn(t, 2)

  sum(
    purrr::map2_dbl(comb_t[, 2], comb_t[, 1]), function(t2, t1) sign(t2 - t1)
  )
}


#' Sequence Checking
#'
#' @param t A numeric vector of length n.
#' @param n A atomic numeric.
#'
#' @return A numeric vector.
check_sequence <- function(t = NULL, n) {
  if (!is.null(t)) {
    if (length(t) != n) {
      stop("While t was supplied: objects t and Y are not of same length.")
    }
    return(as.numeric(t))
  }
  seq(from = 1, to = n)
}
