#' Gellerman-like series
#'
#' @param n numeric, a vector of 0 and 1 (see Details)
#'
#' @return a numeric vector of randomly distributed 0s and 1s
#' @export
#'
#' @details The algorithm implements a Gellerman-like series based on
#' Herrera, D., & Treviño, M. (2015). http://doi.org/10.1371/journal.pone.0136084
#' The algorithm samples from a binomial distribution and imposes two restrictions
#' 1) no more than 3 consecutive values of 0s or 1s.
#' 2) the number of trials 0 or 1 must be the same for a given n.
#' @examples
#' set.seed(165)
#' gell_like(8) # 0 0 1 1 1 0 1 0
gell_like <- function(n) { # n debe ser un número par

  if (n %% 2 != 0) stop("n should be even (2, 4, ...")

  # get half the length of n
  half <- n / 2
  # vector of zeros of length n
  gell <- numeric(n)
  # fill first 3 positions
  gell[1:3] <- rbinom(3, 1, 0.5)
  # exit while when half the length n is 0s OR 1s
  while (sum(gell == 0) != half & sum(gell) != half) {
    for (i in 4:n) { # start at the fourth positionc
      gell[i] <- rbinom(1, 1, 0.5)
      # if the last 4 values sum 4 or 0 compute again the last value
      while (sum(gell[(i - 3):i]) == 4 | sum(gell[(i - 3):i]) == 0) {
        gell[i] <- rbinom(1, 1, 0.5)
      } # continue
    }
  }
  # return the vector

  gell
}
