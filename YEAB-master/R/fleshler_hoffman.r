#' @title Fleshler & Hoffman (1962) progression
#' @description This function calculates the values of intervals approximately for
#' an exponential distribution, but avoiding extremely large values.
#' 
#' @param N The total number of intervals.
#' @param VI The value of the Variable Interval
#'
#' @return A vector of calculated values for the intervals.
#'
#' @export
#' @details This function calculates the values of intervals approximately for
#' a exponential distribution, but avoiding extremely large values which can
#' produce extinction.
#' It uses the formula derived from the Fleshler & Hoffman article, where the
#' first factor of the equation is -log(1 - p)^(-1), representing the expected
#' value or mean of the intervals. This value is also the inverse of a Poisson
#' process, 1/lambda. Since we want the expected value or mean to be the value
#' of the IV, we replace that constant with VI.
#' The function handles the case when n = N, where the value becomes undefined 
#' (log(0)), by using L'Hopital's rule to find the limit of the function as n 
#' approaches N. The resulting values are then multiplied by the IV and the 
#' logarithm of N to obtain the final calculated values.
#'
#' @examples
#' # Calculate intervals for N = 10, and IV = 30
#' N <- 15
#' iv <- 90
#' intervals <- round(fleshler_hoffman(N,iv), 3)
#' # Plot the intervals and the exponential distribution corresponding to the
#' # same mean (IV)
#' hist(intervals, freq = F)
#' curve(dexp(x, rate = 1/iv), add = TRUE, col = 'red')
#' legend('topright', legend = c('F&H', 'Exponential'), lty = 1, col = c('black', 'red'))
#' @references
#' Fleshler, M., & Hoffman, H. S. (1962). A progression for generating variable-interval schedules.
#' Journal of the Experimental Analysis of Behavior, 5(4), 529-530.
#'
fleshler_hoffman <- function(N, VI) {
  n <- 1:N
  intervals <- rep(0, N)

  for (i in n[N > n]) {
    intervals[i] <- VI * (1 +
      log(N) + (N - n[i]) * log(N - n[i]) -
      (N - n[i] + 1) * log(N - n[i] + 1))
  }

  intervals[N] <- VI * (1 + log(N)) #

  return(intervals)
}


# Note 1: In the Fleshler & Hoffman article, the first factor
# of the equation, which we can call f(n), is -log(1 - p)^(-1),
# which is the expected value, or mean (technically E[intervals]),
# where p is the probability of reinforcement per second and log
# is the natural logarithm (base e, or Euler's number). This value
# is also the inverse of a Poisson process, 1/lambda.
# Since we want the expected value or mean to be
# the value of the IV, we replace that constant with iv_val.

# Note 2: When n = N the function f(n) becomes undefined, as there
# is no log(0), however, through calculus, this value can be found
# when n is infinitesimally close to N, that is
# finding the limit of f(n) as n -> N. This is achieved
# using L'Hopital's rule. The derivation is somewhat obtuse
# but it yields f(n = N) = -log(1 - p)^(-1) * (1 + log(N)),
# or equivalently, f(n = N) = (E[intervals]) * (1 + log(N)),
# where E[intervals]) is the value of the IV.

f_and_h <- function(N, n, iv_val) {

  intervals <- rep(0, N)

  for (i in n[N > n]) {
    intervals[i] <- iv_val * (1 +
      log(N) + (N - n[i]) * log(N - n[i]) -
      (N - n[i] + 1) * log(N - n[i] + 1))
  }

  intervals[N] <- iv_val * (1 + log(N)) #

  return(intervals)
}
