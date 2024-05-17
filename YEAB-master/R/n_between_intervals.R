#' Find maximum value within intervals
#'
#' This function searches for the maximum value within a distribution
#' (represented by vector x) that falls within a series of intervals
#' specified by the vector intervals.
#'
#' @param x A numeric vector representing the distribution from which
#' to find the maximum value within intervals.
#' @param intervals A numeric vector specifying the intervals within
#' which to search for the maximum value.
#' @return A numeric vector containing the maximum value within each
#' interval specified by 'intervals'. If no values within an interval,
#' returns 0 for that interval.
#' @export
#' @importFrom dplyr lag
#' @importFrom dplyr between
#' @examples
#' # Create a vector of data with a logarithmically increasing distribution
#' data <- round(exp(seq(log(1), log(100), length.out = 100)))
#'
#' # Define intervals to cover the range 1-100
#' intervals <- seq(1, 100, by = 20)
#'
#' # Find maximum value within intervals
#' n_between_intervals(data, intervals)


n_between_intervals <- function(x, intervals, time_in) {
  # matrix of intervals min, max
  mat_x <- matrix(c(lag(intervals, 1), intervals), ncol = 2)
  mat_x <- mat_x[2:nrow(mat_x), ]
  rsp <- c()

  for (i in 1:nrow(mat_x)) {
    which_resp <- x[between(time_in, mat_x[i, 1], mat_x[i, 2])]
    if (length(which_resp)) {
      rtm <- max(which_resp)
      rsp <- c(rsp, rtm)
    } else {
      rsp <- c(rsp, 0)
    }
  }
  rsp
}
