#' A function to binarize a numeric vector with a given resolution
#'
#' @param x numeric, the vector to be binarized
#' @param x_min numeric, the min value of a vector to create the bins (e.g., 0)
#' @param x_max numeric, the maximum value of the vector x to binarize
#' @param res numeric, the resolution; if x is time, res can be 1 s
#'
#' @return the vector of bins for which x is in
#' @export
#'
#' @examples
#' x <- 1:20
#' get_bins(x, 0, 20, 5)
#' # Returns
#' # [1]  5  5  5  5  5 10 10 10 10 10 15 15 15 15 15 20 20 20 20 20
#' # set.seed(10)
#' x <- runif(20, 0, 10)
#' get_bins(x, 0, 10, 0.5)
#' # Returns
#' # 1] 5.5 3.5 4.5 7.0 1.0 2.5 3.0 3.0 6.5 4.5 7.0 6.0 1.5 6.0 4.0 4.5 1.0 3.0 4.0 8.5
get_bins <- function(x, x_min, x_max, res) {
  # Create a sequence of intervals from whose value x will be binned, taking pairs of values in order
  # eg, (0, 10, 20) will form pairs of intervals of (0, 10], (10, 20] and (10, 20]
  seq_intervals <- seq(x_min, x_max, res)
  # Find intervals takes x and makes intervals according to the sequence above.
  # If x = 1, 4, 9, 10, 12 and seq_intervals = 0, 10, 20 (bins of 10)
  # this will return classes of 1 1 1 1 2. The last part, "* res", multiply the class intervals
  # the return the true bins, so  1 1 1 1 2 -> 10 10 10 10 20
  findInterval(x, seq_intervals, left.open = TRUE) * res
}
