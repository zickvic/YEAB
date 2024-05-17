#' Min-max normalization (also feature rescaling)
#'
#' @param x numeric, vector of values to rescale
#'
#' @return A numeric vector rescaled in the range \eqn{x' \in [0, 1]}
#' @export
#'
#' @examples
#' x <- 5:100
#' x_scaled <- unity_normalization(x)
#' x_scaled
unity_normalization <- function(x) {
  stopifnot(all(is.numeric(x)))

  x_min <- min(x)
  x_max <- max(x)

  (x - x_min) / (x_max - x_min)
}


#' Normalization (or rescaling) between arbitrary a and b
#'
#' @param x numeric
#' @param a numeric
#' @param b numeric
#'
#' @return A numeric vector rescaled in the range \eqn{x' \in [a, b]}
#' @export
#' @examples
#' x <- 5:100
#' a <- 0
#' b <- 1
#' x_scaled <- ab_range_normalization(x, a, b)
#' x_scaled
#' a <- 100
#' b <- 1000
#' x_scaled <- ab_range_normalization(x, a, b)
#' x_scaled
ab_range_normalization <- function(x, a, b) {
  stopifnot(all(is.numeric(x)))
  stopifnot(all(is.numeric(c(a,b))))
  a + (x - min(x)) * (b - a) / (max(x) - min(x))
}
