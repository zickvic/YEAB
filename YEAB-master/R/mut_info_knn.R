#' Mutual information for continuous variables using kNN
#'
#' @param x, numeric
#' @param y, numeric
#' @param method, available methods described by Kraskov et al (2004)
#' @param k, number of nearest neighbors to search; by default 5
#'
#' @return an estimate of the mutual information
#' @export
#' @importFrom rmi knn_mi
#'
#' @details Uses the knn_mi implementation from the rmi package. The methods available are KSG1 and KSG2.
#' @examples
#'
#' set.seed(123)
#' x <- rnorm(1000)
#' y <- rnorm(1000)
#' plot(x, y)
#' # close to 0 if they are independent
#' mut_info_knn(x, y, method = "KSG1", k = 5)
#' y <- 100 * x + rnorm(length(x), 0, 12)
#' plot(x, y)
#' # far from 0 if they are not independent
#' mut_info_knn(x, y, method = "KSG1", k = 5)
#' # simulate a sine function with noise
#' set.seed(123)
#' x <- seq(0, 5, 0.1)
#' y <- 5 * sin(x * pi)
#' y_with_noise <- y + rnorm(length(x), 0, 1)
#' plot(x, y_with_noise)
#' lines(x, y, col = 2)
#' # add a regression line
#' abline(lm(y ~ x))
#' # compute correlation coefficient; for nonlinear functions is close to 0
#' cor(x, y_with_noise)
#' # mutual information can detect nonlinear dependencies
#' mut_info_knn(x, y_with_noise, method = "KSG1", k = 2)
mut_info_knn <- function(x, y, method, k = 5) {
  knn_mi(cbind(x, y), c(1, 1),
    options = list(method = method, k = k)
  )
}
