#' Mutual information of continuous variables using discretization
#'
#' @param x A numeric vector
#' @param y A numeric vector or equal or unequal size as x
#' @param method The method to estimate entropy; available methods are "emp", "mm",
#' "shrink", "sg" (default:"emp"). See details
#' @details This function is based on the infotheo package. It uses equalfreq discretization by default.
#' x and y need not be of equal size.
#' @references
#' Meyer, P. E. (2008). Information-Theoretic Variable Selection and Network Inference from Microarray Data.
#' PhD thesis of the Universite Libre de Bruxelles.
#' @return
#' @export
#' @importFrom infotheo discretize
#' @importFrom infotheo mutinformation
#' @examples
#' set.seed(123)
#' x <- rnorm(1000)
#' y <- rnorm(1000)
#' plot(x, y)
#' # close to 0 if they are independent
#' mut_info_discret(x, y)
#' y <- 100 * x + rnorm(length(x), 0, 12)
#' plot(x, y)
#' # far from 0 if they are not independent
#' mut_info_discret(x, y)
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
#' mut_info_discret(x, y_with_noise)
mut_info_discret <- function(x, y, method = "emp") {
  xd <- discretize(x)
  yd <- discretize(y)
  mutinformation(xd, yd, method = method)
}
