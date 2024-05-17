#' @title Computes the Kullback-Leibler divergence based on kernel density estimates
#'
#' @description Computes the Kullback-Leibler divergence based on kernel density estimates
#' of two samples.
#' @param x numeric, the values from a sample p
#' @param y numeric, the values from a sample q
#' @param from_a numeric, the lower limit of the integration
#' @param to_b numeric, the upper limit of the integration
#'
#' @return a numeric value that is the kl divergence
#' @export
#'
#' @details
#' The Kullback-Leibler divergence is defined as
#' \deqn{D_{KL}(P||Q) = \int_{-\infty}^{\infty} p(x) \log \frac{p(x)}{q(x)} dx}
#'
#' @examples
#' set.seed(123)
#' p <- rnorm(100)
#' q <- rnorm(100)
#' KL_div(p, q, -Inf, Inf) # 0.07579204
#' q <- rnorm(100, 10, 4)
#' KL_div(p, q, -Inf, Inf) # 7.769912
KL_div <- function(x, y, from_a, to_b) {
  integrand <- function(x, y, t) {
    denx <- density(x, na.rm = T)
    deny <- density(y, na.rm = T)
    f.x <- approx(denx$x, denx$y, t)$y
    f.y <- approx(deny$x, deny$y, t)$y
    tmpRatio <- f.x * (log(f.x) - log(f.y))
    tmpRatio <- ifelse(is.infinite(tmpRatio), 0, ifelse(is.na(tmpRatio), 0, tmpRatio))
    tmpRatio
  }
  integrate(integrand, from_a, to_b, x = x, y = y, stop.on.error = FALSE)$value
}