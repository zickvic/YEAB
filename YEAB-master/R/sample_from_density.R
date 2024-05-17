#' Sample from a density estimate
#'
#' @param x A numeric variable from a (un)known distribution
#' @param n Number of samples to return
#'
#' @return A sample with distribution close to x
#' @export
#'
#'
#' @examples TODO
sample_from_density <- function(x, n) {
  # pdf_x <- density(x, n = n^2)
  pdf_x <- ks::kde(x, gridsize = n * 2)
  # pdf_x$y <- pdf_x$estimate
  # pdf_x$xev <- pdf_x$eval.points
  approx(
    cumsum(pdf_x$estimate)/sum(pdf_x$estimate),
    pdf_x$eval.points,
    runif(n)
  )$y
}
#
# library(foreach)
# library(doParallel)
# library(parallel)
# # Use the detectCores() function to find the number of cores in system
# no_cores <- detectCores() - 10
# registerDoParallel(makeCluster(no_cores))
#
# x <- rnorm(100)
#
# res <- foreach(1:10000, .combine = c,
#                .packages=c("infotheo", "YEAB"))  %dopar% {
#    x2 <- sample_from_density(x, 100)
#    KL_div(x, x2, min(c(x, x2)), max(c(x, x2)))
#                }
#
# hist(res)
