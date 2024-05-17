#' @title Peak individual trial analysis using moving average
#'
#' @param tasa_norm, numeric
#' @param bines, numeric
#'
#' @return a list with
#'  params: a numeric vector with start, stop, spread and argmax (the bin at which response rate is max)
#'  mov_av: the moving average
#' @export
#' @importFrom zoo rollapply
#' @details Based on Balci et al 2010
#' @examples
#' data("r_times")
#' # binarize r_times to create response rate at 2 sec bins
#' bines <- get_bins(r_times, 0, 180, 2)
#' bin_res <- 6
#' tasa <- f_table(bines, 0, 180, bin_res)
#' tasa_norm <- tasa$prop / max(tasa$prop)
#' bines <- tasa$bins
#' balci_ind <- balci2010(tasa_norm, bines)
#' par(las = 1)
#' plot(bines, tasa_norm, xlab = "6 sec bins", )
#' lines(bines, balci_ind$mov_av, col = "blue", lwd = 2)
#' abline(v = balci_ind$params[c(1, 2, 4)], lwd = c(1, 1, 2), col = c(1, 1, "red4"))
balci2010 <- function(tasa_norm, bines) {
  mov_av <- zoo::rollapply(tasa_norm,
    width = 3,
    FUN = mean,
    align = "center",
    partial = TRUE
  )

  max_rnorm <- max(mov_av)
#  binmax <- bines[mov_av == max_rnorm]

  binmax <- bines[which.max(mov_av)]

  start <- bines[bines < binmax & mov_av >= 0.7][1]
  stop <- bines[bines > binmax & mov_av < 0.7][1]
  spread <- stop - start

  list(params = c(start, stop, spread, binmax), mov_av = mov_av)
}
