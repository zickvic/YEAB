#' Frequency table for binned data
#' @description Creates a frequency table from a vector of bins from, for example,
#'    get_bins(). It includes zero-frequency bins. If the bins came from the
#'    responding times, this creates a data.frame of response rate.
#' @param x, numeric, a vector of binned data
#' @param min_x, numeric, the minimal value of x
#' @param max_x, numeric, the maximal value of x
#' @param bin_res, numeric, the bin resolution
#'
#' @return A data frame
#' @export
#'
#' @examples
#' data("r_times")
#' bin_res <- 2
#' min_x <- 0
#' max_x <- 180
#' x <- get_bins(r_times, min_x, max_x, bin_res)
#' xt <- f_table(x, min_x, max_x, bin_res)
#' plot(xt$bins, xt$prop)
f_table <- function(x, min_x, max_x, bin_res) {
  xf <- factor(x, levels = seq(min_x, max_x, bin_res))
  xt <- as.data.frame(table(xf))
  xt$prop <- xt$Freq / sum(xt$Freq)
  colnames(xt) <- c("bins", "freq", "prop")
  xt$bins <- as.numeric(as.character(xt$bins))
  xt
}
