#' Find the nearest multiple
#'
#' @param x numeric, the value for which we want to finde a multiple
#' @param multiple numeric, the multiple
#'
#' @return the nearest multiple
#' @export
#'
#' @examples
#' ceiling_multiple(8, 10) # returns 10
#' ceiling_multiple(12, 10) # returns 20
#' ceiling_multiple(21, 11) # returns 22
ceiling_multiple <- function(x, multiple) {
  m <- x %% multiple
  residual <- multiple - m
  x + residual
}
