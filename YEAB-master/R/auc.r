#' @title Area under the curve (AUC)
#' 
#' @description Calculate the area under the curve (AUC) using the trapezoid
#' method.
#' @param x A numeric vector of x values
#' @param y A numeric vector of y values
#' @return A numeric value of the area under the curve
#' @export
#' @examples
#' x_values <- c(0, 1, 2, 3, 4) # Delay times
#' y_values <- c(1, 0.8, 0.6, 0.4, 0.2) # Discounted values
#' auc_result <- trapezoid_auc(x_values, y_values)
#' print(paste("Area Under Curve: ", auc_result))

# Function to calculate AUC using trapezoid method
trapezoid_auc <- function(x, y) {
  dx <- diff(x)
  y_avg <- (y[-1] + y[-length(y)]) / 2
  auc <- sum(dx * y_avg)
  return(auc)
}
