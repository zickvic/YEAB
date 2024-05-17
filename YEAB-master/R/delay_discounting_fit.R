#' Hyperbolic function
#' @description An hyperbolic function to simulate delay discounting data
#' @param k numeric constant, the delay discounting parameter
#' @param delay vector of delays
#'
#' @return A numeric vector of subjective values between 0 and 1
#' @export
#'
#' @examples
#' delay <- seq(0, 10, len = 100)
#' k <- 0.2
#' sv <- eq_hyp(k, delay)
#' plot(delay, sv,
#'   xlab = "delay",
#'   ylab = "Sv",
#'   type = "l"
#' )
eq_hyp <- function(k, delay) {
  1 / (1 + k * delay)
}

#' Hyperbolic fit with nls
#'
#' @param value A numeric vector of the subjective values (indifference points)
#' @param delay A numeric vector of the delays used
#' @param initial_guess A numeric value providing an initial start for k
#' @param max_iter Positive integer with maximum number of iterations
#' @param scale_offset A constant to be added if the residuals are close to 0.
#'  This is to avoid division by 0, which is know to cause problems of convergence.
#' @return An object of class nls
#' @export
#'
#' @examples
#' # Simulated data with k = 0.5
#' data("hyp_data_list")
#' delay <- hyp_data_list$delay
#' sv <- hyp_data_list$sv
#' real_k <- hyp_data_list$real_k
#' model_hyp <- hyperbolic_fit(sv, delay, initial_guess = 0.01)
#' summary(model_hyp)
#' k_est <- coef(model_hyp)
#' k_est
#' # plot real and estimated sv
#' delay_real <- seq(0, max(delay), len = 100)
#' # first, simulate how the data should look with the real k
#' real_sv <- eq_hyp(real_k, delay_real)
#' # simulate estimated fitting line
#' est_sv <- eq_hyp(k_est, delay_real)
#' par(las = 1)
#' plot(
#'   delay, sv,
#'   pch = 21,
#'   col = 1,
#'   bg = 8,
#'   xlab = "Delay",
#'   ylab = "Subjective value"
#' )
#' lines(
#'   delay_real,
#'   est_sv,
#'   col = "red",
#'   lwd = 2
#' )
#' # real data
#' lines(
#'   delay_real,
#'   real_sv,
#'   type = "l",
#'   col = "blue",
#'   lwd = 2
#' )
#' legend(
#'   'topright',
#'   legend = c('data', 'real', 'fit'),
#'   text.col = "white",
#'   pch = c(21, NA, NA),
#'   col = c(1, NA, NA),
#'   pt.bg = c(8, NA, NA),
#'   bty = "n"
#' )
#' legend(
#'   'topright',
#'   legend = c('data', 'real', 'fit'),
#'   pch = c(NA, NA, NA),
#'   lty = c(NA, 1, 1),
#'   col = c(NA, 'blue', 'red'),
#'   bty = "n"
#' )
#'
#' # Now an example with real data
#' data('DD_data')
#' # first, fit a linear model
#' lineal_m <- lm(norm_sv ~ Delay, data = DD_data)
#' # hyperbolic model
#' hyp_m <- hyperbolic_fit(DD_data$norm_sv, delay = DD_data$Delay, 0.1)
#' # exponential model
#' exp_m <- exp_fit(DD_data$norm_sv, delay = DD_data$Delay, 0.1)
#' AIC(lineal_m, hyp_m, exp_m)
#' # compare visually
#' k_hyp <- coef(hyp_m)
#' k_exp <- coef(exp_m)
#' k_lin <- coef(lineal_m)
#' delay_vec <- seq(0, max(DD_data$Delay), len = 200)
#' plot(
#'   DD_data$Delay,
#'   DD_data$norm_sv,
#'   ylim = c(0, 1),
#'   pch = 21,
#'   ylab = "SV",
#'   xlab = "Delay",
#'   bg = "orange",
#'   col = "black"
#' )
#' lines(
#'   delay_vec,
#'   eq_hyp(k = k_hyp, delay_vec),
#'   col = 'green4',
#'   lwd = 2
#' )
#' lines(
#'   delay_vec,
#'   exp(-k_exp * delay_vec),
#'   col = 'steelblue',
#'   lwd = 2
#' )
#' abline(lineal_m, lty = 2, lwd = 2)
#'
#' legend(
#'  'topright',
#'  legend = c('data', 'exp fit', 'hyp fit', 'linear fit'),
#'  text.col = "white",
#'  pch = c(21, NA, NA, NA),
#'  col = c(1, NA, NA, NA),
#'  pt.bg = c('orange', NA, NA, NA),
#'  bty = "n"
#' )
#' legend(
#'  'topright',
#'  legend = c('data', 'exp fit', 'hyp fit', 'linear fit'),
#'  pch = c(NA, NA, NA, NA),
#'  lty = c(NA, 1, 1, 2),
#'  col = c(NA, 'steelblue', 'green4', 1),
#'  bty = "n"
#' )
#' # plot AIC values
#' aic_val <- AIC(lineal_m, hyp_m, exp_m) |> round(2)
#' leg <- sprintf(paste(rownames(aic_val), "= %s", sep = ' '), aic_val$AIC)
#' legend(
#'   'bottomleft',
#'   title = 'AIC\n(the smaller, the better)',
#'   legend = leg,
#'   bty = "n"
#' )
hyperbolic_fit <- function(value, delay, initial_guess, max_iter = 1e5, scale_offset = 0) {
  if (!all(is.numeric(value), is.numeric(delay), is.numeric(initial_guess))) {
    stop("Arguments should be numeric")
  }
  form <- value ~ 1 / (1 + k * delay)
  nls_control <- nls.control(maxiter = max_iter, scaleOffset = scale_offset)
  nls(form,
    start = list(k = initial_guess),
    control = nls_control
  )
}

#' Exponential fit with nls
#'
#' @param value A numeric vector of the subjective values (indifference points)
#' @param delay A numeric vector of the delays used
#' @param initial_guess A numeric value providing an initial start for k
#'
#' @return An object of class nls
#' @export
#'
#' @examples
#' See the examples of ?hyp_fit
exp_fit <- function(value, delay, initial_guess, max_iter = 1e5, scale_offset = 0) {
  if (!all(is.numeric(value), is.numeric(delay), is.numeric(initial_guess))) {
    stop("Arguments should be numeric")
  }
  form <- value ~ exp(-k * delay)
  nls_control <- nls.control(maxiter = max_iter, scaleOffset = scale_offset)
  nls(form,
    start = list(k = initial_guess),
    control = nls_control
  )
}
