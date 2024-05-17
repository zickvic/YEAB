#' Gaussian + ramp fit with LM algorithm
#'
#' @param responses numeric, vector of response or response rate
#' @param time numeric, time bins
#' @param par a list of parameters for the gaussian + linear;
#'   see Buhusi, C. V., Perera, D., & Meck, W. H. (2005) for an explanation
#' @param max.iter numeric, max number of iterations
#' @details Ver Buhusi, C. V., Perera, D., & Meck, W. H. (2005). Memory for timing visual and auditory signals in albino and pigmented rats.
#' @return a numeric vector of coefficients, the same as the par argument
#' @export
#' @importFrom minpack.lm nls.lm
#' @details This algorithm uses the nonlinear least squares nls.lm (Levenberg–Marquardt) from the minpack.lm package
#'
#' @examples
#' # Function to create synthetic data
#' g_plus_lin <- function(par, time) {
#'   par$a * exp(-0.5 * ((time - par$t0) / par$b)**2) + par$c * (time - par$t0) + par$d
#' }
#' # real params
#' pars <- list(a = 20, t0 = 20, b = 10, c = 0.2, d = 1)
#' # time vector for simulation
#' ti <- seq(0, 60, 0.1)
#' # time vector for sampling with 2 sec of resolution
#' ti_data <- seq(0, 60, 2)
#' # r(t) real
#' y_curva <- g_plus_lin(par = pars, ti)
#' # r(t) sampled with noise
#' y_data <- g_plus_lin(par = pars, ti_data) + rnorm(length(ti_data), 0, sd = 2)
#' # param estimation
#' par_est <- gaussian_fit(responses = y_data, t = ti_data, par = pars, max.iter = 10500)
#' par_est
#' # fitted curve
#' y_hat <- g_plus_lin(par_est |> as.list(), ti)
#' # plot results
#' par(
#'   las = 1,
#'   mgp = c(2, 0.5, 0),
#'   tck = -0.015
#' )
#'
#' plot(ti,
#'   y_curva,
#'   type = "l",
#'   col = "blue",
#'   lwd = 2,
#'   ylim = c(0, max(y_curva, y_data)),
#'   xlab = "Time in trial",
#'   ylab = "R(t)",
#' )
#' points(ti_data, y_data, pch = 21, bg = "red", cex = 1.2)
#' lines(
#'   ti,
#'   y_hat,
#'   col = "green2",
#'   lwd = 2
#' )
#' legend(
#'   "topright",
#'   legend = c("real", "real + noise", "ajuste nls.lm"),
#'   lty = c(1, 0, 1),
#'   pch = c(NA, 21),
#'   pt.bg = c(NA, "red"),
#'   col = c("blue", 1, "green2"),
#'   pt.cex = 0.9,
#'   cex = 0.6
#' )
gaussian_fit <- function(responses,
                         time,
                         par = list(
                           a = 0.1,
                           d = 0.1,
                           t0 = 18, # valor próximo a IF
                           b = 10,
                           c = 1
                         ),
                         max.iter = 500) {
  g_plus_lin <- function(par, time) {
    par$a * exp(-0.5 * ((time - par$t0) / par$b)**2) + par$c * (time - par$t0) + par$d
  }

  # objective function observed-predicted
  res_fun <- function(responses, par, time) {
    responses - g_plus_lin(par, time)
  }

  # Ajuste usando Levenberg–Marquardt
  nlm_F <- nls.lm(
    par = par,
    fn = res_fun,
    responses = responses,
    time = time,
    control = nls.lm.control(maxiter = max.iter)
  )
  coef(nlm_F)
}
