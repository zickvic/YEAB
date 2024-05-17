#' @title Biexponential model for the estimation of within and between bouts
#'
#' @param irt numeric, the inter-response times
#'
#' @return the simple biexponential returns numeric vector of the biexponential 
#' model parameters \eqn{L=1/\theta}, the bout length,
#' \eqn{W = 1/w}, the within bout rate, and \eqn{B = 1/b} the bout initiation rate, where
#' \eqn{\theta, w, b} are the proportion of responses that are bouts, the mean within bout IRT, and
#' the mean between-bout IRT.
#' The biexponential refractory model returns a data frame with the parameters \eqn{a, b, q} where
#' \eqn{a, b} are the mean within and between bout IRTs and \eqn{q} is the proportion of responses
#' that are bouts, and \eqn{\delta} is the refractory period. 
#' @export
#'
#' @details Implements the finite mixtures of two exponentials
#' \eqn{p(IRT < \tau) = \theta w e^{-w IRT} + (1-\theta)b e^{-b IRT}}
#'
#' and the biexponential refractory model, or berm, which is a special case of the biexponential model
#'
#' @examples
#' l1 <- 1/0.5
#' l2 <- 1/0.1
#' theta <- 0.5
#' irt <- c(
#'  rexp(100, l1),
#' rexp(100, l2)
#' )
#' biexponential(irt)
#' berm(irt, 0.1)
biexponential <- function(irt) {
  fit <- VGAM::vglm(irt ~ 1, VGAM::mix2exp, trace = TRUE)
  param <- VGAM::Coef(fit)
  params <- data.frame(
    bout_length = 1 / param[1],
    bout_initiation = 1 / param[2],
    bout_within = 1 / param[3]
  )
  return(params)
}

#' @title Biexponential refractory model, BERM
#' 
#' @description Implements the biexponential refractory model, or berm
#' \eqn{p(IRT < \tau) = (1-q)e^{-a(IRT-\delta)} + qe^{-b(IRT-\delta)}}
#' where \eqn{a, b} are the mean within and between bout IRTs and \eqn{q} is the proportion of responses
#' that are bouts, and \eqn{\delta} is the refractory period.
#' 
#' @param irt numeric, the inter-response times
#' @param delta numeric, the refractory period
#' 
#' @return a data frame with the parameters \eqn{a, b, q} where
#' \eqn{a, b} are the mean within and between bout IRTs and \eqn{q} is the proportion of responses
#' that are bouts, and \eqn{\delta} is the refractory period.
#' 
#' @export
#' 

berm <- function(irt, delta) {
  # Custom log-likelihood function for the biexponential refractory model
  log_likelihood <- function(params) {
    a <- params[1]
    b <- params[2]
    q <- params[3]

    # Probability calculation depending on the IRT value relative to delta
    p_irt <- ifelse(
      # if then
      irt < delta, 0,
      # else
      (1 - q) * exp(-a * (irt - delta)) + q * exp(-b * (irt - delta)))

    # Return the negative log-likelihood
    -sum(log(p_irt))
  }

  # Initial parameter estimates for the optimizer
  initial_params <- c(a = 1, b = 1, q = 0.5)

  # Use an optimizer to minimize the negative log-likelihood
  optim_res <- optim(par = initial_params, fn = log_likelihood,
    method = "L-BFGS-B", lower = c(0, 0, 0), upper = c(Inf, Inf, 1))

  # Extract the estimated parameters
  param_estimates <- optim_res$par

  # Return the estimated parameters as a data frame
  params_df <- data.frame(
    a = param_estimates[1],
    b = param_estimates[2],
    q = param_estimates[3]
  )

  return(params_df)
}
