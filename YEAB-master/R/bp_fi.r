#' @title Single breakpoint algorithm, the exhaustive version as the one used in
#' Guilhardi & Church 2004
#'
#'
#' @param r_times numeric, the times at which a response was emitted in a trial
#' @param trial_duration numeric, the duration of the IF interval
#' @param optim_method character, the optimization method to use
#' @param params numeric, initial values for the parameter `bp`
#'
#' @return A data frame of 3 columns
#' `bp` a numeric value which corresponds to the time at which a break point was detected
#' `r1` a numeric value of the response rate _before_ the breakpoint
#' `r2` a numeric value of the responser rate _after_ the breakpoint
#' `d1` a numeric value of the duration of the first state
#' `d2` a numeric value of the duration of the second state
#'
#' @details This algorithm performs an extensive search of every combination (t1, t2)
#'   where t1 starts in the first response through (length(r_times) - 1)
#'
#' @export
#'
#' @examples
#' data("r_times")
#' r_times <- r_times[r_times < 60]
#' single_bp <- exhaustive_sbp(r_times, 60)
#' par(las = 1)
#' plot(r_times, seq_along(r_times),
#'   xlim = c(0, max(r_times)),
#'   main = "Cummulative Record",
#'   xlab = "Time (s)",
#'   ylab = "Cum Resp",
#'   col = 2, type = "s"
#' )
#' abline(v = single_bp$bp)
#'
#' bp_from_opt <- bp_opt(r_times, 60)
#' abline(v = bp_from_opt$bp, col = 3)
exhaustive_sbp <- function(r_times, trial_duration) {
  trial_duration <- max(max(r_times), trial_duration)

  nr <- length(r_times)
  r <- nr / trial_duration
  A <- numeric(nr - 1)

  for (j in 1:(nr - 1)) {
    t1 <- r_times[j]
    t2 <- trial_duration - t1
    r1 <- sum(r_times <= t1) / t1
    r2 <- sum(r_times > t2) / t2
    A[j] <- sum(t1 * abs(r1 - r), t2 * abs(r2 - r), na.rm = T)
  }

  argmax_A <- which.max(A)
  bp <- r_times[argmax_A]
  t1 <- bp
  t2 <- trial_duration - t1
  r1 <- sum(r_times <= bp) / bp
  r2 <- sum(r_times > bp) / (trial_duration - bp)

  data.frame(bp = bp, r1 = r1, r2 = r2, d1 = t1, d2 = t2)
}

#' @title Objective function for the breakpoint optimization algorithm
#'
#' @description Objective function for the breakpoint optimization algorithm
#' for fixed interval trials. This function is used by `optim` to find the
#' optimal breakpoint. Do not call this function directly.
#'
#' @param param A numeric value of the breakpoint
#' @param r_times A numeric vector of response times
#' @param trial_duration A numeric value of the trial duration
#' @return A numeric value representing the sum of areas between the response
#' rate and the target rate.
#' @export

objective_bp <- function(param, r_times, trial_duration) {
  nr <- length(r_times)
  r <- nr / trial_duration
  bp <- param

  t1 <- bp
  t2 <- trial_duration - bp
  r1 <- if (t1 > 0) sum(r_times <= bp) / t1 else 0
  r2 <- if (t2 > 0) sum(r_times > bp) / t2 else 0

  # Calculate the sum of areas
  sum_of_areas <- t1 * abs(r1 - r) + t2 * abs(r2 - r)

  # Return negative of the sum because we are maximizing
  return(-sum_of_areas)
}

#' @title Find the best fit for individual trials using `optim`
#'
#' @description Find the best fit for individual trials by minimizing the
#' negative sum of areas between the response rate and the target rate.
#'
#' @param r_times Vector of response times
#' @param trial_duration Duration of the trial
#'
#' @return A data frame with the following columns:
#' - `bp`: The breakpoint
#' - `r1`: The response rate before the breakpoint
#' - `r2`: The response rate after the breakpoint
#' - `d1`: The duration of the first state
#' - `d2`: The duration of the second state
#' @export
#' 
#' @examples
#' data("r_times")
#' r_times <- r_times[r_times < 60]
#' bp_from_opt <- bp_opt(r_times, 60)
#' plot(r_times, seq_along(r_times),
#'   xlim = c(0, max(r_times)),
#'   main = "Cummulative Record",
#'   xlab = "Time (s)",
#'   ylab = "Cum Resp",
#'   col = 2, type = "s"
#' )
#' abline(v = bp_from_opt$bp)
bp_opt <- function(r_times, trial_duration) {
  trial_duration <- max(max(r_times), trial_duration)

  # Initial guess for the breakpoint
  initial_guess <- median(r_times)
  # bounds
  lower_bound <- min(r_times)
  upper_bound <- max(r_times)
  # Optimization
  result <- optim(par = initial_guess,
    fn = objective_bp,
    r_times = r_times,
    trial_duration = trial_duration,
    lower = lower_bound,
    upper = upper_bound,
    method = "Brent")

  # Extract the optimized breakpoint
  bp <- result$par
  t1 <- bp
  t2 <- trial_duration - bp
  r1 <- sum(r_times <= bp) / bp
  r2 <- sum(r_times > bp) / t2

  data.frame(bp = bp, r1 = r1, r2 = r2, d1 = t1, d2 = t2)
}
