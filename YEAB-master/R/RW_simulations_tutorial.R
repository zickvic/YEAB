
# Basic equation
update_V <- function(value_i, # Initial or value until trial i-th
                     alpha, # learning parameters
                     beta,
                     lambda) {
  value_sum <- sum(value_i) # value at i-th trial
  prediction_error <- lambda - value_sum # prediction error
  value_delta <- alpha * beta * prediction_error # change in V strength
  value_i <- value_i + value_delta # update value
  value_i # return value
}


# # Set parameters
# # lambda have a different value every trial or the same
# lambda <- rep(1,50) # with 100 trials
# # number of trials
# trials <- length(lambda)
# # learning parameters
# alpha <- 0.3
# beta <- 0.3
# # Define a vector to store results
# V_t <- numeric(trials)

# # perform update_V()
# # over vector trials with size length(lambda)

# for(t in 2:trials){
#   V_t[t] <- update_V(value_i = V_t[t - 1],
#                      alpha = alpha,
#                      beta = beta,
#                      lambda = lambda[t-1])
# }

# # We can also obtain the percent change as the first derivative devided
# # by the original function: f'(x)/f(x)

# d_Vt <- diff(V_t)/diff(1:trials)
# pct_change <- abs(c(0,d_Vt)/V_t)

# # Plot results
# par(mgp=c(1.5,0.5,0),
#     cex.axis = 0.9,
#     mar=c(2.5,3,2.5,1)+0.1,
#     family = "sans")

# plot(V_t,
#      xlab = "Trial Number",
#      ylab = "V strength",
#      axes = F,
#      main = "Simple delay conditioning",
#      type = "b",pch=21,
#      bg  = "red")
# lines(pct_change,bg = "orange",type = 'b',pch = 21)
# axis(1, lwd=0, lwd.tick=0.8, tck=0.02,cex.axis=0.8)
# axis(2, lwd=0, lwd.tick=0.8, tck=0.02,cex.axis=0.8)
# legend("topright",
#        inset = c(0,0.4),
#        pt.bg = c("red","orange"),
#        pch = c(21,21),
#        legend = c("V","Change")
#        )
# box()



# # Define a slightly complex arrange of US using a binomial distribution

# # Number of 'trials'
# trials <- 50
# # US probability of 0.7
# # bin dist : pmf=nCk*(p^k)*(1-p)^(n-k)
# # where pmf is probability mass function
# set.seed(1245)
# lambda <- rbinom(n = trials, # how many 'experiments' (observations)
#                  size = 1, # at each experiment, how many times 'flip a coin'?
#                  # this is the maximum value it can reach
#                  prob = 0.7) # which p does have the coin for success?
# # mean value is size * prob... you'll see when plotting results

# V_t <- numeric(trials)
# for(t in 2:trials){
#   V_t[t] <- update_V(value_i = V_t[t - 1],
#                      alpha = alpha,
#                      beta = beta,
#                      lambda = lambda[t-1])
# }

# d_Vt <- diff(V_t)/diff(1:trials)
# pct_change <- abs(c(0,d_Vt)/V_t)

# par(mgp=c(1.5,0.5,0),
#     cex.axis = 0.9,
#     mar=c(2.5,3,2.5,1)+0.1,
#     family = "sans")

# plot(V_t,
#      xlab = "Trial Number",
#      ylab = "V strength",
#      axes = F,
#      main = expression(paste("Probabilistic;"," p(", lambda,") = 0.7")),
#      type = "b",pch=21,
#      ylim = c(0,1),
#      bg = "red")
# abline(h = 0.7,lty = 2)
# lines(pct_change,bg = "orange",type = 'b',pch = 21)
# # what is the value of the mean(V_t)
# abline(h = mean(V_t[13:50]), lty = 1, col = "blue")
# axis(1, lwd=0, lwd.tick=0.8, tck=0.02,cex.axis=0.8)
# axis(2, lwd=0, lwd.tick=0.8, tck=0.02,cex.axis=0.8)
# box()
# # Almost 0.7 after trial 13
# # If we perform this many many times, the  expected value
# # will approach the 'true' value.
# # Suposse we perform the same number of trials over ns sessions

# trials <- 40
# ns     <- 90
# V_t    <- numeric(trials)
# V_matrix <- matrix(NA,ncol = trials,nrow = ns)
# alpha <- 0.3
# beta <- 0.3

# for(s in 1:ns){
#   # Define lambda vector as a random variable with 0 and 1,
#   # with a prob = 0.7 probability to get 1 and 1 - 0.7
#   # to get a 0
#   lambda <- rbinom(n = trials, size = 1,prob = 0.7)
#   # Iterate over every lambda
#   for(t in 2:trials){
#     V_t[t] <- update_V(value_i = V_t[t - 1],
#                        alpha = alpha,
#                        beta = beta,
#                        lambda = lambda[t-1])
#   }
#   if(s == 1){
#     plot(V_t,
#          xlab = "Trial Number",
#          ylab = "V strength",
#          main = expression(paste("Probabilistic;"," p(", lambda,") = 0.7")),
#          type = "l",
#          axes = F,
#          ylim = c(0,1),
#          col = rgb(192/255,192/255,192/255,0.6))
#   } else {
#     lines(V_t,col = rgb(192/255,192/255,192/255,0.6))
#   }
#   V_matrix[s,] <- V_t
#   if(s == ns){
#     lines(colMeans(V_matrix),col = "red",lwd = 1.2)

#     d_Vt <- abs(diff(colMeans(V_matrix)))
#     pct <- c(0,d_Vt)/colMeans(V_matrix)
#     lines(pct_change,col = "orange",lwd = 1.2)

#     abline(h = 0.7,lty = 2)
#     abline(h = 0,lty = 1)
#     axis(1, lwd=0, lwd.tick=0.8, tck=0.02,cex.axis=0.8)
#     axis(2, lwd=0, lwd.tick=0.8, tck=0.02,cex.axis=0.8)
#     box()
#   }

# }

# # Plating with alpha and beta we see how fast the red line can approach
# # the lambda, the asymptotic value. We can see also that no matter how many
# # sessions we 'run' the experiment, V can be bigger (on average) than lambda.
# # Thus, RW model don't asume 'separated' sessions; trials occur from 1 to n,
# # so there is no memory loss (in principle). A better simulation would be one
# # on which the first value of session j is the last of session i. <- Take home message


# ### Conditioning + extinction

# trials <- 50    # 50 trials
# V_t <- numeric(trials) # vector of zeros
# lambda <- 1     # Again, the limiting value of conditioning
# alpha <- 0.3
# beta <- 0.3

# for(t in 2:trials) {

#   # remove the shock after trial 25
#   if(t > 25) lambda <- 0

#   # update associative strength on each trial
#   V_t[t] <-
#     update_V(
#       value = V_t[t-1],
#       lambda = lambda,
#       alpha,beta)
# }

# # plot command

# plot(V_t,
#      xlab = "Trial Number",
#      ylab = "V strength",
#      main = "Conditioning + extinction",
#      type = "b",pch=21,
#      axes = F,
#      bg = "red",
#      ylim = c(0,1))
# abline(v = 25,lty = 2,lwd = 1.1)
# rect(25, 0, 51, 1, col = rgb(0.5,0.5,0.5,1/4), lty = 1)
# text(10,0.9,"conditioning")
# text(35,0.9,"extinction")
# axis(1, lwd=0, lwd.tick=0.8, tck=0.02,cex.axis=0.8)
# axis(2, lwd=0, lwd.tick=0.8, tck=0.02,cex.axis=0.8)
# box()
# # Bloqueo

# trials <- 50
# # vectors of zeros
# Vt_A <- numeric(trials)
# Vt_B <- numeric(trials)
# lambda = 1
# # initially only A is present
# alpha <- c(.3, 0)

# for(t in 2:trials) {

#   # after trial 15, both stimuli are present
#   if(t > 15) alpha <- c(.3, .3)

#   # current associative strengths
#   V_i <- c(Vt_A[t - 1], Vt_B[t - 1])

#   # update both
#   V_j <-
#     update_V(
#       value = V_i,
#       alpha = alpha,
#       lambda = lambda,
#       alpha)

#   # record the updated strengths
#   Vt_A[t] <- V_j[1]
#   Vt_B[t] <- V_j[2]
# }


# # more elaborate plotting commands

# plot(Vt_A,
#      xlab = "Trial Number",
#      ylab = "V strength",
#      main = "Blocking",
#      type = "b",
#      pch = 21,
#      axes = F,
#      bg = "blue",
#      ylim = c(0,1))

# lines(
#   Vt_B,
#   pch = 21,
#   bg = "red",
#   type = "b"
# )

# # A + B
# lines(
#   Vt_B + Vt_A,
#   pch = 21,
#   bg = "purple",
#   type = "b",
#   cex = 0.6
# )
# legend("topright",inset = c(0,0.4),
#        pt.bg = c("purple","blue","red"),
#        pch = c(21,21,21),
#        legend = c("A+B","A","B"))
# abline(v = 15, lty = 2)
# axis(1, lwd=0, lwd.tick=0.8, tck=0.02,cex.axis=0.8)
# axis(2, lwd=0, lwd.tick=0.8, tck=0.02,cex.axis=0.8)
# box()
