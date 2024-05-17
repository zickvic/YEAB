#' @param bw_adjust numeric, bandwidth adjustment factor, default is 1
dkl <- function(x, y, from_a, to_b, bw_adjust = 1) {
  # Bandwidth selection
  bw_x <- bw.nrd0(x) * bw_adjust
  bw_y <- bw.nrd0(y) * bw_adjust
  
  # Density estimation with boundary correction if necessary
  denx <- density(x, from = from_a, to = to_b, adjust = bw_x, na.rm = TRUE)
  deny <- density(y, from = from_a, to = to_b, adjust = bw_y, na.rm = TRUE)
  
  # Integration using precomputed KDE
  integrand <- function(t) {
    f.x <- approx(denx$x, denx$y, t)$y
    f.y <- approx(deny$x, deny$y, t)$y
    f.y <- pmax(f.y, .Machine$double.eps)  # Avoid division by zero
    tmpRatio <- f.x * (log(f.x) - log(f.y))
    tmpRatio <- ifelse(is.finite(tmpRatio), tmpRatio, 0)
    tmpRatio
  }
  
  integrate(Vectorize(integrand), lower = from_a, upper = to_b, stop.on.error = TRUE)$value
}

set.seed(123)
p <- rnorm(100)
q <- rnorm(100)
KL_div(p, q, -Inf, Inf) # 0.07579204
q <- rnorm(100, 10, 4)
KL_div(p, q, -Inf, Inf) # 7.769912

dkl(p, q, min(c(p,q)),max(c(p,q)), 1) # 0.07579204

dkl_mc <- function(x, y, N = 10000, bw_adjust = 1) {
  # Sample from P (assuming x is from P)
  samples <- sample(x, N, replace = TRUE)
  
  # Bandwidth selection
  bw_x <- bw.nrd0(x) * bw_adjust
  bw_y <- bw.nrd0(y) * bw_adjust
  
  # Density estimation with boundary correction if necessary
  denx <- density(x, adjust = bw_x, na.rm = TRUE)
  deny <- density(y, adjust = bw_y, na.rm = TRUE)
  
  # Create spline interpolation functions for the KDEs
  fx <- splinefun(denx$x, denx$y)
  fy <- splinefun(deny$x, deny$y)
  
  # Compute the densities at the sample points using the spline functions
  px <- fx(samples)
  qx <- fy(samples) + .Machine$double.eps
    # Compute the densities at the sample points
  # px <- approxfun(denx$x, denx$y)(samples)
  # qx <- approxfun(deny$x, deny$y)(samples) + .Machine$double.eps
  
  # Calculate the Monte Carlo estimate of the KL divergence
  kld_estimate <- mean(px * log(px / qx))
  
  return(kld_estimate)
}
set.seed(123)
p <- rnorm(100)
q <- rnorm(100)

kld_mc_spline(p, q, N = 1e6) 
KL_div(p, q, min(p,q), max(p,q)) # 0.07579204

q <- rnorm(100, 10, 4)

kld_mc_spline(p, q) 
KL_div(p, q, min(p,q), max(p,q)) # 0.07579204
