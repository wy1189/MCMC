
# 0. parameter settings for simulation ------------------------------------

n_iteration <- 1000

# 1. Initialization -------------------------------------------------------


theta <- numeric(n_iteration)
theta[1] <- .5 # initialization theta
sigma <- 1 # arbiturary variance
proposed_sigma <- c(0.1, 1, 100)
denom <- dnorm(theta[1], 0, sigma)

# 2. Iteration! --------------------------------------------------------------
png('mh_result_diff_sig.png', width = 1024, height = 764)
par(mfcol = c(2,3))
for (j in 1:length(proposed_sigma)){
  for (i in 2:n_iteration){
    theta.star <- rnorm(1, theta[i - 1], proposed_sigma[j])
    num <- dnorm(theta.star, 0, sigma)
    alpha <- min(c(1, num/denom))
    if (runif(1) < alpha){
      theta[i] <- theta.star
      denom <- num
    } else {
      denom <- denom
      theta[i] <- theta[i - 1]
    }
  }
# 3. Visualization --------------------------------------------------------
  hist(theta, main = paste("Distribution of theta,", "Sigma:", as.character(proposed_sigma[j])), freq = F)
  plot(theta, type = "p", main = "Trace")
}
dev.off()
