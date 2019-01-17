
# 0. parameter settings for simulation ------------------------------------

n_iteration <- 10000

# 1. Initialization -------------------------------------------------------


theta <- numeric(n_iteration)
theta[1] <- .5 # initialization theta
sigma <- 5 # arbiturary variance
denom <- dnorm(theta[1], 0, sigma)

# 2. Iteration! --------------------------------------------------------------


for (i in 2:n_iteration){
  theta.star <- rnorm(1, theta[i - 1], sigma)
  num <- dnorm(theta.star, 0, sigma)
  alpha <- min(c(1, num/denom))
  if (rbinom(1, 1, alpha) == 1){
    theta[i] <- theta.star
    denom <- num
  } else {
    denom <- denom
    theta[i] <- theta[i - 1]
  }
}

# 3. Visualization --------------------------------------------------------


png('mh_result.png', width = 1024, height = 764)
par(mfrow = c(2, 1))
hist(theta, main = "Distribution of theta")
plot(theta, type = "l", main = "Trace")
dev.off()