# Data and parameters from previous parts
data <- c(0.66, 2.30, 1.98, 1.49, 0.62)
kappa <- 2
a <- 0
b <- 5
theta_values <- seq(a, b, length.out = 1000)
prior <- dunif(theta_values, min = a, max = b)

# Weibull likelihood func
likelihood_weibull <- function(x, theta) {
  if (theta <= 0) return(0)
  lambda <- sqrt(theta)
  return(dweibull(x, shape = kappa, scale = lambda))
}

# comopute and normalize the posterior
posterior <- sapply(theta_values, function(theta) {
  prod(sapply(data, likelihood_weibull, theta = theta))
}) * prior
posterior <- posterior / sum(posterior * diff(theta_values[1:2]))

# Number of simulations
n_sim <- 100000

# func to simulate from the Weibull distribution given theta
simulate_weibull <- function(theta) {
  lambda <- sqrt(theta)
  rweibull(1, shape = kappa, scale = lambda)
}

# Simulate failure times for the 6th component
set.seed(123)  # Setting seed for reproducibility
simulated_times <- numeric(n_sim)
for (i in 1:n_sim) {
  # Sample a theta value from the posterior distribution
  sampled_theta <- sample(theta_values, size = 1, prob = posterior)
  # Simulate a failure time from the Weibull distribution with the sampled theta
  simulated_times[i] <- simulate_weibull(sampled_theta)
}

# Create histogram of the simulated times
plot = hist(simulated_times, breaks = 50, main = "Histogram of Simulated Failure Times",
     xlab = "Failure Time", xlim = c(0, 5))
print(plot)

# Calculate the probability P(1 < X6 < 2 | x)
prob_between_1_and_2 <- mean(simulated_times > 1 & simulated_times < 2)
print(paste("P(1 < X6 < 2 | x) =", prob_between_1_and_2))

