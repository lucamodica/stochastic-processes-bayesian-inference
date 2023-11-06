library(ggplot2)

# Observed failure times
data <- c(0.66, 2.30, 1.98, 1.49, 0.62)

# Weibull log-likelihood function
log_likelihood_weibull <- function(x, theta) {
  kappa <- 2 # shape parameter
  # Prevent taking sqrt of negative number
  if (theta <= 0) {
    return(-Inf)
  }
  lambda <- sqrt(theta) # scale parameter as the square root of theta
  # Weibull log-likelihood for a vector of x and single theta
  log_likelihood <- log(kappa) - log(lambda) + (kappa - 1) * log(x) - (kappa - 1) * log(lambda) - (x / lambda)^kappa
  # Sum of log-likelihoods for all x due to independence
  sum(log_likelihood)
}

# Prior parameters for Unif(0, 5)
a <- 0
b <- 5
prior_unif <- function(theta) {
  ifelse(theta >= a & theta <= b, log(1 / (b - a)), -Inf) # log Uniform density function
}

# Discretize the parameter space for theta
theta_values <- seq(a, b, length.out = 1000) # 1000 points from 0 to 5

# Compute the unnormalized log-posterior for each theta value
log_posterior_unnormalized <- sapply(theta_values, function(theta) {
  log_likelihood_weibull(data, theta) + prior_unif(theta)
})

# Convert log-posterior to actual values avoiding underflow
max_log_posterior <- max(log_posterior_unnormalized, na.rm = TRUE)
posterior_unnormalized <- exp(log_posterior_unnormalized - max_log_posterior)

# Normalize the posterior so it sums to 1
posterior_normalized <- posterior_unnormalized / sum(posterior_unnormalized)

# Create a data frame for plotting
posterior_df <- data.frame(theta = theta_values, posterior = posterior_normalized)

# Plot the posterior
ggplot(posterior_df, aes(x = theta, y = posterior)) +
  geom_line() +
  labs(title = "Posterior distribution for θ", x = expression(theta), y = "Density") +
  theme_minimal()
