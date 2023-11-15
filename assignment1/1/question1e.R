library(ggplot2)
library(stats)

# Data and parameters
data <- c(0.66, 2.30, 1.98, 1.49, 0.62)
kappa <- 2  # shape parameter
a <- 0      # lower bound for uniform prior
b <- 5      # upper bound for uniform prior
theta_values <- seq(a, b, length.out = 1000)  # Discretize theta space

# Uniform prior
prior <- dunif(theta_values, min = a, max = b)

# Weibull likelihood func
likelihood_weibull <- function(x, theta) {
  if (theta <= 0) return(0)
  lambda <- sqrt(theta)  # Scale parameter
  return(dweibull(x, shape = kappa, scale = lambda))
}

# Compute the posterior for each theta and normalze the results
posterior <- sapply(theta_values, function(theta) {
  prod(sapply(data, likelihood_weibull, theta = theta))
}) * prior
posterior <- posterior / sum(posterior * diff(theta_values[1:2]))

# Plot the posterior
posterior_df <- data.frame(theta = theta_values, posterior = posterior)
plot = ggplot(posterior_df, aes(x = theta, y = posterior)) +
  geom_line() +
  labs(title = "Posterior distribution for θ", x = expression(theta), y = "Density") +
  theme_minimal()
print(plot)
