library(ggplot2)

# Data and parameters
data <- c(0.66, 2.30, 1.98, 1.49, 0.62)
kappa <- 2  # shape parameter
a <- 0      # lower bound for uniform prior
b <- 5      # upper bound for uniform prior
theta_values <- seq(a, b, length.out = 1000)  # Discretize theta space

# Uniform prior
prior <- rep(1 / (b - a), length(theta_values))

# Weibull likelihood function
likelihood_weibull <- function(x, theta) {
  if (theta <= 0) return(0)
  lambda <- sqrt(theta)  # Scale parameter
  (kappa / lambda) * (x / lambda)^(kappa - 1) * exp(-(x / lambda)^kappa)
}

# Compute the posterior for each theta
posterior <- sapply(theta_values, function(theta) {
  prod(sapply(data, likelihood_weibull, theta = theta))
}) * prior

# Normalize the posterior
posterior <- posterior / sum(posterior * diff(theta_values[1:2]))

# Create a data frame for plotting
posterior_df <- data.frame(theta = theta_values, posterior = posterior)

# Plot the posterior
ggplot(posterior_df, aes(x = theta, y = posterior)) +
  geom_line() +
  labs(title = "Posterior distribution for θ", x = expression(theta), y = "Density") +
  theme_minimal()
