
# Load required library
#install.packages('MCMCpack')
#install.packages('ggplot2')
#install.packages('stats')
library(MCMCpack)
library(ggplot2)
library(stats)

# Observed data
data <- c(0.66, 2.30, 1.98, 1.49, 0.62)

# Prior parameters for the inverse gamma distribution
alpha_prior <- 1.01
beta_prior <- 1

# Posterior parameters are updated based on the data
alpha_post <- alpha_prior + length(data)
beta_post <- beta_prior + sum(data^2)

# Generating a sequence of theta values for plotting
theta_values <- seq(0.01, 10, length.out = 1000)

# Calculating the posterior density for each theta value
posterior_density <- dinvgamma(x = theta_values, shape = alpha_post, scale = beta_post)

# Creating a data frame for plotting
posterior_data <- data.frame(theta = theta_values, density = posterior_density)

# Plotting the posterior
posterior_plot <- ggplot(posterior_data, aes(x = theta, y = density)) +
  geom_line(color = 'blue') +
  labs(title = 'Posterior Distribution for Theta', x = 'Theta', y = 'Density') +
  theme_minimal()

print(posterior_plot)
