library(MCMCpack)
library(ggplot2)
library(stats)

data <- c(0.66, 2.30, 1.98, 1.49, 0.62)

# prior parameters
alpha_prior <- 1.01
beta_prior <- 1

# posterior parameters are updated based on the data
alpha_post <- alpha_prior + length(data)
beta_post <- beta_prior + sum(data^2)

# compute the posterior density for each theta value
# with a generated sequence of theta values
theta_values <- seq(0.01, 10, length.out = 1000)
posterior_density <- dinvgamma(x = theta_values, shape = alpha_post, scale = beta_post)

# create a data frame for plotting and plot the posterior itself
posterior <- data.frame(theta = theta_values, density = posterior_density)
plot <- ggplot(posterior, aes(x = theta, y = density)) +
  geom_line(color = 'blue') +
  labs(title = 'Posterior Distribution for Theta', x = 'Theta', y = 'Density') +
  theme_minimal()
print(plot)
