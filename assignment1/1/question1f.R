library(ggplot2)
library(stats)

# Data and parameters
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

# compute and normalize the posterior
posterior <- sapply(theta_values, function(theta) {
  prod(sapply(data, likelihood_weibull, theta = theta))
}) * prior
posterior <- posterior / sum(posterior * diff(theta_values[1:2]))

# posterior predictive distribution func
posterior_predictive <- function(x_new, theta_values, posterior) {
  sapply(x_new, function(x) {
    sum(sapply(theta_values, likelihood_weibull, x = x) * posterior * diff(theta_values[1:2]))
  })
}

# Compute P(1 < X6 < 2 | x) using numerical integration
result <- integrate(
  function(x) posterior_predictive(x, theta_values, posterior), lower = 1, upper = 2
)
print(paste("P(1 < X6 < 2 | x) =", result$value))
