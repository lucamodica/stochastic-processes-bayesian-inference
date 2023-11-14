# Load required libraries
library(stats)
library(MCMCpack)

# Observed data
data <- c(0.66, 2.30, 1.98, 1.49, 0.62)

# Prior parameters for the inverse gamma distribution
alpha_prior <- 1.01
beta_prior <- 1

# Posterior parameters are updated based on the data
alpha_post <- alpha_prior + length(data)
beta_post <- beta_prior + sum(data^2)

# Define the likelihood function for the Weibull distribution with kappa = 2
likelihood_weibull <- function(x, theta) {
  kappa <- 2
  lambda <- sqrt(theta)  # lambda is the square root of theta
  return(dweibull(x, shape = kappa, scale = lambda))
}

# Define the density function for the inverse gamma posterior
posterior_inv_gamma <- function(theta, alpha, beta) {
  return(dinvgamma(theta, shape = alpha, scale = beta))
}

# Define the integrand for the posterior predictive distribution
posterior_predictive <- function(x) {
  theta <- 1  # Since theta is fixed at 1 in the posterior predictive function
  likelihood <- likelihood_weibull(x, theta)
  posterior <- posterior_inv_gamma(theta, alpha_post, beta_post)
  normalization <- posterior_inv_gamma(theta, alpha_post + 1, beta_post + x^2)
  
  return (likelihood * posterior / normalization)
}

# Calculate the probability P(1 < X6 < 2 | x) using numerical integration
prob_1_to_2 <- function() {
  # We integrate over the range x = 1 to x = 2
  integrand <- function(x) {
    posterior_predictive(x)
  }
  # The integrate function should now correctly receive a scalar from integrand
  integrate(Vectorize(integrand), lower = 1, upper = 2)$value
}

# Calculate the probability
probability <- prob_1_to_2()

# Print the result
print(paste("P(1 < X6 < 2 | x) =", probability))
