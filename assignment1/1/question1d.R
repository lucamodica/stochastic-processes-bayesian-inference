# Load required library for numerical integration
library(stats)

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
  return((kappa / lambda) * (x / lambda)^(kappa - 1) * exp(-(x / lambda)^kappa))
}


# Define the density function for the inverse gamma posterior
posterior_inv_gamma <- function(theta, alpha, beta) {
  # Ensure that theta is a scalar
  if (!is.numeric(theta) || length(theta) != 1) {
    stop("Theta should be a scalar.")
  }
  # Calculate the posterior density
  return((beta^alpha) / gamma(alpha) * theta^(-alpha - 1) * exp(-beta / theta))
}

# Define the integrand for the posterior predictive distribution
posterior_predictive <- function(x, alpha, beta) {
  integrand <- function(theta) {
    likelihood_weibull(x, theta) * posterior_inv_gamma(theta, alpha, beta)
  }
  # Ensure that integrate function receives a scalar by using Vectorize
  return(integrate(Vectorize(integrand), lower = 0, upper = Inf)$value)
}

# Calculate the probability P(1 < X6 < 2 | x) using numerical integration
prob_1_to_2 <- function(alpha, beta) {
  # We integrate over the range x = 1 to x = 2
  integrand <- function(x) {
    posterior_predictive(x, alpha, beta)
  }
  # The integrate function should now correctly receive a scalar from integrand
  integrate(Vectorize(integrand), lower = 1, upper = 2)$value
}

# Calculate the probability
probability <- prob_1_to_2(alpha_post, beta_post)

# Print the result
print(paste("P(1 < X6 < 2 | x) =", probability))
