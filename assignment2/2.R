if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# function to simulate a branching process 
# with Poisson offspring distribution
branch <- function(n, lam) { 
	z <- c(1,rep(0,n))
	for (i in 2:(n+1)) {
		z[i] <- sum(rpois(z[i-1],lam));
	}
	return(z);
}

# posterior for lambda, derived from question 2a.
posterior_lambda <- function(lam, data) {
  return (dgamma(lam, sum(data), length(data)));
}

# computed posterior for the observed data
# (Z0, Z1, Z2, Z3, Z4) = (1, 1, 2, 5, 7)
posterior_Z0_Z4 <- function(lam) {
  return (posterior_lambda(lam, c(1, 1, 2, 5, 7)));
};


lambda_values <- seq(0, 3, length.out = 1000)

posterior_densities = posterior_Z0_Z4(lambda_values)


# Plot the posterior distribution
df <- data.frame(lambda = lambda_values, density = posterior_densities)
ggplot(df, aes(x = lambda, y = density)) +
  geom_line() +
  labs(title = "Posterior Distribution of λ",
       x = expression(lambda),
       y = "Density") +
  theme_minimal()


# question 2b: the extinction probability
extinction_prob <- function(lam) {
  # Define the PGF for the Poisson distribution
  pgf <- function(s) { exp(lam * (s - 1)) }
  
  # Define the function to find a root for (pgf(s) - s)
  f_to_minimize <- function(s) { pgf(s) - s }
  
  # Choose the interval to search for a root
  interval <- if (lam <= 1) c(0, 1) else c(0, 0.999)
  
  # Use uniroot to find the root of the function on the interval [0, 1]
  # The root is the fixed point which corresponds to the extinction probability
  root <- uniroot(f_to_minimize, interval)$root;
  
  return(root)
}

# simulation of extintion probability
# for a branching process with Poisson offspring distribution
# and parameter lambda
extinction_prob_simulation <- function(n_sims, gens, lam) {
  simlist <- replicate(n_sims, branch(gens,lam)[11]);
  return (sum(simlist==0)/n_sims);
}

print(extinction_prob(5));
print(extinction_prob_simulation(1000, 10, 5))


