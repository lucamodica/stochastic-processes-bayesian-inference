data_Z0_Z4 <- c(1, 1, 2, 5, 7);

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
  return (posterior_lambda(lam, data_Z0_Z4));
};


# question 2b: the extinction probability
extinction_prob <- function(lam) {
  pgf <- function(s) { exp(lam * (s - 1)) }
  
  # Define the function to find a root for (pgf(s) - s)
  f_to_minimize <- function(s) { pgf(s) - s }
  
  # Choose the interval to search for a root
  interval <- if (lam <= 1) c(0, 1) else c(0, 0.999)
  
  # Use uniroot to find the root of the function on the interval [0, 1]
  # The root is the fixed point which corresponds to the extinction 
  # probability
  root <- uniroot(f_to_minimize, interval)$root;
  
  return(root)
}

# question 2c: extinction probability of a branching process
# taking the uncertainty of lambda into account, in terms of
# the already computed posterior distribution and the extinction
extinction_prob_2c <- function() {
  return(integrate(Vectorize(function(lam) { extinction_prob(lam) * posterior_Z0_Z4(lam) }), 0, Inf)$value);
}
cat("Probability of extinction, using numerical integration: ", extinction_prob_2c(), "\n");

# sample a lambda value, considering the posterior computed in 2a
sample_posterior <- function() {
  return(rgamma(1, shape = sum(data_Z0_Z4), rate = length(data_Z0_Z4)))
}


# simulation of extinction probability (question 2d)
# for a branching process with Poisson offspring distribution
# and parameter lambda. The lambda value will be sampled
# from the same posterior distribution computed in 2a, to
# keep the uncertainty of lambda and the observed data
# into account.
extinction_prob_simulation_bayes <- function(n_sims, gens) {
  simlist <- replicate(n_sims, branch(gens, sample_posterior())[11]);
  return (sum(simlist==0)/n_sims);
}
cat("Probability of extinction, using simulations: ", extinction_prob_simulation_bayes(5000, 10), "\n");


# question 2e: maximum likelihood estimation of lambda
# (using the observed data)
likelihood_data_Z0_Z4 <- function(lam) {
  return (prod(dpois(data_Z0_Z4, lam)));
}
maximum_likelihood_lambda <- function(data) {
  return (optimize(Vectorize(likelihood_data_Z0_Z4), c(0, 100), maximum = TRUE)$maximum);
}
cat("Maximum likelihood estimate of lambda: ", maximum_likelihood_lambda(data_Z0_Z4), "\n");
cat("Probability of extinction, using maximum likelihood estimate: ", extinction_prob(maximum_likelihood_lambda(data_Z0_Z4)), "\n");
