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

# question 2b: the extintion probability
extinction_prob <- function(lam) {
  pgf <- function(s) { exp(lam * (s - 1)) }
  
  # Use optimize the minimize the function (the extinction
  # probability is the smallest fixed point of the pgf)
  prob <- optimize(pgf, c(0, 1))$minimum;
  
  return(prob);
}

