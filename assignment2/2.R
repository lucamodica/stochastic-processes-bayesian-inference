# function to simulate a branching process 
# with Poisson offspring distribution
branch <- function(n, lam) { 
	z <- c(1,rep(0,n))
	for (i in 2:(n+1)) {
		z[i] <- sum(rpois(z[i-1],lam));
	}
	return(z);
}


# question 2b
extinction_prob <- function(lam) {
  pgf <- function(s) { exp(lam * (s - 1)) }
  
  # Use optimize the minimize the function
  prob <- optimize(pgf, c(0, 1))$minimum;
  
  return(prob);
}

print(extinction_prob(3));