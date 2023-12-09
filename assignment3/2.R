# question 2b: join prob for excatly 4 trees in
# the 2 mentioned squares (with an overlapping area)
prob_2b <- function() {
  total_prob <- 0
  lambda_full <- 36
  lambda_small <- 0.4^2 * lambda_full
  lambda_overlap <- 0.2^2 * lambda_full
  lambda_non_overlap <- lambda_small - lambda_overlap

  for (i in 0:4) {
    prob_overlap <- dpois(i, lambda_overlap)
    prob_square1_non_overlap <- dpois(4 - i, lambda_non_overlap)
    prob_square2_non_overlap <- dpois(4 - i, lambda_non_overlap)
    
    total_prob <- total_prob + (prob_overlap * prob_square1_non_overlap * prob_square2_non_overlap)
  }
  return(total_prob)
}
print(prob_2b())


# question 2c: simulation
spatial_poisson_sim <- function(lambda, trials) {
    simlist <- numeric(trials)
    for (i in 1:trials){
      N <- rpois(1, lambda)
      x <- runif(N,0,1)
      y <- runif(N,0,1)
      simlist[i] = N
    }
  
  N_mean = round(mean(simlist))
  print("mean number of trees after the simulations: ")
  print(N_mean)
  x <- runif(N_mean,0,1)
  y <- runif(N_mean,0,1)
  plot(x, y, xlim=c(0,1), ylim=c(0,1), main="Simulated spatial Poisson process")
}
spatial_poisson_sim(36, 10000)

# question 2d: simulation with posterior
# Define the prior parameters for the Gamma distribution
spatial_poisson_sim_posterior <- function(trials) {
  # Update the posterior parameters based on the observed data
  alpha_post <- 0 + 36  # 36 trees observed
  beta_post <- 0 + 1     # Assuming the observation is for a unit area
  simlist <- numeric(trials)
  
  for (i in 1:trials) {
    N <- rgamma(1, shape = alpha_post, rate = beta_post)
    x <- runif(N,0,1)
    y <- runif(N,0,1)
    simlist[i] = N
  }
  
  N_mean = round(mean(simlist))
  print("mean number of trees after the simulations (with posterior): ")
  print(N_mean)
  x <- runif(N_mean,0,1)
  y <- runif(N_mean,0,1)
  plot(x, y, xlim=c(0,1), ylim=c(0,1), main="Simulated spatial Poisson process with posterior")
}
spatial_poisson_sim_posterior(10000)



# question 2e: simulation from Z
compute_Z <- function() {
  alpha_post <- 0 + 36  # 36 trees observed
  beta_post <- 0 + 1     # Assuming the observation is for a unit area
  l <- rgamma(1, shape = alpha_post, rate = beta_post)
  n <- rpois(1, l)
  x <- runif(n, 0, 1)
  y <- runif(n, 0, 1)
  
  # Compute Z
  distances <- matrix(NA, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      distances[i, j] <- sqrt((x[i] - x[j])^2 + (y[i] - y[j])^2)
    }
  }
  # Exclude self-distances (which are 0) by setting them to Inf
  diag(distances) <- Inf
  return (mean(apply(distances, 1, min)))
}

simulation_Z <- function(trials) {
  # Repeat the above process many times to get a distribution of Z
  Z_values <- replicate(trials, compute_Z())
  print("Mean of the distances: ")
  print(mean(Z_values))
  hist(Z_values, main="Histogram of Z values", xlab="Z")
}
simulation_Z(10000)





