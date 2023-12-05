
#b)

log_posterior <- function(theta1, theta2, theta3, data) {
  # Extract the data columns
  x <- data[,1]
  y <- data[,2]
  z <- data[,3]
  
  # Compute the likelihood for each observation
  
  # due to the use of the logarithm get that 
  # log(f^z[i])) = z[i] * log(f)
  # log((1 - f)^(1 - z[i])) = (1 - z[i]) * log(1 - f)
  
  epsilon <- 1e-8  # Small constant to prevent log(0)
  
  likelihoods <- sapply(1:length(x), function(i) {
    f <- exp(exp(theta1) * x[i] + exp(theta2) * (y[i] - theta3)^2) / 
      (1 + exp(exp(theta1) * x[i] + exp(theta2) * (y[i] - theta3)^2))
    z[i] * log(f + epsilon) + (1 - z[i]) * log(1 - f + epsilon)
  })
  
  # the product of the likelihoods of each observation log(a*b*...*N)
  # is equivalent to the sum of the log of each observation's likelihood
  # log(a) + log(b) + ... + log(N)
  
  # Sum the log likelihoods
  sum_likelihoods <- sum(likelihoods)
  
  # Return the sum (since the prior is flat, the posterior is proportional to the likelihood)
  return(sum_likelihoods)
}



#c)

mcmc <- function(start_values, data, iterations = 10000) {
  current_theta <- start_values
  chain <- matrix(NA, nrow = iterations, ncol = 3)
  
  for (i in 1:iterations) {
    proposed_theta <- current_theta + rnorm(3, mean = 0, sd = 0.4)
    
    current_log_posterior <- log_posterior(current_theta[1], current_theta[2], current_theta[3], data)
    proposed_log_posterior <- log_posterior(proposed_theta[1], proposed_theta[2], proposed_theta[3], data)
    
    if (!is.finite(current_log_posterior) || !is.finite(proposed_log_posterior)) {
      next  # Skip iteration if log-posterior is not finite
    }
    
    #symmetric random walk
    acceptance_ratio <- exp(proposed_log_posterior - current_log_posterior)
    
    if (runif(1) < acceptance_ratio) {
      current_theta <- proposed_theta
    }
    
    chain[i, ] <- current_theta
  }
  
  return(chain)
}

# Example usage
start_values <- c(0, 0, 17.22) # Replace with your starting values
data <- read.table("dataAssignment3.txt", header = TRUE)
chain <- mcmc(start_values, data)


# Assuming 'chain' is the output from your MCMC function

# Plot for theta1
plot(chain[, 1], type = "l", col = "blue", xlab = "Iteration", ylab = "Theta1", main = "Trace Plot for Theta1")

# Plot for theta2
plot(chain[, 2], type = "l", col = "red", xlab = "Iteration", ylab = "Theta2", main = "Trace Plot for Theta2")

# Plot for theta3
plot(chain[, 3], type = "l", col = "green", xlab = "Iteration", ylab = "Theta3", main = "Trace Plot for Theta3")



# d)

# Assuming 'chain' contains your MCMC samples
theta_mean <- colMeans(chain)

# Extracting mean values of theta1, theta2, theta3
theta1_mean <- theta_mean[1]
theta2_mean <- theta_mean[2]
theta3_mean <- theta_mean[3]

# Probability function
f <- function(x, y, theta1, theta2, theta3) {
  exp(exp(theta1) * x + exp(theta2) * (y - theta3)^2) / 
    (1 + exp(exp(theta1) * x + exp(theta2) * (y - theta3)^2))
}

# Compute the probability for one animal
p_single_animal <- f(3, 13, theta1_mean, theta2_mean, theta3_mean)

# Compute the probability that 9 out of 10 animals will develop the disease
# binomial(9; 10, p)
p_nine_out_of_ten <- choose(10, 9) * p_single_animal^9 * (1 - p_single_animal)^1

# Print the results
print(paste("Probability for one animal:", p_single_animal))
print(paste("Probability for 9 out of 10 animals:", p_nine_out_of_ten))


