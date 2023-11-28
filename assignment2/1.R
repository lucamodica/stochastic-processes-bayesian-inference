simulate1 <- function() {
  # Initial observed chain
  initial_chain <- c(1, 2, 3, 2, 3, 1, 2, 1, 3, 2, 1, 3)
  
  # Initialize transition count matrix with pseudo-counts (Dirichlet prior)
  transition_counts <- matrix(1, nrow = 3, ncol = 3)
  
  # Update counts with initial chain
  for (i in 1:(length(initial_chain) - 1)) {
    transition_counts[initial_chain[i], initial_chain[i + 1]] <- transition_counts[initial_chain[i], initial_chain[i + 1]] + 1
  }
  
  # Calculate the initial expected transition matrix
  expected_transition_matrix <- apply(transition_counts, 1, function(row) row / sum(row))
  
  # Total length of the chain to simulate
  total_length <- 500
  
  # Initialization of current_state to the last state of the initial chain
  current_state <- initial_chain[length(initial_chain)]
  
  # Simulate the rest of the chain
  for (i in (length(initial_chain) + 1):total_length) {
    # Simulate the next state based on the current expected transition matrix
    transition_prob <- expected_transition_matrix[current_state, ]
    next_state <- sample(1:3, size = 1, prob = transition_prob)
    
    # Update transition counts
    transition_counts[current_state, next_state] <- transition_counts[current_state, next_state] + 1
    
    # Update the expected transition matrix
    expected_transition_matrix <- apply(transition_counts, 1, function(row) row / sum(row))
    
    # Update current state for the next iteration
    current_state <- next_state
  }
  
  return(expected_transition_matrix)
}

simulate1()

# Function to extract P23 from the transition matrix
extract_P23 <- function(matrix) {
  return(matrix[2, 3])
}

# Replicate the simulation 1000 times and extract P23 for each matrix
P23_values <- replicate(1000, extract_P23(simulate1()))

# Create a histogram of the P23 values
hist(P23_values, main = "Histogram of Expected P23 Values", xlab = "P23", breaks = 30)




simulate2 <- function() {
  # Initial observed chain
  initial_chain <- c(1, 2, 3, 2, 3, 1, 2, 1, 3, 2, 1, 3)
  
  # Initialize transition count matrix with pseudo-counts
  transition_counts <- matrix(1, nrow = 3, ncol = 3)
  
  # Update counts with initial chain
  for (i in 1:(length(initial_chain) - 1)) {
    transition_counts[initial_chain[i], initial_chain[i + 1]] <- transition_counts[initial_chain[i], initial_chain[i + 1]] + 1
  }
  
  # Initial expected transition matrix based only on observed data
  initial_expected_matrix <- apply(transition_counts, 1, function(row) row / sum(row))
  
  # Total length of the chain to simulate
  total_length <- 500
  
  # Simulate the rest of the chain
  current_state <- initial_chain[length(initial_chain)]
  
  # Simulate the rest of the chain
  for (i in (length(initial_chain) + 1):total_length) {
    # Simulate the next state based on the initial expected transition matrix
    transition_prob <- initial_expected_matrix[current_state, ]
    next_state <- sample(1:3, size = 1, prob = transition_prob)
    
    # Update transition counts with the new state
    transition_counts[current_state, next_state] <- transition_counts[current_state, next_state] + 1
    
    # Update current state for the next iteration
    current_state <- next_state
  }
  
  # Calculate the final expected transition matrix using all data
  final_expected_matrix <- apply(transition_counts, 1, function(row) row / sum(row))
  
  return(final_expected_matrix)
}

simulate2()

P23_values_simulate2 <- replicate(1000, extract_P23(simulate2()))
hist(P23_values_simulate2, main = "Histogram of Expected P23 Values (simulate2)", xlab = "P23", breaks = 30)

# simulate1 uses a dynamic approach, where the expected transition matrix is updated at each step,
# reflecting the most recent data. This might lead to a more variable distribution as the 
# simulation can 'drift' based on early simulated values.

# simulate2, on the other hand, always uses the initial expected matrix for simulation, which is 
# based only on the observed data. This might result in a more stable or consistent distribution, 
# as the simulation is not influenced by the outcomes of previous simulations.

#install.packages("LearnBayes")
library(LearnBayes)

library(LearnBayes)

simulate3 <- function() {
  # Initial observed chain
  initial_chain <- c(1, 2, 3, 2, 3, 1, 2, 1, 3, 2, 1, 3)
  
  # Initialize transition count matrix with pseudo-counts (from Dirichlet prior)
  transition_counts <- matrix(1, nrow = 3, ncol = 3)
  
  # Update counts with initial chain
  for (i in 1:(length(initial_chain) - 1)) {
    transition_counts[initial_chain[i], initial_chain[i + 1]] <- transition_counts[initial_chain[i], initial_chain[i + 1]] + 1
  }
  
  # Parameters for the Dirichlet posterior for each row of P
  # Replace this with your actual parameters obtained from part (a)
  dirichlet_parameters <- list(c(1, 3, 3), c(3, 1, 3), c(2, 3, 1)) 
  
  # Sample a transition matrix P from the posterior
  sampled_transition_matrix <- t(sapply(dirichlet_parameters, rdirichlet, n = 1))
  
  # Total length of the chain to simulate
  total_length <- 500
  
  # Simulate the rest of the chain
  current_state <- initial_chain[length(initial_chain)]
  for (i in (length(initial_chain) + 1):total_length) {
    # Simulate the next state based on the sampled transition matrix
    transition_prob <- sampled_transition_matrix[current_state, ]
    next_state <- sample(1:3, size = 1, prob = transition_prob)
    
    # Update transition counts with the new state
    transition_counts[current_state, next_state] <- transition_counts[current_state, next_state] + 1
    
    # Update current state for the next iteration
    current_state <- next_state
  }
  
  # Calculate the final expected transition matrix using all data
  final_expected_matrix <- apply(transition_counts, 1, function(row) row / sum(row))
  
  return(final_expected_matrix)
}

P23_values_simulate3 <- replicate(1000, extract_P23(simulate3()))
hist(P23_values_simulate3, main = "Histogram of Expected P23 Values (simulate3)", xlab = "P23", breaks = 30)



