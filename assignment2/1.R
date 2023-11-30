simulate1 <- function() {
  initial_chain <- c(1, 2, 3, 2, 3, 1, 2, 1, 3, 2, 1, 3)
  transition_counts <- matrix(1, nrow = 3, ncol = 3)
  
  # Update counts with initial chain
  for (i in 1:(length(initial_chain) - 1)) {
    transition_counts[initial_chain[i], initial_chain[i + 1]] <- transition_counts[initial_chain[i], initial_chain[i + 1]] + 1
  }
  
  expected_transition_matrix <- apply(transition_counts, 1, function(row) row / sum(row))
  total_length <- 500
  current_state <- initial_chain[length(initial_chain)]
  
  # Simulate the rest of the chain
  for (i in (length(initial_chain) + 1):total_length) {
    transition_prob <- expected_transition_matrix[current_state, ]
    next_state <- sample(1:3, size = 1, prob = transition_prob)
    
    transition_counts[current_state, next_state] <- transition_counts[current_state, next_state] + 1
    expected_transition_matrix <- apply(transition_counts, 1, function(row) row / sum(row))
    current_state <- next_state
  }
  
  return(expected_transition_matrix)
}
cat("Expected value of P when the data is all observed and all simulated values: ", simulate1(),  "\n\n")

# Function to extract P23 from the transition matrix
extract_P23 <- function(matrix) {
  return(matrix[2, 3])
}
P23_values <- replicate(1000, extract_P23(simulate1()))
hist(P23_values, main = "Histogram of Expected P23 Values (simulation 1)", xlab = "P23", breaks = 30)


simulate2 <- function() {
  initial_chain <- c(1, 2, 3, 2, 3, 1, 2, 1, 3, 2, 1, 3)
  transition_counts <- matrix(1, nrow = 3, ncol = 3)
  
  # Update counts with initial chain
  for (i in 1:(length(initial_chain) - 1)) {
    transition_counts[initial_chain[i], initial_chain[i + 1]] <- transition_counts[initial_chain[i], initial_chain[i + 1]] + 1
  }
  
  initial_expected_matrix <- apply(transition_counts, 1, function(row) row / sum(row))
  total_length <- 500
  current_state <- initial_chain[length(initial_chain)]
  
  # Simulate the rest of the chain
  for (i in (length(initial_chain) + 1):total_length) {
    transition_prob <- initial_expected_matrix[current_state, ]
    
    next_state <- sample(1:3, size = 1, prob = transition_prob)
    transition_counts[current_state, next_state] <- transition_counts[current_state, next_state] + 1
    current_state <- next_state
  } 
  final_expected_matrix <- apply(transition_counts, 1, function(row) row / sum(row))
  
  return(final_expected_matrix)
}
cat("Expected value of P in simulation 2: ", simulate2(),  "\n\n")

P23_values_simulate2 <- replicate(1000, extract_P23(simulate2()))
hist(P23_values_simulate2, main = "Histogram of Expected P23 Values (simulate2)", xlab = "P23", breaks = 30)

#install.packages("LearnBayes")
library(LearnBayes)

simulate3 <- function() {
  initial_chain <- c(1, 2, 3, 2, 3, 1, 2, 1, 3, 2, 1, 3)
  transition_counts <- matrix(1, nrow = 3, ncol = 3)
  
  # Update counts with initial chain
  for (i in 1:(length(initial_chain) - 1)) {
    transition_counts[initial_chain[i], initial_chain[i + 1]] <- transition_counts[initial_chain[i], initial_chain[i + 1]] + 1
  }
  
  # Parameters for the Dirichlet posterior for each row of P
  dirichlet_parameters <- list(c(1, 3, 3), c(3, 1, 3), c(2, 3, 1))
  
  sampled_transition_matrix <- t(sapply(dirichlet_parameters, rdirichlet, n = 1))
  total_length <- 500
  
  # Simulate the rest of the chain
  current_state <- initial_chain[length(initial_chain)]
  for (i in (length(initial_chain) + 1):total_length) {
    # Simulate the next state based on the sampled transition matrix
    transition_prob <- sampled_transition_matrix[current_state, ]
    
    next_state <- sample(1:3, size = 1, prob = transition_prob)
    transition_counts[current_state, next_state] <- transition_counts[current_state, next_state] + 1    
    current_state <- next_state
  }
  
  final_expected_matrix <- apply(transition_counts, 1, function(row) row / sum(row))  
  return(final_expected_matrix)
}
cat("Expected value of P in simulation 3: ", simulate2(),  "\n\n")

P23_values_simulate3 <- replicate(1000, extract_P23(simulate3()))
hist(P23_values_simulate3, main = "Histogram of Expected P23 Values (simulate3)", xlab = "P23", breaks = 30)



