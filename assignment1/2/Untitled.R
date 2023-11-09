# Number of squares
num_squares <- 9

# Initialize the transition matrix
transition_matrix <- matrix(0, nrow = num_squares, ncol = num_squares)

# Populate the transition matrix
for (i in 1:(num_squares - 1)) {
  for (roll in 1:4) {
    new_position = i + roll
    
    if (new_position == 2) {
      new_position = 7
    }
    if (new_position == 5) {
      new_position = 3
    }
    if (new_position == 8) {
      new_position = 4
    }
    
    if (new_position > num_squares) {
      new_position = num_squares
    }
    
    transition_matrix[i, new_position] <- transition_matrix[i, new_position] + 0.25
  }
}

for (i in 1:9) {
  transition_matrix[2, i] = transition_matrix[7, i]
  transition_matrix[5, i] = transition_matrix[3, i]
  transition_matrix[8, i] = transition_matrix[4, i]
}

# Set the last square as an absorbing state
transition_matrix[num_squares, num_squares] = 1

# Print the transition matrix
transition_matrix


# a) Expected number of steps to end the game

expected_steps <- rep(Inf, num_squares)
expected_steps[num_squares] <- 0 # No steps from the end square

epsilon <- 1e-5 # convergence threshold
converged <- FALSE

while (!converged) {
  old_expected_steps <- expected_steps
  for (i in 1:(num_squares - 1)) {
    expected_steps[i] <- 1 + sum(transition_matrix[i, ] * old_expected_steps)
  }
  
  # Check for convergence
  if (max(abs(expected_steps - old_expected_steps), na.rm = TRUE) < epsilon) {
    converged <- TRUE
  }
}

expected_steps[1] # Expected number of steps from the start

