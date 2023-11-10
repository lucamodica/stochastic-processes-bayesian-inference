# init transition matrix
num_squares = 9
P = matrix(0, nrow = 9, ncol = 9)

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
    
    P[i, new_position] <- P[i, new_position] + 0.25
  }
}

for (i in 1:9) {
  P[2, i] = P[7, i]
  P[5, i] = P[3, i]
  P[8, i] = P[4, i]
}

# remove the unnecessary rows and columns
P = P[-c(2, 5, 8), -c(2, 5, 8)]

# Set the last square as an absorbing state
# (it's the last cell of the game)
P[nrow(P), nrow(P)] = 1

Q = P[-nrow(P), -nrow(P)]
R = P[1:nrow(P)-1, nrow(P)]
I = diag(nrow(Q))
# compute the fundamental matrix
F = solve(I - Q)


# a) Expected number of steps to end the game
# (i.e. the expected number of steps to reach the absorbing state)
a = F %*% rep(1, nrow(F))
# in our case, the expected length of the game corresponds
# to a[1]; in other words, the expected number of steps
# to reach the absorbing state from the initial state
E_length = round(a[1], 2) # on average, 4.78 moves to reach the end.


# b) probability that the counter will land on square 6 
# before the end of the game, that is: F[1, 6]. In the code
# is F[1, 4], considering the the rows/columns deleted for the
# redundant states.
p_6 = F[1, 4]
