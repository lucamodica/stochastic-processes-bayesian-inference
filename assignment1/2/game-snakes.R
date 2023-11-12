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

print("P:"); P
print("Q:"); Q
print("R:"); R
print("I:"); I


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

p_6


# c) To find the probability of the square landing on square 3, starting from
# square 6, we need to make 3 an absorbing state and then find the probability
# that from the transient state 6 the chain is absorbed in state 3. (F*R)

new_P = matrix(0, nrow = 6, ncol = 6)

for (i in 1:6) {
  new_i = i
  if (1 < i && i < 5) {
    new_i = i + 1
  }
  for (j in 1:6) {
    new_j = j
    if (1 < j && j < 5) {
      new_j = j + 1
    }
    new_P[i, j] = P[new_i, new_j]
  }
}

new_P[1, 5] = P[1, 2]
for (i in 2:4) {
  new_P[i, 5] = P[i + 1, 2]
}
new_P[5, ] = 0
new_P[5, 5] = 1

new_Q = new_P[1:4, 1:4]
new_R = new_P[1:(nrow(new_P)-2), (nrow(new_P) - 1) : (nrow(new_P))]
new_I = diag(nrow(new_Q))

# compute the fundamental matrix
new_F = solve(new_I - new_Q)

absorb_prob = new_F %*% new_R

# state 6 is now our 3rd state in the new_F matrix
# state 3 is now our 1st state in the new_R matrix

absorb_prob[3, 1]
    
    
