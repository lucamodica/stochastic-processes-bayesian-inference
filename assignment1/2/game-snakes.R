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
b_P = P

b_P[ , 4] = P[ , 5]
b_P[ , 5] = P[ , 4]
b_P[4,  ] = P[5,  ]
b_P[5,  ] = 0
b_P[5, 5] = 1

b_P

b_Q = b_P[1:4, 1:4]
b_R = b_P[1:(nrow(b_P)-2), (nrow(b_P) - 1) : (nrow(b_P))]
b_I = diag(nrow(b_Q))

# compute the fundamental matrix
b_F = solve(b_I - b_Q)

b_absorb_prob = b_F %*% b_R

# state 1 is now our 1st state in the b_F matrix
# state 6 is now our 1st state in the b_R matrix

b_absorb_prob[1, 1]


# c) To find the probability of the square landing on square 3, starting from
# square 6, we need to make 3 an absorbing state and then find the probability
# that from the transient state 6 the chain is absorbed in state 3. (F*R)

c_P = matrix(0, nrow = 6, ncol = 6)

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
    c_P[i, j] = P[new_i, new_j]
  }
}

c_P[1, 5] = P[1, 2]
for (i in 2:4) {
  c_P[i, 5] = P[i + 1, 2]
}
c_P[5, ] = 0
c_P[5, 5] = 1

c_P

c_Q = c_P[1:4, 1:4]
c_R = c_P[1:(nrow(c_P)-2), (nrow(c_P) - 1) : (nrow(c_P))]
c_I = diag(nrow(c_Q))

# compute the fundamental matrix
c_F = solve(c_I - c_Q)

c_absorb_prob = c_F %*% c_R

# state 6 is now our 3rd state in the new_F matrix
# state 3 is now our 1st state in the new_R matrix

c_absorb_prob[3, 1]


    
# (d) very answers to (a,b,c) using simulations
# simulation for (a)
# start with counter = 0
init <- c(1,rep(0,5))
# compute the average number of steps 
# by doing 1000 simulations, each of them
# with 100 steps random walk
steps = c()
for (i in 1:1000){
  simA <- markov(init,P,100)
  steps <- c(steps, length(simA[simA < 6]))
}
print(paste("Average number of steps to reach the end of the game: ", mean(steps)))

# simulation for (b)
land_in_6 = c()
for (i in 1:1000){
  simB <- markov(init,P,100)
  land_in_6 <- c(land_in_6, ifelse(4 %in% simB[simB < 6], 1, 0))
}
print(paste("Probability that the counter will land on square 6 before the end of the game: ", sum(land_in_6)/length(land_in_6)))

# simulation for (c)
# change the init, since the counter will start in square 6
init <- c(0, 0, 0, 1, 0, 0)
land_in_3 = c()
for (i in 1:1000){
  simC <- markov(init,P,100)
  land_in_3 <- c(land_in_3, ifelse(2 %in% simC[simC < 6], 1, 0))
}
print(paste("Probability that the counter will land on square 3 before the end of the game, with the counter starting from 6: ", sum(land_in_3)/length(land_in_3)))