# question 2b: join prob for excatly 4 trees in
# the 2 mentioned squares (with an overlapping area)
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
print(total_prob)


# question 2c: simulation
