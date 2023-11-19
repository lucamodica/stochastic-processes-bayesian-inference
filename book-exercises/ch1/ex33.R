# Cards are drawn from a standard deck, with replacement, until an ace appears.
# Simulate the mean and variance of the number of cards required.

for (i in 1:1000) {
  cards <- sample(1:52, 1000, replace = TRUE)
  ace <- which(cards == 1)
  
  # if there are aces in the sample, the mean and variance
  # of the number of cards required to draw an ace are calculated
  if (length(ace) > 0) {
    print(mean(ace))
    print(var(ace))
  }
}