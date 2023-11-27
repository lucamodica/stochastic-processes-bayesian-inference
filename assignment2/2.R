# question 2b
extinction_prob <- function(lambda) {
  func <- function(x) { x - exp(lambda * (x - 1)) }
  
  # Use optimize the minimize the function
  prob <- optimize(func, c(0, 1))$minimum;
  
  return(prob);
}