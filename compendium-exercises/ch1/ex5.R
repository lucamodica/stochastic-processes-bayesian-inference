# Write an R program that computes the parameter α for a Beta(α, α)
# distribution which has 90% of its density in the interval [0.4, 0.6].
upper_limit = 0.6;
lower_limit = 0.4;
target_density = 0.9;
ddiff <- function(alpha) {
  density = pbeta(0.6, alpha, alpha) - pbeta(0.4, alpha, alpha);
  
  return(abs(density - target_density)); 
}

alpha = optimize(ddiff, c(0, 100))$minimum;
print(alpha);