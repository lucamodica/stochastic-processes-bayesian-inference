# Load the dataset (you can replace 'mtcars' with your own dataset)
data <- mtcars

# Calculate the average and standard deviation of all numeric columns
averages <- colMeans(data)
std_devs <- apply(data, 2, sd)

# Brief description of the dataset
description <- paste("This dataset contains information about various car models. It includes columns such as mpg (miles per gallon), cyl (number of cylinders), hp (horsepower), and more.")

# Print the results
cat("Average values for each column:\n")
print(averages)

cat("\nStandard deviations for each column:\n")
print(std_devs)

cat("\nDataset Description:\n")
cat(description)
