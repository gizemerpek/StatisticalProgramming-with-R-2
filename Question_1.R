# 1.SORU

# Function to calculate statistics of a given column in a dataset
calculate_statistics <- function(data) {
  my_data <- data.frame(data)
  # Ask user to input the column name
  column_name <- readline("Enter the column name: ")
  
  # Extract the specified column
  column <- data[[column_name]]
  # Remove NA values from the column
  column <- column[!is.na(column)]
  
  # Initialize variables for statistics
  num_observations <- length(column) + 1  # Add 1 for the NAs
  minimum <- 0
  maximum <- 0
  sum_value <- 0
  sum_of_squares <- 0
  
  # Iterate through each value in the column
  for (value in column) {
    # Update minimum value if current value is less than current minimum or if it's the first value
    if (minimum == 0 || value < minimum) {
      minimum <- value
    }
    # Update maximum value if current value is greater than current maximum
    if (value > maximum) {
      maximum <- value
    }
    
    # Calculate sum of values
    sum_value <- sum_value + value
    # Calculate range (maximum - minimum)
    range_value <- maximum - minimum
    # Calculate mean (sum of values / number of observations)
    mean_value <- sum_value / num_observations
    # Calculate median (middle value of the sorted data)
    median_value <- (column[num_observations %/% 2] + column[num_observations %/% 2 + 1]) / 2
    # Calculate sum of squares of differences from the mean
    sum_of_squares <- sum_of_squares + (value - mean_value)^2
    # Calculate variance (average of the sum of squares)
    variance <- sum_of_squares / (length(column) - 1)
    # Calculate standard deviation (square root of the variance)
    standard_deviation <- sqrt(variance)
    
  }
  # Print and return the statistics as a list
  print(return(list(
    num_observations = num_observations,
    minimum = minimum,
    maximum = maximum,
    range = range_value,
    sum = sum_value,
    mean = mean_value,
    median = median_value,
    sum_of_squares = sum_of_squares,
    variance = variance,
    standard_deviation = standard_deviation
  )))
}

# Call the function with the dataset as input
calculate_statistics(DatasetNA)

