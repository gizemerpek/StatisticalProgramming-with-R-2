#2.soru

# Function to calculate statistics such as mean, median, etc. for a given column with or without factors
calculate_statistics <- function(data, column, factors = NULL) {
  
  result <- data.frame()
  
  if (is.null(factors)) {
    num_obs <- 0
    min_val <- Inf
    max_val <- -Inf
    sum_val <- 0
    sum_squares <- 0
    count <- 0
    
    # Loop through the data to compute statistics
    for (i in 1:length(data[[column]])) {
      value <- data[[column]][i]
      
      if (!is.na(value)) {
        num_obs <- num_obs + 1
        sum_val <- sum_val + value
        sum_squares <- sum_squares + value^2
        count <- count + 1
        
        if (value < min_val) {
          min_val <- value
        }
        
        if (value > max_val) {
          max_val <- value
        }
      }
    }
    
    mean_val <- sum_val / num_obs
    
    sorted_values <- sort(data[[column]], na.last = NA)
    
    if (count %% 2 == 0) {
      median_val <- (sorted_values[count / 2] + sorted_values[count / 2 + 1]) / 2
    } else {
      median_val <- sorted_values[(count + 1) / 2]
    }
    
    variance <- (sum_squares - (sum_val^2) / num_obs) / (num_obs - 1)
    sd_val <- sqrt(variance)
    
    # Create a data frame with the computed statistics
    result <- data.frame(
      Factor = NA,
      Num_Observations = num_obs,
      Minimum = min_val,
      Maximum = max_val,
      Range = max_val - min_val,
      Sum = sum_val,
      Mean = mean_val,
      Median = median_val,
      Sum_of_Squares = sum_squares,
      Variance = variance,
      Standard_Deviation = sd_val
    )
  } else {
    # If factors are provided, compute statistics for each factor level
    for (factor in factors) {
      unique_factors <- unique(data[[factor]])
      
      for (unique_factor in unique_factors) {
        num_obs <- 0
        min_val <- Inf
        max_val <- -Inf
        sum_val <- 0
        sum_squares <- 0
        count <- 0
        
        # Loop through the data to compute statistics for each factor level
        for (i in 1:length(data[[column]])) {
          value <- data[[column]][i]
          current_factor <- data[[factor]][i]
          
          if (current_factor == unique_factor && !is.na(value)) {
            num_obs <- num_obs + 1
            sum_val <- sum_val + value
            sum_squares <- sum_squares + value^2
            count <- count + 1
            
            if (value < min_val) {
              min_val <- value
            }
            
            if (value > max_val) {
              max_val <- value
            }
          }
        }
        
        mean_val <- sum_val / num_obs
        
        sorted_values <- sort(data[[column]], na.last = NA)
        
        if (count %% 2 == 0) {
          median_val <- (sorted_values[count / 2] + sorted_values[count / 2 + 1]) / 2
        } else {
          median_val <- sorted_values[(count + 1) / 2]
        }
        
        variance <- (sum_squares - (sum_val^2) / num_obs) / (num_obs - 1)
        sd_val <- sqrt(variance)
        
        # Add computed statistics for the current factor level to the result data frame
        result <- rbind(result, data.frame(
          Factor = unique_factor,
          Num_Observations = num_obs,
          Minimum = min_val,
          Maximum = max_val,
          Range = max_val - min_val,
          Sum = sum_val,
          Mean = mean_val,
          Median = median_val,
          Sum_of_Squares = sum_squares,
          Variance = variance,
          Standard_Deviation = sd_val
        ))
      }
    }
  }
  
  return(result)
}

# Function to calculate and print statistics based on user input
calculate_and_print_statistics <- function() {
  
  # Get column name from user
  column_name <- readline("Enter the column name: ")
  
  # Get factor(s) from user
  factors <- readline("Enter the factor(s) (separated by comma): ")
  factors <- strsplit(factors, ",")[[1]]
  
  # Calculate statistics and print result
  result <- calculate_statistics(DatasetNA, column_name, factors)
  
  print(result)
}

# Call the function to calculate and print statistics
calculate_and_print_statistics()

