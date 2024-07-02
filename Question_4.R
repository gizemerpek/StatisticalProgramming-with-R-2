
#4.soru
scale_variables <- function(data, vars) {
  scaled_dataset <- data
  
  for (var in vars) {
    column <- scaled_dataset[[var]]
    mean_val <- mean(column, na.rm = TRUE)
    sd_val <- sd(column, na.rm = TRUE)
    
    # Scale the variable
    scaled_column <- (column - mean_val) / sd_val
    scaled_dataset[[var]] <- scaled_column
  }
  
  return(scaled_dataset)
}
# Usage example: scale the 'Var7' variable in DatasetNA
scale_variables(DatasetNA, "Var7")
