#3.Soru

# Function to draw scatterplots or scatterplot matrices based on user input
draw_plots <- function(data) {
  # Inner function to draw a scatterplot between two variables
  draw_scatterplot <- function(x, y) {
    # Extract x and y values from the dataset
    x_values <- data[[x]]
    y_values <- data[[y]]
    
    # Set plot margins
    par(mar = c(5, 5, 2, 2))
    
    # Create a new plot
    plot.new()
    # Set plot window with appropriate limits
    plot.window(xlim = range(x_values, na.rm = TRUE), ylim = range(y_values, na.rm = TRUE), xlab = x, ylab = y)
    
    # Draw box around plot
    box()
    # Add axes
    axis(1)
    axis(2)
    
    # Plot points
    points(x_values, y_values)
    
    # Add title
    title(main = paste("Scatterplot between", x, "and", y))
  }
  
  # Inner function to draw a scatterplot matrix
  draw_scatterplot_matrix <- function() {
    # Identify numeric variables in the dataset
    numeric_vars <- sapply(data, is.numeric)
    numeric_data <- data[, numeric_vars]
    num_vars <- ncol(numeric_data)
    # Set plot layout to a matrix
    par(mfrow = c(num_vars, num_vars))
    par(mar = rep(0.4, 4))
    
    # Loop through each variable pair to draw scatterplots
    for (i in 1:num_vars) {
      for (j in 1:num_vars) {
        # Skip if same variable
        if (i == j) {
          next
        }
        
        # Get variable names
        x_var <- colnames(numeric_data)[i]
        y_var <- colnames(numeric_data)[j]
        
        # Extract x and y values
        x_values <- numeric_data[[x_var]]
        y_values <- numeric_data[[y_var]]
        
        # Create a new plot
        plot.new()
        # Set plot window with appropriate limits
        plot.window(xlim = range(x_values, na.rm = TRUE), ylim = range(y_values, na.rm = TRUE))
        
        # Draw box around plot
        box()
        # Add axes
        axis(1)
        axis(2)
        
        # Plot points
        points(x_values, y_values)
      }
    }
    
    # Reset plot layout
    par(mfrow = c(1, 1))
  }
  
  # Ask user for choice
  choice <- as.integer(readline("Enter '1' for scatterplot or '2' for scatterplot matrix: "))
  if (choice == 1) {
    # If choice is 1, ask for X and Y variable names and draw scatterplot
    x_var <- readline("Enter the X-axis variable name: ")
    y_var <- readline("Enter the Y-axis variable name: ")
    draw_scatterplot(x_var, y_var)
  } else if (choice == 2) {
    # If choice is 2, draw scatterplot matrix
    draw_scatterplot_matrix()
  } else {
    # If choice is invalid, print error message
    print("Invalid choice!")
  }
}

# Call the function with the dataset as input
draw_plots(DatasetNA)

