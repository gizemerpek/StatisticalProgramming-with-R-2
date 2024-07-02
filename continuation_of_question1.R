
#1.sorunun devam?? Cross-products, Covariance, Correlations hesaplamas??
calculation <- function(data) {
  my_data <- data.frame(data)
  
  column_name1 <- readline("Enter the first column name: ")
  column_name2 <- readline("Enter the second column name: ")
  
  column1 <- data[[column_name1]]
  column2 <- data[[column_name2]]
  
  column1 <- column1[!is.na(column1)]
  column2 <- column2[!is.na(column2)]
  
  
  
  x <- column1
  y <- column2
  n <- length(x)
  
  cross_product <- 0
  for (i in 1:n) {
    cross_product <- cross_product + (x[i] * y[i])
  }
  
  cross_product
  
  mean_x <- 0
  mean_y <- 0
  for (i in 1:n) {
    mean_x <- mean_x + x[i]
    mean_y <- mean_y + y[i]
  }
  mean_x <- mean_x / n
  mean_y <- mean_y / n
  
  covariance <- 0
  for (i in 1:n) {
    covariance <- covariance + (x[i] - mean_x) * (y[i] - mean_y)
  }
  covariance <- covariance / (n - 1)
  covariance
  
  var <- 0
  sd_x <- 0
  sd_y <- 0
  for (i in 1:n) {
    var <- var + (x[i] - mean_x) * (y[i] - mean_y)
    sd_x <- sd_x + (x[i] - mean_x)^2
    sd_y <- sd_y + (y[i] - mean_y)^2
  }
  var <- var / (n - 1)
  sd_x <- sqrt(sd_x / (n - 1))
  sd_y <- sqrt(sd_y / (n - 1))
  
  correlation <- var / (sd_x * sd_y)
  correlation
  
  print(return(list(
    cross_product=cross_product,
    covariance=covariance,
    correlation=correlation
  )))
}
calculation(DatasetNA)
