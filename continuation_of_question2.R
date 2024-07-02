#Calculation of Cross Products and Covariance and Correlations of the 2nd question

calculate_cross_products <- function(data, var_names, group_var = NULL, gender_var = NULL) {
  # Filter data based on group and gender variables
  if (!is.null(group_var) && !is.null(gender_var)) {
    data <- subset(data, !is.na(data[group_var]) & !is.na(data[gender_var]))
    groups <- unique(data[group_var])
    genders <- unique(data[gender_var])
  } else if (!is.null(group_var)) {
    data <- subset(data, !is.na(data[group_var]))
    groups <- unique(data[group_var])
  } else if (!is.null(gender_var)) {
    data <- subset(data, !is.na(data[gender_var]))
    genders <- unique(data[gender_var])
  }
  
  # Calculate cross-products
  results <- data.frame()
  
  for (var1 in var_names) {
    for (var2 in var_names) {
      if (var1 != var2) {
        cross_product <- 0
        
        if (!is.null(group_var) && !is.null(gender_var)) {
          for (group in groups) {
            for (gender in genders) {
              subset_data <- subset(data, data[group_var] == group & data[gender_var] == gender)
              n <- nrow(subset_data)
              for (i in 1:n) {
                if (!is.na(subset_data[i, var1]) && !is.na(subset_data[i, var2])) {
                  cross_product <- cross_product + (subset_data[i, var1] * subset_data[i, var2])
                }
              }
            }
          }
        } else if (!is.null(group_var)) {
          for (group in groups) {
            subset_data <- subset(data, data[group_var] == group)
            n <- nrow(subset_data)
            print(paste("Processing group:", group, "for variables:", var1, "and", var2))
            for (i in 1:n) {
              if (!is.na(subset_data[i, var1]) && !is.na(subset_data[i, var2])) {
                cross_product <- cross_product + (subset_data[i, var1] * subset_data[i, var2])
              }
            }
          }
        } else if (!is.null(gender_var)) {
          for (gender in genders) {
            subset_data <- subset(data, data[gender_var] == gender)
            n <- nrow(subset_data)
            for (i in 1:n) {
              if (!is.na(subset_data[i, var1]) && !is.na(subset_data[i, var2])) {
                cross_product <- cross_product + (subset_data[i, var1] * subset_data[i, var2])
              }
            }
          }
        } else {
          n <- nrow(data)
          print(paste("Processing all data for variables:", var1, "and", var2))
          for (i in 1:n) {
            if (!is.na(data[i, var1]) && !is.na(data[i, var2])) {
              cross_product <- cross_product + (data[i, var1] * data[i, var2])
            }
          }
        }
        
        results <- rbind(results, data.frame(Variable1 = var1, Variable2 = var2, CrossProduct = cross_product))
      }
    }
  }
  
  return(results)
}
# Calculate covariance
calculate_covariance <- function(data, var_names, group_var = NULL, gender_var = NULL) {
  # Filter data based on group and gender variables
  if (!is.null(group_var) && !is.null(gender_var)) {
    data <- subset(data, !is.na(data[group_var]) & !is.na(data[gender_var]))
    groups <- unique(data[group_var])
    genders <- unique(data[gender_var])
  } else if (!is.null(group_var)) {
    data <- subset(data, !is.na(data[group_var]))
    groups <- unique(data[group_var])
  } else if (!is.null(gender_var)) {
    data <- subset(data, !is.na(data[gender_var]))
    genders <- unique(data[gender_var])
  }
  
  # Calculate covariance
  results <- data.frame()
  
  for (var1 in var_names) {
    for (var2 in var_names) {
      if (var1 != var2) {
        covar <- 0
        
        if (!is.null(group_var) && !is.null(gender_var)) {
          for (group in groups) {
            for (gender in genders) {
              subset_data <- subset(data, data[group_var] == group & data[gender_var] == gender)
              n <- nrow(subset_data)
              if (n > 1) {
                mean_var1 <- mean(subset_data[, var1], na.rm = TRUE) # Ensure NA values are ignored in mean calculation
                mean_var2 <- mean(subset_data[, var2], na.rm = TRUE)
                covar <- covar + sum((subset_data[, var1] - mean_var1) * (subset_data[, var2] - mean_var2), na.rm = TRUE) / (n - 1)
              }
            }
          }
        } else if (!is.null(group_var)) {
          for (group in groups) {
            subset_data <- subset(data, data[group_var] == group)
            n <- nrow(subset_data)
            if (n > 1) {
              mean_var1 <- mean(subset_data[, var1], na.rm = TRUE)
              mean_var2 <- mean(subset_data[, var2], na.rm = TRUE)
              covar <- covar + sum((subset_data[, var1] - mean_var1) * (subset_data[, var2] - mean_var2), na.rm = TRUE) / (n - 1)
            }
          }
        } else if (!is.null(gender_var)) {
          for (gender in genders) {
            subset_data <- subset(data, data[gender_var] == gender)
            n <- nrow(subset_data)
            if (n > 1) {
              mean_var1 <- mean(subset_data[, var1], na.rm = TRUE)
              mean_var2 <- mean(subset_data[, var2], na.rm = TRUE)
              covar <- covar + sum((subset_data[, var1] - mean_var1) * (subset_data[, var2] - mean_var2), na.rm = TRUE) / (n - 1)
            }
          }
        } else {
          n <- nrow(data)
          if (n > 1) {
            mean_var1 <- mean(data[, var1], na.rm = TRUE)
            mean_var2 <- mean(data[, var2], na.rm = TRUE)
            covar <- covar + sum((data[, var1] - mean_var1) * (data[, var2] - mean_var2), na.rm = TRUE) / (n - 1)
          }
        }
        
        results <- rbind(results, data.frame(Variable1 = var1, Variable2 = var2, Covariance = covar))
      }
    }
  }
  
  return(results)
}

# Calculate correlation
calculate_correlation <- function(data, var_names, group_var = NULL, gender_var = NULL) {
  # Filter data based on group and gender variables
  if (!is.null(group_var) && !is.null(gender_var)) {
    data <- subset(data, !is.na(data[group_var]) & !is.na(data[gender_var]))
    groups <- unique(data[group_var])
    genders <- unique(data[gender_var])
  } else if (!is.null(group_var)) {
    data <- subset(data, !is.na(data[group_var]))
    groups <- unique(data[group_var])
  } else if (!is.null(gender_var)) {
    data <- subset(data, !is.na(data[gender_var]))
    genders <- unique(data[gender_var])
  }
  
  # Calculate correlation
  results <- data.frame()
  
  for (var1 in var_names) {
    for (var2 in var_names) {
      if (var1 != var2) {
        corr <- 0
        
        if (!is.null(group_var) && !is.null(gender_var)) {
          for (group in groups) {
            for (gender in genders) {
              subset_data <- subset(data, data[group_var] == group & data[gender_var] == gender)
              n <- nrow(subset_data)
              if (n > 1) {
                covar <- cov(subset_data[, var1], subset_data[, var2], use = "pairwise.complete.obs") # Handle NA values in cov calculation
                sd_var1 <- sd(subset_data[, var1], na.rm = TRUE) # Ensure NA values are ignored in sd calculation
                sd_var2 <- sd(subset_data[, var2], na.rm = TRUE)
                corr <- covar / (sd_var1 * sd_var2)
              }
            }
          }
        } else if (!is.null(group_var)) {
          for (group in groups) {
            subset_data <- subset(data, data[group_var] == group)
            n <- nrow(subset_data)
            if (n > 1) {
              covar <- cov(subset_data[, var1], subset_data[, var2], use = "pairwise.complete.obs")
              sd_var1 <- sd(subset_data[, var1], na.rm = TRUE)
              sd_var2 <- sd(subset_data[, var2], na.rm = TRUE)
              corr <- covar / (sd_var1 * sd_var2)
            }
          }
        } else if (!is.null(gender_var)) {
          for (gender in genders) {
            subset_data <- subset(data, data[gender_var] == gender)
            n <- nrow(subset_data)
            if (n > 1) {
              covar <- cov(subset_data[, var1], subset_data[, var2], use = "pairwise.complete.obs")
              sd_var1 <- sd(subset_data[, var1], na.rm = TRUE)
              sd_var2 <- sd(subset_data[, var2], na.rm = TRUE)
              corr <- covar / (sd_var1 * sd_var2)
            }
          }
        } else {
          n <- nrow(data)
          if (n > 1) {
            covar <- cov(data[, var1], data[, var2], use = "pairwise.complete.obs")
            sd_var1 <- sd(data[, var1], na.rm = TRUE)
            sd_var2 <- sd(data[, var2], na.rm = TRUE)
            corr <- covar / (sd_var1 * sd_var2)
          }
        }
        
        results <- rbind(results, data.frame(Variable1 = var1, Variable2 = var2, Correlation = corr))
      }
    }
  }
  
  return(results)
}

# ??apraz ??arp??mlar?? hesapla
cross_products <- calculate_cross_products(DatasetNA, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8"), "Group", "Gender")
print("Cross-Products:")
print(cross_products)

# Kovaryanslar?? hesapla
covariances <- calculate_covariance(DatasetNA, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8"), "Group", "Gender")
print("\nCovariances:")
print(covariances)

# Korelasyonlar?? hesapla
correlations <- calculate_correlation(DatasetNA, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8"), "Group", "Gender")
print("\nCorrelations:")
print(correlations)

