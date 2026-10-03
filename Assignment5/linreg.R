# Linear regression over a window of points, returning slope, intercept and fitted values
linreg <- function(regressionSource, regressionLength, regressionOffset) {
  # Total number of elements in the source
  n <- length(regressionSource)
  
  # Check the length and offset arguments
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }
  if (regressionLength < 2) stop("regressionLength must be at least 2")
  
  # Window of regressionLength points that ends regressionOffset points before the last one
  start_index <- max(1, n - regressionLength + 1 - regressionOffset)
  end_index <- n - regressionOffset
  source_subset <- regressionSource[start_index:end_index]
  
  # Index values, their sums and their means
  index_values <- 1:length(source_subset)
  mean_index <- sum(index_values) / length(index_values)
  mean_source <- sum(source_subset) / length(source_subset)
  
  # Numerator and denominator of the least squares formula
  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  
  # Slope, intercept and fitted values
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  predicted_values <- slope * index_values + intercept
  
  # Return the three results as a list
  result <- list(slope = slope,
                 intercept = intercept,
                 predicted_values = predicted_values)
  return(result)
}