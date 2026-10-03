# Simple Moving Average: the mean of each window of 'period' values
sma <- function(data, period) {
  # Stop if the data is shorter than the period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  # Slide a window across the data and store the mean of each window
  sma_values <- numeric(length(data) - period + 1)
  for (i in 1:(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i] <- sum(current_window) / period
  }
  
  return(sma_values)
}