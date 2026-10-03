# Exponential Moving Average: recent values get more weight
ema <- function(data, period) {
  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)
  
  # Initialize a vector to store the EMA values
  ema_values <- numeric(length(data))
  
  # The first EMA equals the first data point, the rest use the EMA formula
  for (i in seq_along(data)) {
    if (i == 1) {
      ema_values[i] <- data[i]
    } else {
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }
  
  return(ema_values)
}