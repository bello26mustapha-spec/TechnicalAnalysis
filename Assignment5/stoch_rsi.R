# Stochastic RSI: RSI scaled between its lowest and highest value, then smoothed
if (!exists("rsi")) source("Assignment5/rsi.R")
if (!exists("sma")) source("Assignment5/sma.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)
  
  # Scale the RSI between 0 and 1 (all NA if there is not enough data)
  valid <- rsi_values[!is.na(rsi_values)]
  if (length(valid) == 0) {
    k_values <- rep(NA_real_, length(rsi_values))
  } else {
    min_rsi <- min(valid)
    max_rsi <- max(valid)
    k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  }
  
  # %K line is the moving average of the scaled values, %D is the moving average of %K
  k_line <- sma(k_values, k_period)
  d_line <- sma(k_line, d_period)
  
  # Return the two lines as a list
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}