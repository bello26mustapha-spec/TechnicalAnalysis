# Relative Strength Index with Wilder's smoothing
rsi <- function(data, period) {
  # Differences between consecutive data points
  diff_values <- diff(data)
  
  # Gains are the positive changes, losses are the size of the other changes
  gains <- ifelse(diff_values > 0, diff_values, 0)
  losses <- ifelse(diff_values > 0, 0, abs(diff_values))
  
  # RSI vector filled with NA until there is enough data
  rsi_values <- rep(NA_real_, length(data))
  
  # Start from the average of the first 'period' changes, then smooth
  if (length(data) > period) {
    avg_gain <- sum(gains[1:period]) / period
    avg_loss <- sum(losses[1:period]) / period
    
    for (i in (period + 1):length(data)) {
      avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
      avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }
  
  return(rsi_values)
}