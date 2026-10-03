# MACD: difference of two EMAs, a signal line and a histogram
if (!exists("ema")) source("Assignment5/ema.R")

macd <- function(data, short_period, long_period, signal_period) {
  # Calculate the short-term and long-term EMAs
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)
  
  # MACD line, signal line (EMA of the MACD line) and histogram
  macd_line <- short_ema - long_ema
  signal_line <- ema(macd_line, signal_period)
  histogram <- macd_line - signal_line
  
  # Return the three results as a list
  result <- list(macd_line = macd_line,
                 signal_line = signal_line,
                 histogram = histogram)
  return(result)
}