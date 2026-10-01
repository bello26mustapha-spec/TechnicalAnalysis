# stock_functions.R
# Assignment 2: load stock data and compute basic statistics

library(quantmod)
library(TTR)

# Read symbols from portfolio.txt and return one data frame per stock
load_stock_data <- function(portfolio_file = "Assignment2/portfolio.txt",
                            start_date = "2025-01-01",
                            end_date = "2025-12-31") {
  
  # Stop if the portfolio file is missing
  if (!file.exists(portfolio_file)) {
    stop("Portfolio file not found: ", portfolio_file)
  }
  
  # Read one symbol per line, dropping blank lines
  symbols <- trimws(readLines(portfolio_file, warn = FALSE))
  symbols <- symbols[symbols != ""]
  
  # Download each symbol and store it as a data frame
  stock_list <- list()
  for (sym in symbols) {
    result <- tryCatch({
      xts_data <- getSymbols(sym, src = "yahoo",
                             from = start_date, to = end_date,
                             auto.assign = FALSE)
      df <- data.frame(Date = index(xts_data), coredata(xts_data))
      colnames(df) <- c("Date", "Open", "High", "Low",
                        "Close", "Volume", "Adjusted")
      df
    }, error = function(e) {
      message("Could not load ", sym, ": ", conditionMessage(e))
      NULL
    })
    if (!is.null(result)) stock_list[[sym]] <- result
  }
  
  return(stock_list)
}

# Most frequent closing price (rounded to cents, since prices are continuous)
get_mode <- function(x) {
  x <- round(x, 2)
  counts <- table(x)
  as.numeric(names(counts)[which.max(counts)])
}

# Calculate basic statistics on the closing price of one stock
calculate_statistics <- function(stock_df, ma_period = 20) {
  
  # Check the input is a data frame with a Close column
  if (!is.data.frame(stock_df) || !"Close" %in% names(stock_df)) {
    stop("stock_df must be a data frame with a Close column")
  }
  
  close_prices <- stock_df$Close
  
  # Collect all results in one list
  stats <- list(
    mean           = mean(close_prices, na.rm = TRUE),
    median         = median(close_prices, na.rm = TRUE),
    mode           = get_mode(close_prices),
    std_dev        = sd(close_prices, na.rm = TRUE),
    moving_average = SMA(close_prices, n = ma_period)
  )
  
  return(stats)
}

# Build one summary table with a row per stock
summarize_portfolio <- function(stock_list) {
  rows <- lapply(names(stock_list), function(sym) {
    s <- calculate_statistics(stock_list[[sym]])
    data.frame(Symbol = sym,
               Mean = round(s$mean, 2),
               Median = round(s$median, 2),
               Mode = s$mode,
               Std_Dev = round(s$std_dev, 2),
               Latest_MA20 = round(tail(s$moving_average, 1), 2))
  })
  do.call(rbind, rows)
}