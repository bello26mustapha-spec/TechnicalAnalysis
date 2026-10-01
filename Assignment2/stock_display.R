# Assignment 2: display imported data and visualizations

source("Assignment2/stock_functions.R")
library(ggplot2)

# Print the first and last rows of every stock
display_stock_data <- function(stock_list, n = 5) {
  for (sym in names(stock_list)) {
    cat("\nStock:", sym, "\n")
    print(head(stock_list[[sym]], n))
    print(tail(stock_list[[sym]], n))
  }
}

# Line chart of the closing price with its moving average
plot_closing_price <- function(stock_df, symbol, ma_period = 20) {
  stock_df$MA <- SMA(stock_df$Close, n = ma_period)
  ggplot(stock_df, aes(x = Date)) +
    geom_line(aes(y = Close, color = "Close")) +
    geom_line(aes(y = MA, color = paste0(ma_period, "-day MA")), na.rm = TRUE) +
    labs(title = paste(symbol, "Closing Price and Moving Average"),
         y = "Price (USD)", color = "")
}

# Bar chart of daily trading volume
plot_volume <- function(stock_df, symbol) {
  ggplot(stock_df, aes(x = Date, y = Volume)) +
    geom_col(fill = "steelblue") +
    labs(title = paste(symbol, "Daily Trading Volume"), y = "Shares traded")
}

# Bar chart comparing the mean closing price of all stocks
plot_mean_prices <- function(stock_list) {
  summary_df <- summarize_portfolio(stock_list)
  ggplot(summary_df, aes(x = Symbol, y = Mean, fill = Symbol)) +
    geom_col() +
    geom_errorbar(aes(ymin = Mean - Std_Dev, ymax = Mean + Std_Dev), width = 0.3) +
    labs(title = "Mean Closing Price by Stock (error bars = 1 std dev)",
         y = "Price (USD)")
}

# Runs everything for the whole portfolio
stocks <- load_stock_data()
display_stock_data(stocks)
print(summarize_portfolio(stocks))
for (sym in names(stocks)) {
  print(plot_closing_price(stocks[[sym]], sym))
  print(plot_volume(stocks[[sym]], sym))
}
print(plot_mean_prices(stocks))