library(shiny)
library(ggplot2)
library(quantmod)

# Find a project file whether the app runs from the project folder or from its own folder
find_file <- function(path) {
  for (p in c(path, file.path("..", path))) if (file.exists(p)) return(p)
  NULL
}

# Load my Assignment 5 indicator functions (ema.R must come before macd.R)
for (f in c("ema.R", "sma.R", "macd.R", "rsi.R", "crossover.R", "crossunder.R")) {
  path <- find_file(file.path("Assignment5", f))
  if (is.null(path)) stop("Cannot find Assignment5/", f, ". Run the app from the TechnicalAnalysis project.")
  source(path)
}

# Stock symbols from the Assignment 2 portfolio file, with a fallback list
portfolio_path <- find_file("Assignment2/portfolio.txt")
symbols <- if (!is.null(portfolio_path)) trimws(readLines(portfolio_path, warn = FALSE)) else
  c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA")
symbols <- symbols[symbols != ""]

# Download daily prices from Yahoo Finance as a data frame, or NULL if anything fails
fetch_stock <- function(symbol, start_date, end_date) {
  tryCatch({
    x <- getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    df <- data.frame(Date = index(x), coredata(x))
    colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    df <- df[complete.cases(df[, c("Open", "High", "Low", "Close")]), ]
    if (nrow(df) == 0) stop("No data returned")
    df
  }, error = function(e) NULL)
}

# Combine daily bars into weekly or monthly bars
aggregate_bars <- function(df, time_frame) {
  if (time_frame == "Daily") return(df)
  groups <- droplevels(cut(df$Date, if (time_frame == "Weekly") "week" else "month"))
  bars <- lapply(split(df, groups), function(g) {
    data.frame(Date = max(g$Date), Open = g$Open[1], High = max(g$High),
               Low = min(g$Low), Close = g$Close[nrow(g)], Volume = sum(g$Volume))
  })
  bars <- do.call(rbind, bars)
  rownames(bars) <- NULL
  bars
}

# Add the indicator columns using my Assignment 5 functions
add_indicators <- function(df, p) {
  n <- nrow(df)
  pad_sma <- function(period) {
    if (n >= period) c(rep(NA_real_, period - 1), sma(df$Close, period)) else rep(NA_real_, n)
  }
  df$MA_short <- pad_sma(p$short_ma)
  df$MA_long <- pad_sma(p$long_ma)
  df$RSI <- rsi(df$Close, p$rsi_period)
  m <- macd(df$Close, p$macd_fast, p$macd_slow, p$macd_signal)
  df$MACD <- m$macd_line
  df$MACD_signal <- m$signal_line
  df$MACD_hist <- m$histogram
  df
}

# Trading rule: Buy when the short MA crosses above the long MA, Sell when it crosses below, otherwise Hold
find_signals <- function(df) {
  up <- crossover(df$MA_short, df$MA_long)
  down <- crossunder(df$MA_short, df$MA_long)
  df$Trade <- ifelse(up == "Up", "Buy", ifelse(down == "True", "Sell", "Hold"))
  df
}

# Draw the price as a candlestick, line or area chart
price_chart <- function(df, chart_type, symbol) {
  if (chart_type == "Candlestick") {
    w <- as.numeric(median(diff(df$Date))) * 0.35
    df$Direction <- ifelse(df$Close >= df$Open, "Up", "Down")
    p <- ggplot(df, aes(x = Date, color = Direction, fill = Direction)) +
      geom_linerange(aes(ymin = Low, ymax = High)) +
      geom_rect(aes(xmin = Date - w, xmax = Date + w,
                    ymin = pmin(Open, Close), ymax = pmax(Open, Close))) +
      scale_color_manual(values = c(Up = "#2e8b57", Down = "#c0392b")) +
      scale_fill_manual(values = c(Up = "#2e8b57", Down = "#c0392b"))
  } else if (chart_type == "Area") {
    p <- ggplot(df, aes(x = Date, y = Close)) +
      geom_area(fill = "steelblue", alpha = 0.3) +
      geom_line(color = "steelblue")
  } else {
    p <- ggplot(df, aes(x = Date, y = Close)) + geom_line(color = "steelblue")
  }
  p + labs(title = paste(symbol, chart_type, "chart"), x = NULL, y = "Price (USD)") +
    theme_minimal()
}

# Overlay the two moving averages on the price chart
add_ma_layers <- function(p, df, short_ma, long_ma) {
  p +
    geom_line(data = df, aes(x = Date, y = MA_short), color = "#e67e22", linewidth = 0.9,
              na.rm = TRUE, inherit.aes = FALSE) +
    geom_line(data = df, aes(x = Date, y = MA_long), color = "#8e44ad", linewidth = 0.9,
              na.rm = TRUE, inherit.aes = FALSE) +
    labs(subtitle = paste0("Orange: ", short_ma, "-bar moving average    Purple: ", long_ma, "-bar moving average"))
}

# Mark the bars where the rule gives a Buy or Sell signal
add_signal_layers <- function(p, df) {
  buys <- df[df$Trade == "Buy", ]
  sells <- df[df$Trade == "Sell", ]
  p +
    geom_point(data = buys, aes(x = Date, y = Low), shape = 24, fill = "#1e8449", color = "black",
               size = 3.5, inherit.aes = FALSE) +
    geom_text(data = buys, aes(x = Date, y = Low, label = "Buy"), color = "#1e8449",
              fontface = "bold", vjust = 2.3, inherit.aes = FALSE) +
    geom_point(data = sells, aes(x = Date, y = High), shape = 25, fill = "#c0392b", color = "black",
               size = 3.5, inherit.aes = FALSE) +
    geom_text(data = sells, aes(x = Date, y = High, label = "Sell"), color = "#c0392b",
              fontface = "bold", vjust = -1.6, inherit.aes = FALSE)
}

# User interface: data widgets, indicator toggles, indicator settings and trading rule options
ui <- fluidPage(
  titlePanel("Portfolio Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("symbol", "Stock symbol (type to add another):", choices = symbols,
                     selected = symbols[1], options = list(create = TRUE)),
      dateRangeInput("date_range", "Select Date Range:",
                     start = Sys.Date() - 365, end = Sys.Date(), max = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:", choices = c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Chart Type:", choices = c("Candlestick", "Line", "Area")),
      hr(),
      checkboxGroupInput("indicators", "Technical indicators:",
                         choices = c("Moving Averages", "RSI", "MACD"), selected = "Moving Averages"),
      numericInput("short_ma", "Short moving average (bars):", value = 20, min = 2, max = 200),
      numericInput("long_ma", "Long moving average (bars):", value = 50, min = 3, max = 400),
      conditionalPanel("input.indicators && input.indicators.indexOf('RSI') > -1",
                       numericInput("rsi_period", "RSI period:", value = 14, min = 2, max = 100)),
      conditionalPanel("input.indicators && input.indicators.indexOf('MACD') > -1",
                       numericInput("macd_fast", "MACD fast period:", value = 12, min = 2, max = 100),
                       numericInput("macd_slow", "MACD slow period:", value = 26, min = 3, max = 200),
                       numericInput("macd_signal", "MACD signal period:", value = 9, min = 2, max = 100)),
      hr(),
      checkboxInput("show_signals", "Show Buy/Sell signals (moving average crossover)", value = TRUE),
      helpText("Periods are counted in bars: days, weeks or months, depending on the time frame.")
    ),
    mainPanel(
      plotOutput("stock_chart", height = "450px"),
      conditionalPanel("input.indicators && input.indicators.indexOf('RSI') > -1",
                       plotOutput("rsi_chart", height = "200px")),
      conditionalPanel("input.indicators && input.indicators.indexOf('MACD') > -1",
                       plotOutput("macd_chart", height = "220px")),
      h4("Buy/Sell signals"),
      tableOutput("signal_table")
    )
  )
)

# Server: download, aggregate, calculate indicators and signals, then draw each output
server <- function(input, output) {
  stock_data <- reactive({
    req(input$symbol, input$date_range)
    validate(need(input$date_range[1] < input$date_range[2], "The start date must be before the end date."))
    df <- fetch_stock(toupper(input$symbol), input$date_range[1] - 400, input$date_range[2])
    validate(need(!is.null(df), "Could not download data. Check the symbol and your internet connection."))
    aggregate_bars(df, input$time_frame)
  })
  
  analysis <- reactive({
    req(input$short_ma, input$long_ma, input$rsi_period, input$macd_fast, input$macd_slow, input$macd_signal)
    validate(need(input$short_ma < input$long_ma, "The short moving average must be smaller than the long one."))
    validate(need(input$macd_fast < input$macd_slow, "The MACD fast period must be smaller than the slow period."))
    params <- list(short_ma = input$short_ma, long_ma = input$long_ma, rsi_period = input$rsi_period,
                   macd_fast = input$macd_fast, macd_slow = input$macd_slow, macd_signal = input$macd_signal)
    df <- find_signals(add_indicators(stock_data(), params))
    df[df$Date >= input$date_range[1] & df$Date <= input$date_range[2], ]
  })
  
  enough <- function(df) validate(need(nrow(df) >= 2, "Not enough data in this range. Widen the dates or use a shorter time frame."))
  
  output$stock_chart <- renderPlot({
    df <- analysis()
    enough(df)
    p <- price_chart(df, input$chart_type, toupper(input$symbol))
    if ("Moving Averages" %in% input$indicators) p <- add_ma_layers(p, df, input$short_ma, input$long_ma)
    if (isTRUE(input$show_signals)) p <- add_signal_layers(p, df)
    p
  })
  
  output$rsi_chart <- renderPlot({
    df <- analysis()
    enough(df)
    ggplot(df, aes(x = Date, y = RSI)) +
      geom_hline(yintercept = 70, linetype = "dashed", color = "#c0392b") +
      geom_hline(yintercept = 30, linetype = "dashed", color = "#2e8b57") +
      geom_line(color = "#2c3e50", na.rm = TRUE) +
      scale_y_continuous(limits = c(0, 100)) +
      labs(title = paste0("RSI (", input$rsi_period, "): above 70 overbought, below 30 oversold"),
           x = NULL, y = "RSI") +
      theme_minimal()
  })
  
  output$macd_chart <- renderPlot({
    df <- analysis()
    enough(df)
    ggplot(df, aes(x = Date)) +
      geom_col(aes(y = MACD_hist), fill = "grey70", na.rm = TRUE) +
      geom_line(aes(y = MACD), color = "#2980b9", na.rm = TRUE) +
      geom_line(aes(y = MACD_signal), color = "#e67e22", na.rm = TRUE) +
      labs(title = "MACD (blue), signal line (orange) and histogram (grey)", x = NULL, y = "MACD") +
      theme_minimal()
  })
  
  output$signal_table <- renderTable({
    df <- analysis()
    sig <- df[df$Trade != "Hold", c("Date", "Trade", "Close")]
    validate(need(nrow(sig) > 0, "No crossover signals in this date range."))
    sig$Date <- format(sig$Date)
    sig$Close <- round(sig$Close, 2)
    sig
  })
}

shinyApp(ui, server)