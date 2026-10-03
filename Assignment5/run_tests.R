# Load the functions
source("Assignment5/sma.R")
source("Assignment5/ema.R")
source("Assignment5/macd.R")
source("Assignment5/stdev.R")

# Data from the PDF examples
d <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
m <- c(100, 105, 110, 115, 120, 125, 130)

# SMA with a period of 3, checked against TTR::SMA
cat("SMA:\n"); print(round(sma(d, 3), 4))
cat("Matches TTR:", isTRUE(all.equal(sma(d, 3), as.numeric(na.omit(TTR::SMA(d, 3))))), "\n\n")

# EMA with a period of 3
cat("EMA:\n"); print(round(ema(d, 3), 4))

# MACD with periods 3, 5 and 2
r <- macd(m, 3, 5, 2)
cat("\nMACD line:\n"); print(round(r$macd_line, 4))
cat("Signal line:\n"); print(round(r$signal_line, 4))
cat("Histogram:\n"); print(round(r$histogram, 4))

# Standard deviation, checked against sd() adjusted to divide by n
cat("\nstdev:", round(stdev(d), 4), "\n")
cat("Matches sd() x sqrt((n-1)/n):",
    isTRUE(all.equal(stdev(d), sd(d) * sqrt((length(d) - 1) / length(d)))), "\n")