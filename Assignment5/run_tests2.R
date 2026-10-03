# Load the functions
source("Assignment5/linreg.R")
source("Assignment5/rsi.R")
source("Assignment5/stoch_rsi.R")
source("Assignment5/crossover.R")
source("Assignment5/crossunder.R")

# Linear regression on the last 5 points, then 5 points ending 2 before the end
d <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
r1 <- linreg(d, 5, 0)
cat("linreg(d, 5, 0): slope", r1$slope, "intercept", r1$intercept, "\n")
print(round(r1$predicted_values, 4))
r2 <- linreg(d, 5, 2)
cat("linreg(d, 5, 2): slope", r2$slope, "intercept", r2$intercept, "\n")
print(round(r2$predicted_values, 4))
r3 <- linreg(c(1, 2, 3, 4, 5), 5, 0)
cat("linreg(1:5): slope", r3$slope, "intercept", r3$intercept, "\n")

# Check against R's built-in lm()
fit <- lm(y ~ x, data = data.frame(x = 1:5, y = d[5:9]))
cat("Matches lm():", isTRUE(all.equal(c(r1$intercept, r1$slope), as.numeric(coef(fit)))), "\n\n")

# RSI with a period of 5, and a range check
p <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
r <- rsi(p, 5)
cat("RSI:\n"); print(round(r, 4))
cat("All RSI values between 0 and 100:", all(r[!is.na(r)] >= 0 & r[!is.na(r)] <= 100), "\n\n")

# StochRSI with the RSI period 5
s <- stoch_rsi(p, 5, 3, 3)
cat("StochRSI k_line:\n"); print(round(s$k_line, 4))
cat("StochRSI d_line:\n"); print(round(s$d_line, 4))
s14 <- stoch_rsi(p, 14, 3, 3)
cat("PDF example (period 14), all NA:", all(is.na(s14$k_line)), "\n\n")

# Crossover and crossunder with the PDF's arrays
a1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
a2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
cat("crossover:\n"); print(crossover(a1, a2))
cat("crossunder:\n"); print(crossunder(a1, a2))

# A downward cross
cat("crossover down:\n"); print(crossover(c(5, 6, 3, 2), c(4, 4, 4, 4)))
cat("crossunder down:\n"); print(crossunder(c(5, 6, 3, 2), c(4, 4, 4, 4)))