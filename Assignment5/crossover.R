# Crossover: "Up" where arr1 crosses above arr2, "Down" where it crosses below, otherwise "None"
crossover <- function(arr1, arr2) {
  # Both arrays must have the same length
  if (length(arr1) != length(arr2)) stop("Both arrays should have the same length")
  
  # Start with "None" for every position
  crossover_signals <- rep("None", length(arr1))
  
  # Check each position against the previous one (isTRUE ignores missing values)
  for (i in seq_along(arr1)[-1]) {
    if (isTRUE(arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1])) {
      crossover_signals[i] <- "Up"
    } else if (isTRUE(arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1])) {
      crossover_signals[i] <- "Down"
    }
  }
  
  return(crossover_signals)
}