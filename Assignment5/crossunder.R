# Crossunder: "True" where arr1 crosses under arr2, otherwise "False" (first position is "None")
crossunder <- function(arr1, arr2) {
  # Both arrays must have the same length
  if (length(arr1) != length(arr2)) stop("Both arrays should have the same length")
  
  # The first position has no previous value to compare with
  crossunder_signals <- rep("False", length(arr1))
  crossunder_signals[1] <- "None"
  
  # Check each position against the previous one (isTRUE ignores missing values)
  for (i in seq_along(arr1)[-1]) {
    if (isTRUE(arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1])) {
      crossunder_signals[i] <- "True"
    }
  }
  
  return(crossunder_signals)
}