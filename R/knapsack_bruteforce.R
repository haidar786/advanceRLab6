brute_force_knapsack <- function(x, W, ...) {
  stopifnot(is.data.frame(x))
  stopifnot(W > 0)

  data <- x
  lengthOfCombinations <- 2^nrow(df)
  df <- data.frame()
  for (i in 1:nrow(data)) {
    elementCombinations <- combn(rownames(data), i, simplify = FALSE)
    combinedWeight <- colSums(combn(data$w, i))
    combinedValue <- colSums(combn(data$v, i))
    newDF <- data.frame(combinedWeight, combinedValue, I(elementCombinations))
    df <- rbind(df, newDF)
  }
  df <- df[order(df$combinedValue, decreasing = TRUE),]
  left_weight <- W
  optimal_value <- 0
  elemets <- c()
  for (i in 1:nrow(df)) {
    row <- df[i,]
    if (left_weight > row$combinedWeight) {
      optimal_value <- optimal_value + row$combinedValue
      left_weight <- left_weight - row$combinedWeight
      elemets <- c(elemets, unlist(row$elementCombinations))
    }
  }
  return(list("value"=round(optimal_value), "elements" = as.numeric(elemets)))
}

