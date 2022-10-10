greedy_knapsack <- function(x, W) {
  df <- x
  print(df)
  max_values <- df$v / df$w
  df$maxValue <- max_values
  df <- df[order(df$maxValue, decreasing = TRUE),]
  left_weight <- W
  optimal_value <- 0
  elements <- c()
  for (i in 1:nrow(df)) {
    row <- df[i,]
    if (left_weight > row$w) {
      optimal_value <- optimal_value + row$v
      left_weight <- left_weight - row$w
      elements <- c(elements, rownames(row))
    }
  }
  numericElements <- as.numeric(elements)
  tail <- tail(numericElements, n=1)
  lastIndex <- length(numericElements)
  optimal_value <- as.numeric(optimal_value) - as.numeric(x$v[tail])
  return(list("value"=round(optimal_value), "elements" = numericElements[-35]))
}

