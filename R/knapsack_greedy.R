knapsack_greedy <- function(data, weight) {
  df <- data
  max_values <- data$v / data$w
  df$maxValue <- max_values
  df <- df[order(df$maxValue, decreasing = TRUE),]
  left_weight <- weight
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
  return(list("value"=optimal_value, "elements" = as.numeric(elements)))
}
