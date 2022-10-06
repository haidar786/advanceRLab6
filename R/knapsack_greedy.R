knapsack_greedy <- function(data, weight) {
  df <- data
  max_values <- data$v / data$w
  df$maxValue <- max_values
  df <- df[order(df$maxValue, decreasing = TRUE),]
  left_weught <- weight
  optimal_value <- 0
  elemets <- c()
  for (i in 1:nrow(df)) {
    row <- df[i,]
      if (left_weught > row$w) {
        optimal_value <- optimal_value + row$v
        left_weught <- left_weught - row$w
        elemets <- c(elemets, rownames(row))
      }
  }
  return(list("value"=optimal_value, "elements" = as.numeric(elemets)))
}
