greedy_knapsack <- function(x, W) {
  stopifnot(is.data.frame(x))
  stopifnot(W > 0)
  df <- x
  max_values <- df$v / df$w
  df$maxValue <- max_values
  rn <- rownames(df)
  df$elements <- rn
  df <- df[order(df$maxValue, decreasing = TRUE),]
  rownames(df) <- rn
  left_weight <- W
  optimal_value <- 0
  elements <- c()
  index <- 1
  repeat {
    if (left_weight >= df$w[index]) {
      optimal_value <- optimal_value + df$v[index]
      left_weight <- left_weight - df$w[index]
      elements <- c(elements, df$elements[index])
    }else {
      break
    }
    index <- index + 1
  }
  return(list("value"=round(optimal_value), "elements" = as.numeric(elements)))
}


