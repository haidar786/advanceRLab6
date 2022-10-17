#' greedy_knapsack problems is used for finding the best value over weight ratio of given data to the capacity of knapsack.
#' @param x This argument is contain a data frame of value and weight of items
#' @param W This is the capacity of knapsack
#' @return  This return the list contain value and elements
#' @examples
#' RNGversion(min(as.character(getRversion()),"3.5.3"))
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' n <- 2000
#' knapsack_objects <-
#' data.frame(w=sample(1:4000, size = n, replace = TRUE),
#' v=runif(n = n, 0, 10000))
#' greedy_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' @export

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


