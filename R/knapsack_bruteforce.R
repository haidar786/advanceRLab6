brute_force_knapsack <- function(x, W, ...) {
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
      elemets <- c(elemets, row$elementCombinations)
    }
  }
  print(class(row$elementCombinations))
  return(list("value"=optimal_value, "elements" = elemets))
}


RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
