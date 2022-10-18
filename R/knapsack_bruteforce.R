library(foreach)
library(doParallel)
library(parallel)
library(profvis)

#' Brute forcing knapsack problems is used for finding all the possible combinations of given data.
#' @param x This argument is contain a data frame of value and weight of items
#' @param W This is the capacity of knapsack
#' @param parallel This parameter is to run some code in parallel core or not
#' @import foreach doParallel parallel profvis
#' @importFrom utils combn
#' @return  This return the list contain value and elements
#' @examples
#' RNGversion(min(as.character(getRversion()),"3.5.3"))
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' n <- 2000
#' knapsack_objects <-
#' data.frame(w=sample(1:4000, size = n, replace = TRUE),
#' v=runif(n = n, 0, 10000))
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' @export

brute_force_knapsack <- function(x, W, parallel = FALSE) {
  stopifnot(is.data.frame(x))
  stopifnot(W > 0)

  data <- x
  lengthOfCombinations <- 2^nrow(df)
  df <- data.frame()
  if (parallel == FALSE) {
    df <- foreach (i=1:nrow(data), .combine = rbind) %do% {
      elementCombinations <- combn(rownames(data), i, simplify = FALSE)
      combinedWeight <- colSums(combn(data$w, i))
      combinedValue <- colSums(combn(data$v, i))
      newDF <- data.frame(combinedWeight, combinedValue, I(elementCombinations))
      newDF
    }
  }else {
    numCores = detectCores()
    registerDoParallel(makeCluster(numCores))
    df <- foreach (i=1:nrow(data), .combine = rbind) %dopar% {
      elementCombinations <- combn(rownames(data), i, simplify = FALSE)
      combinedWeight <- colSums(combn(data$w, i))
      combinedValue <- colSums(combn(data$v, i))
      newDF <- data.frame(combinedWeight, combinedValue, I(elementCombinations))
      newDF
    }
    stopImplicitCluster()
  }
  df <- df[order(df$combinedValue, decreasing = TRUE),]
  left_weight <- W
  optimal_value <- 0
  elemets <- c()
  starts <- rep(100, 40)
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
