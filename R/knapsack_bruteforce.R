knapsack_bruteforce <- function(data, capacity) {
  df <- data
  m <- sapply(as.numeric(data$w), function(x){ as.integer(intToBits(x))})
  print(m)
}














RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

knapsack_bruteforce(knapsack_objects[1:8,], 3500)
