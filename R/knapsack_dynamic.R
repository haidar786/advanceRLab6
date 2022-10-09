knapsack_dynamic <- function(x, W) {
  data <- x
  mat <- matrix(nrow=nrow(data), ncol=W)
  k_recursive <- function(n, C) {
    if (isTRUE(mat[n,C])) {
      return(mat[n,C])
    }
    res <- 0
    if (n == 0 | C == 0) {
      res <- 0
    }else if (data$w[n] > C) {
      res <- k_recursive(n-1, C)
    }else {
      temp1 <- k_recursive(n-1, C)
      temp2 <- data$v[n] + k_recursive(n-1, C - data$w[n])
      res <- max(temp1, temp2)
    }
    mat[n,C] <- res
    return(res)
  }
  return(list(value = k_recursive(n = nrow(data), C = W)))
}

