my_dir <- function(n, alpha) {
  result <- sapply(alpha, function(a) rgamma(n, a))
  if (n > 1) {
    return(result / rowSums(result))
  }
  result / sum(result)
}


set.seed(123)

samp_my <- my_dir(10, rep(0.01, 20))

set.seed(123)

samp_gt <- gtools::rdirichlet(10, rep(0.01, 20))
