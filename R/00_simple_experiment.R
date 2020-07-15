library(tmsamples)

library(tidyverse)

data <- textmineR::nih_sample_dtm

summary(colSums(data))

data <- data[, colSums(data) >= 5]

par <- sample_parameters(
  rep(0.1, 20), 
  beta = colSums(data) * 100, 
  num_documents = 100
)

sims <- lapply(1:100, function(x){
  sim <- 
    sample_documents(
      theta = par$theta, 
      phi = par$phi, 
      doc_lengths = rowSums(data), 
      verbose = T, 
      threads = 20
    )
  
  colnames(sim) <- paste0("w", seq_len(ncol(sim)))
  
  sim
})


compare_fun <- function(data, sim) {
  
  rank_data <- colnames(data)[order(colSums(data), decreasing = TRUE)]
  
  rank_sim <- colnames(sim)[order(colSums(sim), decreasing = TRUE)]
  
  result <- map2(
    .x = rank_data,
    .y = rank_sim,
    .f = function(.x, .y) {
      ct <- ks.test(data[, .x], sim[, .y])
      
      ct$p.value
    }
  ) 
  
  unlist(result)
}

sim_comparisons <- parallel::mclapply(
  sims,
  function(sim) {
    compare_fun(data = data, sim = sim)
  },
  mc.cores = 22
)

names(sim_comparisons) <- paste0("s", seq_along(sim_comparisons))

sim_comparisons <- as_tibble(do.call(cbind, sim_comparisons))

# With a p-value of 0.05, we'd expect 5% of these to be less than 0.05 
# by random chance
pct_miss <- sapply(sim_comparisons, function(x) sum(x <= 0.05) / length(x))


# ugly document level plot
plot(log10(seq_len(ncol(data))), log10(sort(data[12,], decreasing = TRUE)))
for (j in seq_along(sims)) {
  lines(
    log10(seq_len(ncol(sims[[j]]))), 
    log10(sort(sims[[j]][12, ], decreasing = TRUE)),
    col = j,
    lwd = 2)
}

# ugly corpus plot
plot(log10(seq_len(ncol(data))), log10(sort(colSums(data), decreasing = TRUE)))
for (j in seq_along(sims)) {
  lines(
    log10(seq_len(ncol(sims[[j]]))), 
    log10(sort(colSums(sims[[j]]), decreasing = TRUE)),
    col = j,
    lwd = 2)
}

