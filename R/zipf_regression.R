# generate some samples based on zipf's law parameter coming from data
library(tidyverse)
library(tmsamples)

# sample data
dtm <- textmineR::CreateDtm(
  doc_vec = textmineR::nih_sample$ABSTRACT_TEXT,
  doc_names = 1:nrow(textmineR::nih_sample),
  ngram_window = c(1, 2),
  stopword_vec = c(),
  cpus = 7
)

# estimate zipf function
f <- local({
  y <- sort(colSums(dtm), decreasing = TRUE)
  x <- seq_along(y)
  lm(I(log10(y)) ~ I(log10(x)))
})

# declare some sampling parameters
Nk <- 50

Nd <- 2000

doc_lengths <- sample(rowSums(dtm), Nd, replace = TRUE)

beta <- predict(f, data.frame(x = rank(-1 * colSums(dtm)))) 
                
beta <- 10 ^ beta

beta <- beta / sum(beta) * ncol(dtm) * 1 # magnitude is tuneable

alpha <- rep(5 / Nk, Nk) # symmetric alpha

# sample some documents
par <- sample_parameters(
  alpha = alpha,
  beta = beta,
  num_documents = Nd
)

docs <- sample_documents(
  theta = par$theta,
  phi = par$phi,
  doc_lengths = doc_lengths,
  verbose = TRUE,
  threads = 7
)

colnames(docs) <- paste0("w_", seq_len(ncol(docs)))

plot(log10(seq_len(ncol(docs))), log10(sort(colSums(docs), decreasing = TRUE)), type = "l")




