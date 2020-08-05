# This experiment started because I got panicked about reproducing zipf's law
# In the end, the lessons I learned are (a) leave in stop words and infrequent
# words in the corpus when making the sample. (b) You want a bigger magnitude
# vector for beta. 1 * V seems good. (c) When you remove the infrequent words
# it chops of the tail end droop and you get a straight line just as Zipf's law
# would predict.

# A lingering issue is that an lda model fit on it is poor. The "best possible fit"
# measure of r-squared on the parameters that generate the model is also poor.
# I didn't have this issue with the r-squared topic models paper.

# UPDATE 2020-07-27
# Setting the magnitude of beta to 0.01 * V results in a good fitting and mostly
# coherent model. But the zipfs law plot leaves something to be desired.
# could this be because I'm not using the zpif's law to make beta? Could it also
# be that I need to have a bigger vocabulary to simulate, then cut it down?

# Anothe update: using zipf's law + 0.01 * V leads to much improved zipf plot.
# Also give acceptable R-squared. Coherence sucks for it and the colSums version though.
# That may have more to do with the coherence calculation which bombs if topics
# are full of stop words

# yet more update: doubling the number of topics pushes out the tail drop a bit
# but so far all simulated DTMs are way sparser than the data itself

# turns out that an asymmetric alpha results in more coherent topics learned from tidylda

library(tmsamples)
library(tidylda)

# d <- textmineR::nih_sample_dtm
# 
# d <- d[, colSums(d) >= 5]

# keep stopwords and full vocabulary in reconstruction
d <- textmineR::CreateDtm(
  doc_vec = textmineR::nih_sample$ABSTRACT_TEXT,
  doc_names = seq_len(nrow(textmineR::nih_sample)),
  ngram_window = c(1,1),
  stopword_vec = c()
)

beta <- colSums(d) / sum(d) * 0.01 * ncol(d)

# beta <- generate_zipf(ncol(d), magnitude = sum(d), zipf_par = 1.1)

# alpha <- gtools::rdirichlet(1, rep(0.1, 25)) + .Machine$double.eps
# 
# alpha <- as.numeric(alpha) / sum(alpha) * 2.5

alpha <- rep(0.2, 25)

par <- sample_parameters(alpha = alpha, beta = beta, num_documents = 1000)

doc_lengths <- rpois(1000, 500)

dsim <- sample_documents(
  theta = par$theta, 
  phi = par$phi, 
  doc_lengths = doc_lengths, 
  threads = 7
)

colnames(dsim) <- paste0("w_", seq_len(ncol(dsim)))

plot(log10(seq_len(ncol(dsim))), log10(sort(colSums(dsim), decreasing = TRUE)), type = "l")

# alpha2 <- rep(0.1, 25)
# 
# par2 <- sample_parameters(alpha2, beta, num_documents = 1000)
# 
# dsim2 <- sample_documents(
#   theta = par2$theta, 
#   phi = par2$phi, 
#   doc_lengths = doc_lengths,
#   threads = 7
# )
# 
# plot(log10(seq_len(ncol(dsim2))), log10(sort(colSums(dsim2), decreasing = TRUE)))
# 
# colnames(dsim2) <- colnames(par2$phi)
# 
lda_wrong <- tidylda(
  data = dsim,
  k = 25,
  iterations = 200,
  burnin = 150,
  alpha = 0.2,
  beta = 1,
  optimize_alpha = FALSE,
  calc_likelihood = TRUE,
  calc_r2 = TRUE,
  verbose = TRUE
)

lda_wrong

# 
lda_right <- tidylda(
  data = dsim2,
  k = 25,
  iterations = 200,
  burnin = 150,
  alpha = alpha,
  beta = beta,
  calc_likelihood = TRUE,
  calc_r2 = TRUE,
  verbose = TRUE
)
