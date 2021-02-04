library(stm)
library(quanteda)
library(geometry)
library(Rtsne)
library(rsvd)

data("austensurvey")

austen_corpus <- Text.Replace::create_corpus(austensurvey, "Comment")

austen_2016_corpus <- quanteda::corpus_subset(austen_corpus, subset = Year == "2016")


austen_2016_dfm <- quanteda::dfm(austen_2016_corpus,
                                 remove_punct = T,
                                 remove = stopwords('en')) %>%
  quanteda::dfm_trim(min_termfreq = 0.95,
                     termfreq_type = "quantile",
                     max_docfreq = 0.1,
                     docfreq_type = "prop")


austen_2016_stm <- quanteda::convert(austen_2016_dfm,
                                     to = "stm")

set.seed(100)

austen_stm_spectral <- stm::searchK(documents = austen_2016_stm$documents,
                                   vocab = austen_2016_stm$vocab,
                                   K = 0,
                                   data = austen_2016_stm$meta,
                                   init.type = "Spectral")

#Update Central K based on above results

set.seed(100)

central_k = 34
offset_val = 2
interval = 1

k_list <- seq(central_k - offset_val, central_k + offset_val, 
              by = interval)

austen_stm_searchK <- stm::searchK(documents = austen_2016_stm$documents,
                                   vocab = austen_2016_stm$vocab,
                                   K = k_list,
                                   data = austen_2016_stm$meta)

plot(austen_stm_searchK)

# Evaluate heldout likelihood and residuals to determine k


many_models <- data.frame(K = k_list) %>% 
  dplyr::mutate(topic_model = future_map(K, ~stm(austen_2016_stm, K = ., 
                                                 verbose = TRUE, 
                                                 vocab = austen_2016_stm$vocab)))


