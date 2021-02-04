library(Text.Replace)
library(textmineR)
library(text2vec)
library(tidytext)
library(BTM)


data("austensurvey")

k_topics = 35

n_terms = 10

### LDA model

austen_corpus <- Text.Replace::create_corpus(austensurvey, "Comment")

austen_2016_corpus <- quanteda::corpus_subset(austen_corpus,
                                              subset = Year == "2016")

austen_2016_dtm <- textmineR::CreateDtm(doc_vec = austen_2016_corpus,
                                   doc_names = austen_2016_corpus$docname,
                                   ngram_window = c(1,6))

set.seed(100)

austen_2016_lda = textmineR::FitLdaModel(dtm = austen_2016_dtm, 
                                         k = k_topics, 
                                         iterations = 200, 
                                         burnin = 180,
                                         alpha = 0.1,
                                         beta = colSums(austen_2016_dtm) / sum(austen_2016_dtm) * 100,
                                         optimize_alpha = TRUE,
                                         calc_likelihood = TRUE,
                                         calc_coherence = TRUE,
                                         calc_r2 = FALSE,
                                         cpus = 2)

austen_2016_lda.TopWords <- textmineR::GetTopTerms(austen_2016_lda$phi, 
                                                   M = n_terms)

austen_2016_tcm <- textmineR::Dtm2Tcm(austen_2016_dtm)

austen_2016_lda.Coherence <- text2vec::coherence(x = austen_2016_lda.TopWords,
                                                 tcm = austen_2016_tcm,
                                                 n_doc_tcm = dim(austen_2016_dtm)[1])


# BTM Model

austen_2016_df <- subset(austensurvey, subset = Year == 2016)

austen_2016_tibble <- tibble::tibble(id = base::seq_along(austen_2016_df$Comment),
                                text = austen_2016_df$Comment)

austen_2016_tibble_cleaned <- austen_2016_tibble %>%
  tidytext::unnest_tokens(word, text, token = 'words')

# Remove stopwords using the "snowball" repository
austen_2016_tibble_cleaned <- dplyr::anti_join(austen_2016_tibble_cleaned,
                                          by = "word",
                                          tidytext::get_stopwords(source ="snowball"))



austen_2016_btm = BTM::BTM(austen_2016_tibble_cleaned,
                      k = k_topics,
                      alpha = 10,
                      beta = 0.1,
                      iter = 2000,
                      window = 15,
                      background = TRUE,
                      trace = 200,
                      detailed = TRUE
)

austen_2016_btm.topWords = BTM:::terms.BTM(austen_2016_btm, top_n = n_terms)

# austen_2016_btm.topBiterms = BTM:::terms.BTM(austen_2016_btm, type = "biterms",
#                                              top_n = n_terms)

austen_2016_btm.topWords.vec = matrix(, nrow = n_terms, ncol = k_topics)

# austen_2016_btm.topBiterms.vec = matrix(, nrow = n_terms, ncol = k_topics)

for(i in 1:k_topics){
  austen_2016_btm.topWords.vec[,i] <- austen_2016_btm.topWords[[i]][,1]
  # austen_2016_btm.topBiterms.vec[,i] <- austen_2016_btm.topBiterms[[i]][,1]
}


# WORK???
austen_2016_btm.Coherence <- text2vec::coherence(x = austen_2016_btm.topWords.vec,
                                                 tcm = austen_2016_tcm,
                                                 n_doc_tcm = dim(austen_2016_dtm)[1])







