library(stm)
library(quanteda)
### Subset corpus
AFMC_corpus_Q1_FS <- corpus_subset(AFMC_corpus, 
                                   subset = Question == 1 & Source == "Field Survey")

### Create dfm and trim
AFMC_dfm_Q1_FS_trim <- dfm(AFMC_corpus_Q1_FS, 
                           remove_punct = T, 
                           remove = stopwords('en')) %>% 
  dfm_trim(min_termfreq = 0.95, 
           termfreq_type = "quantile", 
           max_termfreq = 0.1, 
           docfreq_type = "prop")


AFMC_dfm_Q1_FS_trim <- dfm(AFMC_corpus_Q1_FS, 
                           remove_punct = T, 
                           remove = stopwords('en'))

set.seed(100)
### K = number of topics to generate using LDA fit
my_lda_fit20 <- stm(AFMC_dfm_Q1_FS_trim, 
                    K = 20, 
                    verbose = FALSE)
plot(my_lda_fit20)


my_lda_fit10 <- stm(AFMC_dfm_Q1_FS_trim, 
                    K = 10, 
                    verbose = FALSE)
plot(my_lda_fit10)


### Source: https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25
library(textmineR)

### Create Document Term Matrix
textmine_AFMC_dtm_Q1_FS <- CreateDtm(doc_vec = AFMC_corpus_Q1_FS, 
                                     doc_names = AFMC_corpus_Q1_FS$docname, 
                                     ngram_window = c(1,2))

# tf <- TermDocFreq(textmine_AFMC_dtm_Q1_FS)
# original_tf <- tf %>% select(term, term_freq, doc_freq)
# tf$term
# vocab <- tf$term[ tf$term_freq >1 & tf$doc_freq <nrow(textmine_AFMC_dtm_Q1_FS)/2]
# head(vocab)

ntopics <- 20
textmine_model <- FitLdaModel(dtm = textmine_AFMC_dtm_Q1_FS, 
                              k = ntopics, 
                              iterations = 500)
textmine_model$k <- ntopics
textmine_model$coherence <- CalcProbCoherence(phi = textmine_model$phi, 
                                              textmine_AFMC_dtm_Q1_FS, M = 5)



coherence_mat <- data.frame(k = 1:ntopics, 
                            coherence = textmine_model$coherence, 
                            stringsAsFactors = FALSE)

ggplot(coherence_mat, aes(x=k, y=coherence))+
  geom_point()+
  geom_line(group = 1) + 
  ggtitle("Best Topic by Coherence Score") + 
  theme_minimal() + 
  scale_x_continuous()+
  ylab("Coherence")


textmine_model$top_term <- GetTopTerms(phi = textmine_model$phi, M = 20)

top20_wide <- as.data.frame(textmine_model$top_term)
head(top20_wide)

textmine_model$topic_linguistic_dis <- CalcHellingerDist(textmine_model$phi)
textmine_model$hclust <- hclust(as.dist(textmine_model$topic_linguistic_dis),"ward.D")
plot(textmine_model$hclust)

### Quanteda Collocation Source: https://tutorials.quanteda.io/statistical-analysis/collocation/
phrases_n <- function(x, size = 2, min_count = 5){
  temp <- tokens_select(x, pattern = "^[A-Z]", valuetype = "regex", case_insensitive = FALSE, padding = TRUE) 
  phrases_n <- textstat_collocations(temp, min_count = min_count, tolower = F, size = size)
}
