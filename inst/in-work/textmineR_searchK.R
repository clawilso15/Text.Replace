library(Text.Replace)
library(textmineR)


data("austensurvey")

austen_corpus <- Text.Replace::create_corpus(austensurvey, "Comment")

austen_2016_corpus <- quanteda::corpus_subset(austen_corpus, subset = Year == "2016")

austen_2016_dtm <- textmineR::CreateDtm(doc_vec = austen_2016_corpus,
                                        doc_names = austen_2016_corpus$docname,
                                        ngram_window = c(1,6))
  

# Determine number of topics to model

#k_list <- seq(10,85, by=15)

initial_k_lbound <-10
initial_k_ubound <-40
num_models_per_iteration <- 5

k_lbound <- initial_k_lbound
k_ubound <- initial_k_ubound

iteration_interval <- floor((k_ubound - k_lbound)/num_models_per_iteration)

iteration <- 0

complete = F

coherence_mat <- list()

max_k <- 0

while (complete == F && iteration < 5) {
  
  
  iteration <- iteration + 1
  
  k_list <- seq(k_lbound, k_ubound, 
                by = iteration_interval)
  
  print(paste0("Now evaluating iteration ",iteration,
               " with bounds [", k_lbound, ",", k_ubound, "]"))
  
  model_list <- TmParallelApply(X = k_list, FUN = function(k){
    
    set.seed(100)
    
    m <- FitLdaModel(dtm = austen_2016_dtm, 
                     k = k, 
                     iterations = 200, 
                     burnin = 180,
                     alpha = 0.1,
                     beta = colSums(austen_2016_dtm) / sum(austen_2016_dtm) * 100,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = FALSE,
                     cpus = 2)
    m$k <- k
    
    m
  }, export= ls(), # c("nih_sample_dtm"), # export only needed for Windows machines
  cpus = 2) 
  
  print(paste0("Iteration ", iteration, " Complete with Coherence values:"))
  
  coherence_mat[[iteration]] <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                              coherence = sapply(model_list, function(x) mean(x$coherence)), 
                              stringsAsFactors = FALSE)
  
  print(coherence_mat[[iteration]])
  
  # Find Max coherence for fitted models
  max_k <- coherence_mat[[iteration]][which(coherence_mat[[iteration]][,2] == max(coherence_mat[[iteration]][,2])),1]
  
  mat_index <- which(coherence_mat[[iteration]][,1] == max_k)
  
  max_k_val = coherence_mat[[iteration]][mat_index,2]
  
  print(paste0("Model k = ",max_k ," had the best topic coherence with a value of ", max_k_val))
  
  # Find k values immediately to left and right to subset on
  # Assumes the max value of k is not at the extreme
  
    if(mat_index == 0 ){
      if(max_k - iteration_interval < 0){
        k_lbound = 2
      } else {
        k_lbound = max_k - iteration_interval
      }
      k_ubound = coherence_mat[[iteration]][mat_index + 1 ,1]
    } else if(mat_index == nrow(coherence_mat[[iteration]])) {
      k_lbound = coherence_mat[[iteration]][mat_index - 1 ,1]
      k_ubound = max_k + iteration_interval
    } else {
      k_lbound = coherence_mat[[iteration]][mat_index - 1 ,1]
      k_ubound = coherence_mat[[iteration]][mat_index + 1 ,1]
    }

  if(((k_ubound - k_lbound)/num_models_per_iteration) < 1){
    if(iteration_interval == 1){
      complete = T
    }else{
    iteration_interval = 1 
    }
  } else {
    iteration_interval = floor((k_ubound - k_lbound)/num_models_per_iteration)
  }
}
