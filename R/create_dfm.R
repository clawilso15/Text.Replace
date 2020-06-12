#' Creates a Document Feature Matrix
#'
#' @description Creates a \code{list} of \code{quanteda::dfm} based on specified ngrams
#'
#' @importFrom quanteda dfm
#' 
#' @param .corpus A \code{quanteda::corpus}-class object
#' @param .ngrams Number of \code{quanteda::dfm}-class objects to create
#' @param .remove_punct Boolean indicating if punctuation should be removed
#' @param ... Additional arguments passed on to \code{quanteda::dfm}
#' 
#' @return A \code{list} of \code{quanteda::dfm}-class objects
#'
#' @export
create_dfm <- function(.corpus, .ngrams = 5, .remove_punct = T,...) {
  
  if(!quanteda::is.corpus(.corpus))
    {
    
     stop('{.corpus} must be a quanteda::corpus class object')
    
    }
  
  out = vector(length = length(.ngrams), mode = "list")
  
  for(i in 1:length(.ngrams)){
    
    out[[i]] = quanteda::tokens(.corpus, remove_punct = .remove_punct) %>% 
               quanteda::tokens_ngrams(.ngrams[i]) %>% 
               quanteda::dfm(...)
    
  }
  
  names(out) = paste('ngrams',1:length(.ngrams), sep = '')
  
  return(out)
  
}
