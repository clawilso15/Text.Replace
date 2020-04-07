#' Creates a Document Feature Matrix
#'
#' @description Creates a \code{list} of \code{quanteda::dfm} based on specified ngrams
#'
#' @importFrom quanteda dfm
#' 
#' @param obj A \code{quanteda::corpus}-class object
#' @param num_ngrams Number of \code{quanteda::dfm}-class objects to create
#' 
#' @return A \code{list} of \code{quanteda::dfm} -class objects
#'
#' @export
create_dfm <- function(obj, num_ngrams = 5,remove_punct = T,...) {
  
  if(!quanteda::is.corpus(obj)){
    
     stop('obj must be a quanteda::corpus class obj')
    
  }
  
  out = vector(length = length(num_ngrams), mode = "list")
  
  for(i in 1:length(num_ngrams)){
    
    out[[i]] = quanteda::tokens(obj, remove_punct = remove_punct) %>% 
               quanteda::tokens_ngrams(num_ngrams[i]) %>% 
               quanteda::dfm(...)
    
  }
  
  names(out) = paste('ngrams',1:length(num_ngrams), sep = '')
  
  return(out)
  
}
