#' Creates a Document Feature Matrix
#'
#' @importFrom quanteda dfm
#'
#' @export
create_dfm <- function(obj, num_ngrams = 5,...) {
  
  out = list()
  
  for(i in 1: num_ngrams){
    
    out[[i]] = quanteda::dfm(x = obj, ngrams = i, ...)
    
  }
  
  names(out) = paste('ngrams',1:num_ngrams, sep = '')
  
   return(out)
  
}
