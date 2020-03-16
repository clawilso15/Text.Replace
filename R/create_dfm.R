#' Creates a Document Feature Matrix
#'
#' @description Creates a \code{list} of \code{quanteda::dfm} based on 
#'
#' @importFrom quanteda dfm
#' 
#' @param obj A \code{quanteda::corpus}-class object
#' @param num_ngrams Number of \code{quanteda::dfm}-class objects to create
#' 
#' @return A \code{list} of \code{quanteda::dfm} -class objects
#'
#' @export
create_dfm <- function(obj, num_ngrams = 5,...) {
  
  if(!quanteda::is.corpus(obj)){
    stop('obj must be a quanteda::corpus class obj')
  }
  
  out = list()
  
  for(i in 1: num_ngrams){
    
    out[[i]] = quanteda::dfm(x = obj, ngrams = i, ...)
  }
  
  names(out) = paste('ngrams',1:num_ngrams, sep = '')
  
  return(out)
}
