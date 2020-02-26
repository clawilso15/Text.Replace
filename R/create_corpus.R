#' Build a corpus object
#' 
#' @description Creates a \code{quanteda::corpus}-class object from a collection of texts
#'
#' @importFrom ggplot2 sym
#' @importFrom quanteda corpus docvars
#' 
#' @return A \code{quanteda} corpus object 
#' @export
create_corpus <- function(df, text_col) {
  
  my_corpus <- quanteda::corpus(df[[text_col]])
  
  other_cols = colnames(df)[!(colnames(df) %in% text_col)]
  
  for(i in other_cols){
    
      quanteda::docvars(my_corpus, i) <- df[[i]]
    
  }
  
  return(my_corpus)
  
}