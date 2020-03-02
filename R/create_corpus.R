#' Build a corpus object
#' 
#' @description Creates a \code{quanteda::corpus}-class object from a collection of texts
#'
#' @importFrom ggplot2 sym
#' @importFrom quanteda corpus docvars
#' 
#' @param df A data frame containing header values and single comments column
#' @param text_col A \code{string} value identifying column with text to analyze
#' 
#' @return A \code{quanteda} corpus object 
#' @export
create_corpus <- function(df, text_col) {
  
  #Error check provided arguments
  if(!is.data.frame(df)){
    stop('df must be a data frame')
  } 
    
  if(!(text_col %in% colnames(df))) {
    
    #Need to fix column name output readability
    stop('This function requires that the text_col be a data frame header.\n',
         colnames(df))
  }
    
  #Create a quanteda corpus from identified text column
  my_corpus <- quanteda::corpus(df[[text_col]])
  
  #Create temprary data frame without text column
  other_cols = colnames(df)[!(colnames(df) %in% text_col)]
  
  #Create quanteda document variables
  for(i in other_cols){
    
      quanteda::docvars(my_corpus, i) <- df[[i]]
    
  }
  
  return(my_corpus)
  
}