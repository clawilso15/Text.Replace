#' Build a corpus object
#' 
#' @description Creates a \code{quanteda::corpus}-class object from a collection of texts
#'
#' @importFrom ggplot2 sym
#' @importFrom quanteda corpus docvars
#' 
#' @param .data   A data frame containing header values and single comments column
#' @param .column A \code{string} value identifying column with text to analyze
#' 
#' @return A \code{quanteda} corpus object 
#' 
#' @export
create_corpus <- function(.data, .column) {
  
  # Error check provided arguments
    if(!is.data.frame(.data)) stop('df must be a data frame')
  
    
  if(!(.column %in% colnames(.data))) {
    
    frame_headers = list(colnames(.data))
    
    #Need to fix column name output readability
    stop('This function requires that the text_col be a data frame header.\n',
         frame_headers)
  }
    
  #Create a quanteda corpus from identified text column
  my_corpus <- quanteda::corpus(.data[[.column]])
  
  #Create temprary data frame without text column
  other_cols = colnames(.data)[!(colnames(.data) %in% .column)]
  
  #Create quanteda document variables
  for(i in other_cols){
    
      quanteda::docvars(my_corpus, i) <- .data[[i]]
    
  }
  
  return(my_corpus)
  
}