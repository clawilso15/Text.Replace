#' Determines Collocated Terms
#' 
#' @description Searches a \code{quanteda::tokens} to locate ngrams.
#' The function cleans the token and returns a \code{dataframe} with results.
#' 
#' @importFrom quanteda tokens_select 
#' 
#' @param x A \code{quanteda::token}
#' @param ngram Ngram size to locate
#' @param min_count The minimum number of instances for each ngram to be included
#' 
#' @return A \code{dataframe} containing located phrases and number of appearances.
#' 
#' @export

phrases_collocations <- function(x, ngram = 2, min_count = 5) {
  
  temp <- quanteda::tokens_select(x, 
                        pattern = "^[A-Z]", 
                        valuetype = "regex", 
                        case_insensitive = FALSE, 
                        padding = TRUE) 
  
  phrases_collocations <- quanteda::textstat_collocations(temp, 
                                     min_count = min_count, 
                                     tolower = F, size = ngram)
}