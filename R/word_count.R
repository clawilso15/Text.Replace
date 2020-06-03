#' Return a count of the number of words in each document
#' 
#' @param .data A character vector or a \code{data.frame} containing a character vector
#' @param .column Character string naming a column from \code{.data}
#'                If \code{.column = NULL} \code{.data} is presumed to be a character vector
#'
#' @return A \code{data.frame} to which a column containing the number of words 
#'        for each document will be added to the column in \code{data}.  If 
#'        \code{.data} is a character vector the object returned will be a two-column
#'        \code{data.frame} in which the first column contains the text from each document
#'        and the second column will contain
#' @export
word_count <- function(.data, .column = NULL){
  
  .this_data = .data
  
  if(is.data.frame(.data)) {
    
     if(is.null(.column)) stop("\n\nIf {.data} is a data.frame\n{.column} must specify a column name or column number")
      
     .this_data = .data[[.column]] 
     
  }
  
  score <- function(f) unlist(lapply(.this_data, f))

funs <-
  c(
    function(s) sapply(gregexpr("\\W+", s), length) + 1,
    function(s) sapply(gregexpr("[[:alpha:]]+", s), function(x) sum(x > 0)),
    function(s) vapply(strsplit(s, "\\W+"), length, integer(1)),
    function(s) length(strsplit(gsub(' {2,}', ' ', s), ' ')[[1]]),
    function(s) length(str_match_all(s, "\\S+")[[1]]),
    function(s) str_count(s, "\\S+"),
    function(s) sapply(gregexpr("\\W+", s), function(x) sum(x > 0)) + 1,
    function(s) length(unlist(strsplit(s," "))),
    function(s) sapply(strsplit(s, " "), length),
    function(s) str_count(s, '\\w+')
  )

  words = lapply(funs, score)
  
  return(data.frame(.data,words = words[[length(words)]]))
    
}