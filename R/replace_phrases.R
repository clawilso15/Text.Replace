#' Replace text values
#'
#' @param text 
#' @param old 
#' @param new 
#' 
#' @importFrom stringr str_replace_all
#'
#' @return
#'
replace_phrases <- function(text, old, new){
  
  stringr::str_replace_all(string = text, 
                           pattern = old,
                           replacement = new)

}