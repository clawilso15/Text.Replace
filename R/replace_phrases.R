replace_phrases <- function(text, old, new){
  
  stringr::str_replace_all(string = text, 
                           pattern = old,
                           replacement = new)

}