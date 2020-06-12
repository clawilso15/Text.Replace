parse_text <- function(.data,...){
  
    spacyr::spacy_initialize()
  
    return(spacyr::spacy_parse(.data,...))
  
}