library(tidytext)
library(stringi)
library(hunspell)

#' @title Spell Text Column
#' 
#' @description A function to spellcheck a column of text based on the _hunspell_ English dictionary.
#'
#' @param df A \code{dataframe} with a text column to analyze
#' @param text_col The column name with the text to spellcheck
#' @param to_lower A \code{logical} statement to specific whether to analyze the words as passed or to convert to all lower case before analysis. The default is \code{FALSE} to analyze data as given.
#' @param remove_all_caps A \code{logical} statement on whether to remove words that are all capital before analysis or not. The default is \code{TRUE} to remove all capital words assumine them to be a proper noun or acronym.
#'
#' @return A \code{character vector} of misspelled words.
#' @export
#'
#' @examples
#' \dontrun{
#' data(austensurvey)
#' flag_words <- spellcheck_words(austensurvey, "Comment")
#' }
spellcheck_words <- function(df, text_col, to_lower = FALSE, remove_all_caps = TRUE){
  
  tokens <- df %>% tidytext::unnest_tokens(output = word, 
                                    input = !! rlang::sym(text_col),
                                    token = "words",
                                    to_lower = to_lower)
  
  all.words <- unique(tokens$word)
  
  if(remove_all_caps){
    caps <- stringi::stri_extract_all_regex(all.words, 
                                            pattern = "^[A-Z]{2,}")
    all.words <- all.words[!all.words %in% caps]
  }
  
  flagged.words <- hunspell::hunspell(all.words)
  
  spellcheck_words <- unique(unlist(flagged.words))
}