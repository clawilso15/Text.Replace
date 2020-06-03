#' @importFrom Matrix colSums
#' @importFrom DT datatable
#' @export
text.replace <- function(regex_pattern, 
                         replacement, 
                         object, 
                         column_name, 
                         ngrams,
                         printem = F) {
    
    object[[column_name]] = gsub(regex_pattern, 
                                 replacement = replacement,
                                 object[[column_name]])
    
    my_corpus = Text.Replace::create_corpus(object, column_name)
    
    my_dfm = Text.Replace:::create_dfm(my_corpus, 
                                     num_ngrams = ngrams,
                                     remove_punct = T,
                                     tolower = TRUE, 
                                     stem = FALSE, 
                                     select = NULL, 
                                     remove = NULL, 
                                     dictionary = NULL, 
                                     thesaurus = NULL, 
                                     groups = NULL)
    
    freq <- Matrix::colSums(my_dfm[[1]])
    word <- names(freq)
    freq <- unname(freq)
    ord  <- order(freq, decreasing = T)
    word <- word[ord]
    freq <- freq[ord]
    
    df <- data.frame(term = word, frequency = freq) 
    
    if(printem) DT::datatable(df[1:100,])
    
    return(list(data = object,
                corpus = my_corpus, 
                dfm = my_dfm,
                df = df))
    
  }
