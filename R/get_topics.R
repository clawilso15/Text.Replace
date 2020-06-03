#' Perform topic modeling on a corpus
#'
#' @param corp A corpus object
#' @param dfm_subset A logical expression indicating elements to keep: missing values are taken as false  
#' @param num_topics The number of topics to return
#' @param num_ngrams The phrase length to 
#' @param ...        Additional options passed to \code{Text.Replace::create_dfm()}
#' 
#' @examples 
#' \dontrun{
#' 
#'  # load AFMC We Need data set
#'    csv = "path/to/AFMC_We_Need/data/file.csv"
#'  
#'  # Extract text from the CSV file  
#'    DATA = Text.Replace:::extract_text(csv)
#'  
#'  # Subset DATA for question 1 of the Field Survey as object \code{q1f_DATA}
#'    q1f_DATA = subset(DATA, Source == 'Field Survey' & Question == 1)
#'
#'  # Create the corpus object
#'    corp = Text.Replace::create_corpus(q1f_DATA, "Comments")
#'    
#'  # Get the topics 
#'    Text.Replace::get_topics(corp = corp,
#'                             dfm_subset = Base == "Robins",
#'                             num_topics = 10,
#'                             num_ngrams = 1,
#'                             remove = quanteda::stopwords("english")) 
#' 
#' }
get_topics <- function(corp, 
                       dfm_subset,
                       num_topics,
                       num_ngrams,
                       ...) {

DFM  = Text.Replace::create_dfm(corp,
                                num_ngrams,
                                remove_punct = T,
                                ...)

e <- substitute(dfm_subset)
r <- eval(e, obj, parent.frame())

SUB = quanteda::dfm_subset(DFM[[1]], subset = r)

DTM = quanteda::convert(SUB, to = "topicmodels")

topics = topicmodels::LDA(DTM, k = num_topics)

return(topics)

}