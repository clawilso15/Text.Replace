#' Create a DFM with term frequency - inverse document frequency values
#'
#' @export
#' @param dfm A document feature matrix
#' @param docvar A character string denoting and document variable within \code{dfm}
#' @param ... Not currently used 
#' @importFrom quanteda dfm_subset dfm_group dfm_tfidf
get_tfidf = function(dfm, docvar,...){
  
  sub_dfm = dfm_subset(dfm, subset = !is.na(eval(parse(text = docvar))))
  
  grp_dfm = dfm_group(dfm_sub, groups = docvar)
  
  tfidf_dfm = dfm_tfidf(grp_dfm)
  
  return(tfidf_dfm)
  
}