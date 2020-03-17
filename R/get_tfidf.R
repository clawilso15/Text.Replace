#' Create a DFM with term frequency - inverse document frequency values
#'
#' @export
#' @param dfm A document feature matrix
#' @param docvar A character string denoting and document variable within \code{dfm}
#' @param ... Not currently used 
#' @importFrom quanteda dfm_subset dfm_group dfm_tfidf docvars
get_tfidf = function(dfm, docvar,...){
  
  dv_num = which(names(quanteda::docvars(dfm)) %in% docvar)
  
  sub_dfm = quanteda::dfm_subset(dfm, subset = dv_num)
  
  grp_dfm = quanteda::dfm_group(sub_dfm, groups = docvar)
  
  tfidf_dfm = quanteda::dfm_tfidf(grp_dfm)
  
  return(tfidf_dfm)
  
}
