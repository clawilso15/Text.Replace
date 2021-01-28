#' Randomly sample survey responses
#'
#' @param df A dataframe object with
#' @param text_col The column with each response per row
#' @param n The number of responses to sample
#'
#' @description This 
#'
#' @return A vector of sampled comments from the supplied dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#' data(austensurvey)
#' austen_2016 <- subset(austensurvey, subset = Year == 2016)
#' set.seed(100)
#' rand_2016 <- response_sampler(austen_2016, "Comment", 10)
#' }
#' 
response_sampler <- function(df, text_col, n){
  
  rand_responses <- sample(nrow(df), n)
  
  response_sampler <- df[rand_responses, text_col]
}