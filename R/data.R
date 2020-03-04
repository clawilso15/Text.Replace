#' @title Product and service contracts
#'
#' @description
#' A dataset containing a single value score for the x attribute
#' (i.e. supply risk) and y attribute (i.e. profit impact) of 200
#' product and service contracts (PSC)
#'
#' @format A \code{tibble} with 200 rows and 3 variables
#'
#' \describe{
#'   \item{PSC}{Contract identifier for each product and service}
#'   \item{x_attribute}{x attribute score, from 1 (worst) to 5 (best) in increments of 0.01}
#'   \item{y_attribute}{y attribute score, from 1 (worst) to 10 (best) in increments of 0.01}
#' }
#'
'psc'
#' @title Sample Class Survey Data
#' 
#' @description 
#' A dataset containing a single column of comments with metadata
#' that partitions it.
#' 
#' @format A \code{tibble} with 1,110 rows and 4 variables
#' 
#' \describe{
#'   \item{Input_key}{Survey Number associated with question key and comment}
#'   \item{question_key}{Example key for multiple questions 1 to 6}
#'   \item{raw_evaluation}{A numeric evaluation input for positive (1), neutral (0), negative (-1)}
#'   \item{comments}{Free text associated input and question keys}
#' }
#' 
"surveyData"