#' @title Sample Class Survey Data
#' @docType data
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

#' @title Sample Survey Data
#' @docType data
#' 
#' @description A \code{dataframe} contain survey results from 2016 and 2017 surveys
#' in Austen,Texas that is for simple testing and running of examples
#' 
#' @source \url{https://data.world/cityofaustin/mf9f-kvkk}
#' 
#' @format A \code{dataframe} with 3,187 rows with 3 variables:
#' \describe{
#' \item{Year}{The survey year for each response}
#' \item{Council.District}{The specific city district the comment is addressing}
#' \item{Comment}{The survey response in free form text}
#' }
"austensurvey"