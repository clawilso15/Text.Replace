#' Creates a Document Feature Matrix
#'
#' @importFrom quanteda dfm
#'
#' @export
create_dfm <- function(obj, ...) {
  
   return(quanteda::dfm(x = obj, ...))
  
}
