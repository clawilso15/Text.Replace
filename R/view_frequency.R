#' View the frequency of each document variable in the data set
#'
#' @param dfm A document feature matrix created by \code{create_dfm()}
#' @param docvar A character string denoting the document variable to be displayed
#' @param ... Additional arguments sent to \code{metricsgraphics::mjs_plot()}  
#' @importFrom metricsgraphics mjs_plot mjs_bar mjs_labs mjs_add_css_rule
#' @importFrom magrittr `%>%`
#' @export
view_frequency <- function(dfm, docvar,...) {
  
  dt <- as.data.frame(table(dfm@docvars[docvar]))
  
  o <- order(dt$Freq, decreasing = T)
  
  mjs_plot(dt[o,], x = Freq, Var1,...) %>% 
    mjs_bar() %>%
        mjs_labs(x_label = "X = x", y_label = "Frequency")      %>% 
    mjs_add_css_rule("{{ID}} .mg-active-datapoint { font-size: 20pt }")
}