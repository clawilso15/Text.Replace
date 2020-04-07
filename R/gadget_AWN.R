#' Text analysis app for the AFMC We Need Data
#'
#' @description Shiny gadget used to visually inspect columns in a data set and select columns to remove
#'
#' @param data A data set
#' @param rownames \code{logical} Should rownames be included?
#' @param theme \code{character} A bootswatch theme provided to \code{shinythemes::shinytheme}
#' @param width \code{character} Width of the gadget (in valid css units)
#' @param height \code{character} Height of the gadget (in valid css units)
#' @param css \code{character} Path to a custom css file
#' 
#' @importFrom shinythemes shinytheme
#' @importFrom shiny runGadget browserViewer
#' @importFrom shiny fluidPage tags includeCSS sidebarLayout sidebarPanel
#' @importFrom shiny uiOutput selectizeInput actionButton reactive h4
#' @importFrom shiny stopApp observeEvent mainPanel fluidRow
#' @importFrom data.table as.data.table ':='
#' @importFrom DT renderDataTable dataTableOutput datatable
#' 
#' @return A \code{list} of length 2
#'   \item{data}{A \code{data.frame} containing the columns that were not removed}
#'   \item{script}{A line of code that can be used to replicate cleaning performed in the gadget}
#' 
#' @examples \dontrun{clean_columns(mtcars)}
#' 
#' @family shinygadgets
#' @return A printed shiny app
#' @export
gadget_AWN <- 
function(obj, 
         rownames = TRUE, 
         theme = "flatly",
         width = '100%', 
         height = '600px', 
         css = NULL) {

ui = fluidPage(theme = shinythemes::shinytheme(theme = theme),
               if(is.null(css)) Text.Replace::add_css(),
               
sidebarLayout(
  sidebarPanel(width = 3,
    uiOutput("ngrams_"),
    uiOutput("source_"),
    uiOutput("question_"),
    uiOutput("bases_"),
    uiOutput("grade_"),
    uiOutput("twoltr_"),
    uiOutput("center_"),
    uiOutput("milciv_"),
    uiOutput("afsc_"),
    actionButton('done',h4('Finish'), width = '100%')), 
  
    mainPanel(width = 9,
              tabsetPanel(type = "pills",
              tabPanel(h4("Data Table"), DT::dataTableOutput("obj_data", height = "600px")),
              tabPanel(h4("Wordcloud"), shiny::plotOutput("wordcloud", height = "600px"))))))

server = function(input, output) {

output$ngrams_ <- renderUI({
  
  selectizeInput('ngrams',
                 h4('Select Phrase Length'),
                 choices = 1:length(obj),
                 selected = 1, 
                 multiple = !TRUE)
  
})
  
output$bases_ <- renderUI({
  
  selectizeInput('bases', 
                 h4('Select Bases'),
                 choices = unique(obj[[1]]@docvars$Base),
                 selected = NULL, 
                 multiple = TRUE)
  
})

output$center_ <- renderUI({
  
  selectizeInput('centers', 
                 h4('Select Centers'),
                 choices = unique(obj[[1]]@docvars$Center),
                 selected = NULL, 
                 multiple = TRUE)
  
})

output$grade_ <- renderUI({
  
  selectizeInput('grades', 
                 h4('Select Grade'),
                 choices = unique(obj[[1]]@docvars$Grade),
                 selected = NULL, 
                 multiple = TRUE)
  
})

output$source_ <- renderUI({
  
  selectizeInput('sources', 
                 h4('Select Source'),
                 choices = unique(obj[[1]]@docvars$Source),
                 selected = NULL, 
                 multiple = !TRUE)
  
})

output$question_ <- renderUI({
  
  selectizeInput('questions', 
                 h4('Select Question'),
                 choices = unique(obj[[1]]@docvars$Question),
                 selected = NULL, 
                 multiple = TRUE)
  
})

output$twoltr_ <- renderUI({
  
  selectizeInput('twoltrs', 
                 h4('Select 2 Ltr'),
                 choices = unique(obj[[1]]@docvars$`2 Ltr`),
                 selected = NULL, 
                 multiple = TRUE)
  
})

output$afsc_ <- renderUI({
  
  selectizeInput('afscs', 
                 h4('Select AFSC'),
                 choices = unique(obj[[1]]@docvars$`Series AFSC`),
                 selected = NULL, 
                 multiple = TRUE)
  
})

output$milciv_ <- renderUI({
  
  selectizeInput('milciv', 
                 h4('Select Mil/Civ'),
                 choices = unique(obj[[1]]@docvars$`Mil Civ`),
                 selected = NULL, 
                 multiple = TRUE)
  
})
out_obj = reactive({ obj[[as.numeric(input$ngrams)]] })

dfm_out = reactive({ 
  
  base_obj = reactive({
    
             `if`(!is.null(input$bases),
                  quanteda::dfm_subset(out_obj(), subset = Base == as.character(input$bases)),
                  out_obj())
  })
      
    
  question_obj = reactive({ 
    
             `if`(!is.null(input$questions),
                  quanteda::dfm_subset(base_obj(), subset = Question == as.character(input$questions)),
                  base_obj())
    
  })
    
  afsc_obj = reactive({ 
    
             `if`(!is.null(input$afscs),
                  quanteda::dfm_subset(question_obj(), subset = `Series AFSC` == as.character(input$afscs)),
                  question_obj())
    
  })
  center_obj = reactive({ 
    
             `if`(!is.null(input$centers),
                  quanteda::dfm_subset(afsc_obj(), subset = Center == as.character(input$centers)),
                  afsc_obj())

  })
  source_obj = reactive({ 
    
             `if`(!is.null(input$sources),
                  quanteda::dfm_subset(center_obj(), subset = Source == as.character(input$sources)),
                  center_obj())

  })
  twoltr_obj = reactive({ 
    
             `if`(!is.null(input$twoltrs),
                  quanteda::dfm_subset(source_obj(), subset = `2 Ltr` == as.character(input$twoltrs)),
                  source_obj())

  })
  milciv_obj = reactive({ 
    
              `if`(!is.null(input$milciv),
                   quanteda::dfm_subset(twoltr_obj(), subset = `Mil Civ` == as.character(input$milciv)),
                   twoltr_obj())

  })
  grade_obj = reactive({ 
    
              `if`(!is.null(input$grades),
                   quanteda::dfm_subset(milciv_obj(), subset = Grade == as.character(input$grades)),
                   milciv_obj())
    
  })
  
  grade_obj()

})


freq = reactive({ Matrix::colSums(dfm_out()) })
word = reactive({ names(freq()) })
ord = reactive({ order(freq(), decreasing = T) })
df = reactive({ data.frame(term = word()[ord()],
                           frequency = unname(freq())[ord()]) })

dfm_trim = reactive({ quanteda::dfm_trim(dfm_out(), 
                                         min_termfreq = 5,
                                         max_termfreq = 100) })

  output$obj_data <- DT::renderDataTable({

      DT::datatable(df(),
                    fillContainer = T,
                    extensions = 'Responsive')

})
  
output$wordcloud <- renderPlot({

      quanteda::textplot_wordcloud(dfm_trim(), max_words = 100)

})
# 
#   observeEvent(input$done, {
# 
#     stopApp(list(data = as.data.frame( df() )))
# 
# })
}
              runGadget(app = ui,
                        server = server,
                        viewer = browserViewer(browser = getOption("browser")))
}

