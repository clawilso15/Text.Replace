library(shiny)
library(miniUI)
library(quanteda)
library(hunspell)
library(DT)

# Commented out code not currently functioning

# Features
# 1.0 add values to output table based on button click, 
# radiobutton selected, and recommendation selected or user defined string


#' @title Shiny Spell Check Gadget
#'
#' @description A \code{shiny gadget} that pairs with the
#'   \code{\link{spellcheck_words}} to examine flagged words and create a
#'   dataframe for string replacements.
#'   
#' @importFrom miniUI miniPage
#' @importFrom hunspell dictionary
#'
#' @param words A \code{character} vector of strings to spell check
#' @param corpus A \code{quanteda corpus} to examine each word from words in 
#'   its textual context.
#' @param my_dict A \code{hunspell} dictionary object to use as reference in
#'   suggesting spelling corrections. The default is the 'en-US' dictionary.
#'
#' @return A dataframe
#' @export
#'
#' @examples
spell_check_gadget <- function(words, corpus, 
                               my_dict = hunspell::dictionary("en_US")){
  
  spell.ui <- miniPage(
    theme = "flatly",
    gadgetTitleBar("Spell Checker"),
    miniTabstripPanel(
      miniTabPanel("Words", icon = icon("file-word"),
                   miniContentPanel(
                     
                     fluidRow(
                       # View input word vector column
                       column(3,
                              h3("Identified Incorrect Strings"),
                              
                              # Drop down to select incorrect string to review in the later interfaces        
                              selectInput(inputId = "wordSelect",
                                          label = "Select Word to review",
                                          choices = words,
                                          selected = words[1])
                       ),
                       
                       # View recommended corrections column
                       column(3, offset = 1,
                              h3("Recommended Corrections"),
                              h5("Below words and phrases are suggested by hunspell 
                               dictionary or user provided dictionary."),
                              
                              # Full list of recommended suggestions -- May remove for final release
                              verbatimTextOutput("suggest"),
                              
                              # Drop down to select appropriate correction
                              selectInput(inputId = "suggestions", 
                                          label = "Select replacement word", 
                                          choices = c("NONE"), 
                                          selected = "NONE")
                       ),
                       
                       # Add to ouput data frame column
                       column(3, offset = 1,
                              
                              h3("Select Replacement Text"),
                              
                              # Radio buttons to selected betwen recommended correction or user defined replacement      
                              radioButtons(inputId = "radio",
                                           label = "Select which new text to use:",
                                           choices = list("Use Drop Down string" = 1,
                                                          "Use User Text string" = 2),
                                           selected = 1),
                              
                              # Input string to allow user defined string as replacement text
                              textInput(inputId = "new_text",
                                        label = "Input replacement text in lieu of suggested drop down:",
                                        value = "Enter text..."),
                              
                              # Event button to add old and replacement terms to data frame for exporting
                              actionButton(inputId = "df_add",
                                           label = "Data Frame Add",
                                           ))
                     ),
                     
                     # View selected term in context
                     
                     hr(),
                     
                     h3("Selected Word in Context"),
                     h5("Use the table below to verify term usage and thus the correct replacement text."),
                     
                     DT::dataTableOutput("kwic_table")
                   )
      ),
      
      # New page to view output table
      miniTabPanel("Replacements", icon = icon("exchange-alt"),
                   miniContentPanel(
                     
                     ###################### Incremental Functionality Testing
                     fluidRow(column(4, verbatimTextOutput("value"))),
                     
                     hr(),
                     
                     #fluidRow(column(3, verbatimTextOutput("radio-select"))),
                     
                     hr(),
                     
                     #fluidRow(column(3,verbatimTextOutput("user-text"))),
                     
                     hr(),
                     
                     #################################
                     
                     # Data table to view all added terms for gadget output
                     h3("Replacement Table"),
                     h5("Table below displays terms and their selected replacements from the Words tab."),
                     
                     DT::dataTableOutput("replace_table")
                   )
      )
      
    )
  )
  
  spell.server <- function(input, output, session){
    
    # Word correction suggestions
    output$suggest <- renderPrint({hunspell_suggest(input$wordSelect, dict = my_dict)})
    
    # Update recommendation dropdown with newly selected word to review
    observe({
      
      sugg <- unlist(hunspell::hunspell_suggest(input$wordSelect, dict = my_dict))
      
      updateSelectInput(session, "suggestions", choices = sugg)
      
    })
    
    # Update output table and display
    observe({
      
      # word.replace = "test-text"
      # 
      # if(input$radio == 1){
      #   word.replace <- input$suggestions
      # }
      # 
      # if(input$radio == 2){
      #   word.replace <- input$new_text
      # }
      # 
      # out_df <- data.frame("Old Word" = input$wordSelect, "New Word" = word.replace)
      output$replace_table <- DT::renderDataTable({out_df()})
    })
    
    
    
    
    # Create a reactive table
    # out_df <- reactiveTable({
    #   
    #   
    #   
    #   
    # })
    # 
    # 
    
    
    out_df <- reactive({
      word.replace = "test-text"
      
      if(input$radio == 1){
        word.replace <- input$suggestions
      }
      
      if(input$radio == 2){
        word.replace <- input$new_text
      }
      
      out_df <- data.frame("Old Word" = input$wordSelect, "New Word" = word.replace)
    })
    # 
    # output$replace_table <- DT::renderDataTable({out_df()})
    
    ################### Incremental Functionality Testing
    # Test button action
    output$value <- renderPrint({ input$df_add })
    
    # Test radio select output
    #output$radio-select <- renderPrint({ input$radio })
    
    # Test user string output
    #output$user-text <- renderPrint({input$new-text})
    #####################
    
    # Update kwic output table and display
    output$kwic_table <- DT::renderDataTable({
      quanteda::kwic(corpus, input$wordSelect)[4:6]
    })
    
    # End gadget when 'Done' button is pressed and output data frame of old and new text columns
    observeEvent(input$done, {
      gadget.out <- out_df()
      stopApp(gadget.out)
    })
    
  }
  
  runGadget(spell.ui, spell.server, viewer = browserViewer())
}