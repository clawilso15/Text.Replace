server <- function(input, output) {
  
  output$map <- renderPlot({
    
    map_data$value = map_data[, input$select]
    
    state_choropleth(map_data,
                     title = input$select, 
                     num_colors = input$num_colors)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(map_data)
   # map_data[order(map_data[input$select]), ]
  })
}