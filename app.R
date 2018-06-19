




library(shiny)

ui <- fluidPage(
  
  titlePanel( h1("Siniestros fatales de tránsito en Uruguay, 2013-2017")),
  
  
  fluidRow(
    column(width = 6),
    column(width = 10,
           tabsetPanel(
             tabPanel(title = h6("Introducción"),
                      fluidRow(                        
                      )
             ),
             tabPanel(title = h6("Marco Teórico"), 
                      tableOutput('table1')
             ),
             tabPanel(title = h6("Análisis de datos"),
                      tableOutput('table2')
             ),
             tabPanel(title = h6("Conclusiones"),
                      tableOutput('table2')
             ),
             tabPanel(title = h6("Interpretaciones"),
                      tableOutput('table2')
             ),
             tabPanel(title = h6("Anexo"),
                      tableOutput('table2')
             )
           )
    )
  )
)



server <-  function(input, output) {
  
  output$text_out <- renderText({ 
    paste("You have selected", input$text_input)
  })
  
}




shinyApp(ui = ui, server = server)

