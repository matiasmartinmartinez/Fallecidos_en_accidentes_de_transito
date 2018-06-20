




library(shiny)

ui <- fluidPage(
  
  titlePanel( h1("Siniestros fatales de tránsito en Uruguay, 2013-2017")),
  
           tabsetPanel(
             tabPanel(
               title = h6("Introducción"),
               hr(),
               p("El presente estudio surge de la curiosidad por conocer el comportamiento de los siniestros de tránsitos en Uruguay y posee como objetivo madre una profunda aplicación de los conocimientos adquiridos en la asignatura Nuevas técnicas para el análisis de datos, marcando énfasis en un correcto análisis exploratorio y la generación de adecuadas visualizaciones de los datos.Se analizara tanto los datos del fallecido como lo del suceso.")),
             h3("Antecedentes:"),
             p(" En su programa de Sistema de Información Nacional de Tránsito (SINATRÁN), donde se extrajo los datos, la Unidad Nacional de Seguridad Vial (UNASEV), genera un informe anual sobre la Siniestralidad Vial en Uruguay para el periodo dado desde 2009 a la actualidad. Dicho informes se pueden encontrar a través del siguiente",  a(href="http://unasev.gub.uy/inicio/sinatran/informes_siniestralidad_vial_uruguay/","enlace.")),
            
             tabPanel(title = h6("Marco Teórico")              ),
             
             tabPanel(title = h6("Análisis de datos")                          ),
             
             tabPanel(title = h6("Conclusiones")                                 ),
             
             tabPanel(title = h6("Interpretaciones")             ),
             
             tabPanel(title = h6("Anexo")             )
           )
    )
  


server <-  function(input, output) {

  
}




shinyApp(ui = ui, server = server)

