




library(shiny)

ui <- fluidPage(
  
  titlePanel( h1("Siniestros fatales de tránsito en Uruguay, 2013-2017"),
  
           tabsetPanel(
             tabPanel(
               title = h6("Introducción"),
               hr(),
               p("El presente estudio surge de la curiosidad por conocer el comportamiento de los siniestros de tránsitos en Uruguay y posee como objetivo madre una profunda aplicación de los conocimientos adquiridos en la asignatura Nuevas técnicas para el análisis de datos, marcando énfasis en un correcto análisis exploratorio y la generación de adecuadas visualizaciones de los datos.Se analizara tanto los datos del fallecido como lo del suceso.")),
             h3("Antecedentes:"),
             p(" En su programa de Sistema de Información Nacional de Tránsito (SINATRÁN), donde se extrajo los datos, la Unidad Nacional de Seguridad Vial (UNASEV), genera un informe anual sobre la Siniestralidad Vial en Uruguay para el periodo dado desde 2009 a la actualidad. Dicho informes se pueden encontrar a través del siguiente",  a(href="http://unasev.gub.uy/inicio/sinatran/informes_siniestralidad_vial_uruguay/","enlace.")),
             h3("Justificación:"),
             p("Conocer la realidad uruguaya a nivel de tránsito permite no sólo generar conocimiento y concientizar acerca de la importancia en los cuidados y medidas de seguridad apropiadas en el tránsito, sino también tomar decisiones capaces de transformar un posible siniestro fatal en un accidente menor, o en mejor instancia en una posible inexistencia del mismo.
Adicionalmente, motivar la divulgación de distintos estudios estadísticos que a día de hoy provienen únicamente de entidades gubernamentales, los mismos no solo de ámbitos de seguridad vial sino sociales en general."),
             h3("Preguntas a responder"),
             p("1 - ¿Cuál es la distribución relativa por sexo y qué comportamiento posee la edad del fallecido en el período dado?"),
             p("2 - ¿Existe relación entre el rol del fallecido tanto con la edad, como con el vehículo del difunto?"),
             p("3 -  Para observaciones de vehículo igual a peatón: ¿qué vehículo estubo involucrado en el fallecimientos de los mismos y cuál posee mayor frecuencia acumulada?"),
             p("4 - ¿Hay relación entre el vehículo del fallecido y otro posible vehículo involucrado en el accidente?"),
             p("5 - ¿Qué comportamiento posee la jurisdicción por si misma, y según el tipo de siniestro ocurrido?"),
             p("6 - ¿Cuál es la distribución de los días de supervivencia luego del accidente y qué proporción ocupa cada tipo de siniestro en caso de muerte súbita?"),
             p("7 - ¿Cuál es la tasa de fallecidos por departamento?"),
             p("8 - ¿Qué fechas y horarios poseen mayor frecuencia absoluta de difuntos?")
             
             ),
             
             
             
             tabPanel(
               title = h6("Marco Teórico"),
               h3("Fuente de datos"),
               
               
                      ),
             
             tabPanel(title = h6("Análisis de datos")                          ),
             
             tabPanel(title = h6("Conclusiones")                                 ),
             
             tabPanel(title = h6("Interpretaciones")             ),
             
             tabPanel(title = h6("Anexo")             )
           )
    )
  


server <-  function(input, output) {

  
}




shinyApp(ui = ui, server = server)

