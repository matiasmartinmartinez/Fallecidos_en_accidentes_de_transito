###################################################################################################################################################################################


#Datos a utilizar
load("base_datos___fallecidos_transito_uruguay_2013-2017.RData")


#Cargamos paquetes en caso de ser necesario

#paquete#s.a.utilizar<- c( "tidyverse", "rmarkdown", "shiny","shinythemes", "ggmosaic", "plotly", "ggmap", "raster", "rgdal", "knitr", "scales", "lubridate", "devtools","grid", "gridExtra","sp")
#ipack(paquetes.a.utilizar)
library(plotly)
###################################################################################################################################################################################








#UI

ui <- fluidPage(theme = shinythemes::shinytheme("paper"),
                
                
                titlePanel( h1("Siniestros fatales de tránsito en Uruguay, 2013-2017")),
                
                tabsetPanel(
                  
                  
                  #DISTRIBUCIÓN TERRITORIAL
                  tabPanel(
                    title = h6("Distribución territorial"),
                    shiny::hr(),
                    
                    sidebarLayout(
                      
                      sidebarPanel(
                        
                        checkboxGroupInput("años", "Elegir Año:",
                                           choices = c("2013", "2014", "2015", "2016","2017"))
                      ),
                      
                      
                      mainPanel(
                        plotOutput("mapa"),
                        shiny::hr(),
                        p("Dichas tasas corresponden a los fallecidos en siniestros de tránsito cada 10.000 habitantes en cada departamento del Uruguay")
                        
                      )
                    )
                  ),  
                  
                  
                  
                  #DENSIDAD UI
                  
                  tabPanel(
                    title = h6("Densidad según vehículo"),
                    shiny::hr(),
                    
                    sidebarLayout(
                      
                      sidebarPanel(
                        
                        selectInput("sexodensidad", "Sexo:",
                                    choices = c("Ambos", "Femenino", "Masculino")),
                        
                        selectInput("añosdensidad", "Elegir año:",
                                    choices = c("Todos","2017", "2016", "2015", "2014","2013")),
                        
                        checkboxGroupInput("vehiculodensidad", "Vehículo:",
                                           choiceNames = c("Auto","Bicicleta","Camioneta" ,"Moto", "Peatón"),
                                           choiceValues =  c("AUTO", "BICICLETA", "CAMIONETA", "MOTO","PEATON"))
                        
                      ),
                      mainPanel(   
                        plotlyOutput("densidad")
                      )
                    )
                  ),
                  #MOSAICOs UI
                  
                  tabPanel(
                    title = h6("Mosaicos"),
                    shiny::hr(),
                    
                    sidebarLayout(
                      
                      sidebarPanel(
                        
                        selectInput("variablemosaico", "Variable:",
                                    choices = c("Sexo","Rol", "Jurisdicción")),
                        
                        checkboxGroupInput("añosmosaico", "Años a considerar:",
                                           choices = c("2013", "2014", "2015", "2016","2017"))
                        
                        
                      ),
                      mainPanel( 
                        p("A continuación puede visualizar la proporción de fallecidos según vehículo del mismo."),
                        plotlyOutput("rol"),
                        shiny::hr(),
                        shiny::hr(),
                        shiny::hr(),
                        p("En tanto, el siguiente gráfico muestra la proporción de fallecidos según la existencia o no de otro posible involucrado."),
                        plotlyOutput("involucrado"),
                        shiny::br()
                        
                      )
                    )
                  ),
                  
                  
                  #SERIES DE TIEMPO UI
                  tabPanel(
                    title = h6("Series de tiempo"),
                    sidebarLayout(
                      
                      
                      sidebarPanel(
                        
                        selectInput("fallecidos", "Fallecidos:",
                                    choices = c("Total", "Sexo", "Rol","Jurisdicción")),
                        selectInput("intervalo", "Intervalo:",
                                    choices = c("1 mes","3 meses", "6 meses")),
                        checkboxInput("zoom","Zoom"),
                        checkboxInput("error", "Error", TRUE),
                        checkboxInput("puntoylinea", "Punto y línea", TRUE)
                      ),
                      mainPanel(
                        plotOutput("fecha")
                        
                      )
                    )
                  ),
                  
                  #                  #FECHA Y HORA
                  tabPanel(
                    title = h6("Fecha y hora"),
                    sidebarLayout(
                      
                      
                      sidebarPanel(
                        
                        selectInput("sexofechayhora", "Sexo:",
                                    choices = c("Ambos", "Femenino", "Masculino")),
                        sliderInput("sdias", "Días de supervivencia:",
                                    min = 1, max = 30, value = 30 ),
                        
                        checkboxGroupInput("añosfechayhora", "Elegir año:",
                                           choices = c("2017", "2016", "2015", "2014","2013"))
                   
                      
                        
                      ),
                      mainPanel(
                        shiny::hr(),
                        plotOutput("tilefechayhora"),
                        shiny::hr(),
                        p("Indica la frecuencia absoluta de fallecidos según día de la semana y hora del accidente.")
                      
                        
                      )
                    )
                  )
                  
                  
                  
                ))









################################################################################################


# SERVER

server <- function(input, output) {
  
  
  
  
  
  
  
  #DISTRIBUCIÓN TERRITORIAL
  
  añosmapaInput <- reactive({input$años})
  
  output$mapa <- renderPlot({
    
    
    if(is.null(input$años)) 
      #si no marcas ningún año
      
    {
      mapeo(   cbind((datos %>% count(dep) %>% arrange(dep)),censo)%>% 
                 mutate(n=(n/censo)*10000)    ) +
        labs(fill = "Tasa",
             x = NULL,
             y = NULL) +
        scale_fill_gradient2(
          low = "#d8b365",
          mid = "white",
          high = "#5ab4ac",
          midpoint = mean(pob.dep$tasa))
    }
    
    else
      #si elegis al menos un año
    {
      
      n= (datos %>% filter( a %in% añosmapaInput() )%>% count(a,dep) %>% arrange(a) %>%
            mutate(censo=(rep(censo,(length(table(a)))))) %>% mutate(n= ((n/censo)*10000)))
      
      mapeo(n) +
        labs(
          fill = "Tasa",
          x = NULL,
          y = NULL) +
        scale_fill_gradient2(
          low = "#d8b365",
          mid = "white",
          high = "#5ab4ac",
          midpoint =  mean(n$n)) +
        facet_wrap( ~ a)
    }
    
  })
  #cierre render
  
  
  
  
  
  
  
  
  #DENSIDAD
  
  
  #Reactivos:
  sexodensidadInput <- reactive({ 
    switch(input$sexodensidad,
           "Femenino"= "F",
           "Masculino" = "M")   })
  añosdensidadInput <- reactive({   input$añosdensidad  })
  
  
  
  output$densidad <- renderPlotly({
    
    
    if(is.null( input$vehiculodensidad ))
      #si no marca vehículo
    {
      if(input$añosdensidad=="Todos")
      {
        if (input$sexodensidad=="Ambos")
          
        {  
          ggplotly(
            datos  %>% 
              ggplot(aes(edad)) +
              geom_density(
                fill = "grey23",
                alpha = 0.5, 
                show.legend = T) +
              labs(
                x =  "Edad", 
                y = "Densidad")  + 
              theme_minimal() + 
              guides(fill = guide_legend(title = "Vehículo:")) +
              theme(
                legend.position = "bottom", 
                axis.title  = (element_text(
                  size = 12,
                  colour = "darkslategray")))
          )
        }
        else
          #no marcó ambos, decide elegir Masculino o Femenino
        {
          ggplotly(
            datos %>% filter(sexo%in%sexodensidadInput())%>% 
              ggplot(aes(edad)) +
              geom_density(fill = "grey23",
                           alpha = 0.5, 
                           show.legend = T) +
              labs(
                x =  "Edad", 
                y = "Densidad")  + 
              theme_minimal() + 
              guides(fill = guide_legend(title = "Vehículo:")) +
              theme(
                legend.position = "bottom", 
                axis.title  = (element_text(
                  size = 12,
                  colour = "darkslategray")))
          )  
        }
        #if ambos,M,F
      }
      #if añosdensidad=="Todos"
      
      
      else
        #elige un año
      {
        if (input$sexodensidad=="Ambos")
        {
          ggplotly(
            datos %>% filter(a %in% añosdensidadInput() )%>%
              ggplot(aes(edad)) +
              geom_density(
                fill = "grey23",
                alpha = 0.5, 
                show.legend = T) +
              labs(
                x =  "Edad", 
                y = "Densidad")  + 
              theme_minimal() + 
              guides(fill = guide_legend(title = "Vehículo:")) +
              theme(
                legend.position = "bottom", 
                axis.title  = (element_text(
                  size = 12,
                  colour = "darkslategray")))
          )
        }
        else
          #además de elegir un año, elige sexo F o M
        {
          ggplotly(
            datos %>% filter(sexo %in% sexodensidadInput()) %>% 
              filter(a %in% añosdensidadInput())%>% 
              ggplot(aes(edad)) +
              geom_density( 
                fill = "grey23", 
                alpha = 0.5, 
                show.legend = T) +
              labs(
                x =  "Edad", 
                y = "Densidad")  +
              theme_minimal() + 
              guides(fill = guide_legend(title = "Vehículo:")) +
              theme(
                legend.position = "bottom", 
                axis.title  = (element_text(
                  size = 12,
                  colour = "darkslategray")))
          )
        }
      }
      #if añosdensidad=="Todos"
    }
    
    
    else  
      #ELIGE VEHÍCULO
      
    {
      if(input$añosdensidad=="Todos")
      {
        if (input$sexodensidad=="Ambos")      
        {  
          ggplotly(
            datos  %>% filter( vehi == input$vehiculodensidad  )%>% 
              ggplot(aes(edad)) +
              geom_density(
                aes(fill = vehi), 
                alpha = 0.5, 
                show.legend = T) +
              labs(
                x =  "Edad", 
                y = "Densidad")  + 
              theme_minimal() + 
              guides(fill = guide_legend(title = "Vehículo:")) +
              theme(
                legend.position = "bottom", 
                axis.title  = (element_text(
                  size = 12,
                  colour = "darkslategray"))) + 
              scale_fill_manual (values=colores))
        }
        else
        {
          ggplotly(
            datos %>% filter(  vehi == input$vehiculodensidad) %>% 
              filter(vehi%in%input$vehiculodensidad )%>% 
              ggplot(aes(edad)) +
              geom_density(
                aes(fill = vehi), 
                alpha = 0.5, 
                show.legend = T) +
              labs(
                x =  "Edad", 
                y = "Densidad")  + 
              theme_minimal() + 
              guides(fill = guide_legend(title = "Vehículo:")) +
              theme(
                legend.position = "bottom", 
                axis.title  = (element_text(
                  size = 12,
                  colour = "darkslategray"))) + 
              scale_fill_manual (values=colores))  
        }
        #if ambos,M,F
      }
      #if añosdensidad=="Todos"
      else
        #elige vehículo, ambos sexos y un marca al menos un año
      {
        if (input$sexodensidad=="Ambos")
        {
          ggplotly(
            datos %>% filter(a %in% añosdensidadInput()) %>%
              filter(  vehi == input$vehiculodensidad) %>% 
              ggplot(aes(edad)) +
              geom_density(         
                aes(fill = vehi), 
                alpha = 0.5, 
                show.legend = T) +
              labs(
                x =  "Edad", 
                y = "Densidad")  + 
              theme_minimal() + 
              guides(fill = guide_legend(title = "Vehículo:")) +
              theme(
                legend.position = "bottom", 
                axis.title  = (element_text(
                  size = 12,
                  colour = "darkslategray"))) + 
              scale_fill_manual (values=colores))
        }
        else
          #Elige vehículo, Femenino o Maculino, y un marca al menos un año.
        {
          ggplotly(
            datos %>% filter(  sexo %in%sexodensidadInput() ) %>% 
              filter(  a %in% añosdensidadInput()  )%>% 
              filter( vehi == input$vehiculodensidad )%>% 
              ggplot(aes(edad)) +
              geom_density(
                aes(fill = vehi), 
                alpha = 0.5, 
                show.legend = T) +
              labs(
                x =  "Edad", 
                y = "Densidad")  +
              theme_minimal() + 
              guides(fill = guide_legend(title = "Vehículo:")) +
              theme(
                legend.position = "bottom", 
                axis.title  = (element_text(
                  size = 12,
                  colour = "darkslategray"))) + 
              scale_fill_manual (values=colores))
        }
      }
      #if añosdensidad=="Todos"
    }
    
    
  })
  #renderplotly
  
  
  
  #MOSAICOS
  
  #PLOT 1
  añosmosaicoInput <- reactive({input$añosmosaico})
  
  
  output$rol <- renderPlotly({
    if (input$variablemosaico=="Sexo")
    {
      if (is.null(añosmosaicoInput())) 
      {
        ggplotly(
          datos %>% dplyr::count(vehi, sexo) %>% filter(vehi!="PEATON") %>%
            group_by( vehi= ifelse (
              (vehi!="BICICLETA" &
                 vehi!="MOTO" &
                 vehi!="AUTO" &
                 vehi!="BICICLETA" &
                 vehi!="CAMIONETA"),"OTROS", vehi ),sexo) %>%
            summarise(n=sum(n)) %>%
            ggplot() +
            geom_mosaic(aes(
              weight = n,
              x = product( reorder(abbreviate(vehi, 4),-n) ),
              fill = abbreviate( sexo, 1) ),
              alpha=0.64) +
            labs(
              x = "Vehículo del fallecido",
              y = "Proporción de fallecidos") +
            guides(fill = guide_legend(title = "Sexo")) +
            theme_minimal() +
            theme(
              axis.title =
                element_text(
                  size = 12,colour="grey30")) +
            scale_fill_manual( values = colores[c(2,3,1)]))
      }
      else
      {  ggplotly(
        datos %>% filter(a== añosmosaicoInput ())%>%
          dplyr::count(vehi, sexo) %>% filter(vehi!="PEATON") %>%
          group_by( vehi= ifelse (
            (vehi!="BICICLETA" &
               vehi!="MOTO" &
               vehi!="AUTO" &
               vehi!="BICICLETA" &
               vehi!="CAMIONETA"),"OTROS", vehi ),sexo) %>%
          summarise(n=sum(n)) %>%
          ggplot() +
          geom_mosaic(aes(
            weight = n,
            x = product( reorder(abbreviate(vehi, 4),-n) ),
            fill = abbreviate( sexo, 1) ),
            alpha=0.64) +
          labs(
            x = "Vehículo del fallecido",
            y = "Proporción de fallecidos") +
          guides(fill = guide_legend(title = "Sexo")) +
          theme_minimal() +
          theme(
            axis.title =
              element_text(
                size = 12,colour="grey30")) +
          scale_fill_manual( values = colores[c(2,3,1)]))
      }
      
    }
    
    else if (input$variablemosaico=="Rol"){
      
      
      
      if (is.null(añosmosaicoInput()))
        
      {
        
        
        ggplotly(
          datos %>% dplyr::count(vehi,rol) %>% filter(vehi!="PEATON") %>%
            group_by( vehi= ifelse (
              (vehi!="BICICLETA" &
                 vehi!="MOTO" &
                 vehi!="AUTO" &
                 vehi!="BICICLETA" &
                 vehi!="CAMIONETA"),"OTROS", vehi ),rol) %>%
            summarise(n=sum(n)) %>%
            ggplot() +
            geom_mosaic(aes(
              weight = n,
              x = product( reorder(abbreviate(vehi, 4),-n) ),
              fill = abbreviate( rol, 1) ),
              alpha=0.64) +
            labs(
              x = "Vehículo del fallecido",
              y = "Proporción de fallecidos") +
            guides(fill = guide_legend(title = "Rol")) +
            theme_minimal() +
            theme(
              axis.title =
                element_text(
                  size = 12,colour="grey30")) +
            scale_fill_manual( values = colores[c(2,3,1)])
        )
        
      }
      else
      {
        
        ggplotly(
          datos%>% filter(a== añosmosaicoInput ()) %>% dplyr::count(vehi,rol) %>% filter(vehi!="PEATON") %>%
            group_by( vehi= ifelse (
              (vehi!="BICICLETA" &
                 vehi!="MOTO" &
                 vehi!="AUTO" &
                 vehi!="BICICLETA" &
                 vehi!="CAMIONETA"),"OTROS", vehi ),rol) %>%
            summarise(n=sum(n)) %>%
            ggplot() +
            geom_mosaic(aes(
              weight = n,
              x = product( reorder(abbreviate(vehi, 4),-n) ),
              fill = abbreviate( rol, 1) ),
              alpha=0.64) +
            labs(
              x = "Vehículo del fallecido",
              y = "Proporción de fallecidos") +
            guides(fill = guide_legend(title = "Rol")) +
            theme_minimal() +
            theme(
              axis.title =
                element_text(
                  size = 12,colour="grey30")) +
            scale_fill_manual( values = colores[c(2,3,1)]))
        
      }
    }
    
    else if (input$variablemosaico=="Jurisdicción")
    {
      if (is.null(añosmosaicoInput()))
      {
        ggplotly(
          datos %>% dplyr::count(vehi,jur) %>% filter(vehi!="PEATON") %>%
            group_by( vehi= ifelse (
              (vehi!="BICICLETA" &
                 vehi!="MOTO" &
                 vehi!="AUTO" &
                 vehi!="BICICLETA" &
                 vehi!="CAMIONETA"),"OTROS", vehi ),jur) %>%
            summarise(n=sum(n)) %>%
            ggplot() +
            geom_mosaic(aes(
              weight = n,
              x = product( reorder(abbreviate(vehi, 4),-n) ),
              fill = abbreviate( jur, 1) ),
              alpha=0.64) +
            labs(
              x = "Vehículo del fallecido",
              y = "Proporción de fallecidos") +
            guides(fill = guide_legend(title = "Jurisdicción")) +
            theme_minimal() +
            theme(
              axis.title =
                element_text(
                  size = 12,colour="grey30")) +
            scale_fill_manual( values = colores[c(2,3,1)])
        )
      }
      else
      {
        
        ggplotly(
          datos%>% filter(a== añosmosaicoInput()) %>% dplyr::count(vehi,jur) %>% filter(vehi!="PEATON") %>%
            group_by( vehi= ifelse (
              (vehi!="BICICLETA" &
                 vehi!="MOTO" &
                 vehi!="AUTO" &
                 vehi!="BICICLETA" &
                 vehi!="CAMIONETA"),"OTROS", vehi ),jur) %>%
            summarise(n=sum(n)) %>%
            ggplot() +
            geom_mosaic(aes(
              weight = n,
              x = product( reorder(abbreviate(vehi, 4),-n) ),
              fill = abbreviate( jur, 1) ),
              alpha=0.64) +
            labs(
              x = "Vehículo del fallecido",
              y = "Proporción de fallecidos") +
            guides(fill = guide_legend(title = "Jurisdicción")) +
            theme_minimal() +
            theme(
              axis.title =
                element_text(
                  size = 12,colour="grey30")) +
            scale_fill_manual( values = colores[c(2,3,1)]))
        
      }
    }
    
    
    
  })
  
  #PLOT 2
  output$involucrado <- renderPlotly({
    if (is.null(añosmosaicoInput())) 
    {
      if (input$variablemosaico=="Sexo") 
      {
        ggplotly(
          datos  %>%dplyr::count(involucrado, sexo) %>%
            ggplot() +
            geom_mosaic(aes(
              weight = n,
              x = product(sexo),
              fill = involucrado),
              alpha=0.5) +
            labs(
              x = "Sexo",
              y = "Proporción involucrado") +
            theme(
              panel.background = element_rect(
                fill="white"),
              plot.title = element_text(
                size = 16,
                face = "italic",
                colour = "grey20",
                vjust = -2),
              axis.title = element_text(
                size=12,
                colour="grey20")) +
            scale_fill_manual(
              values = colores) +
            guides(
              fill= guide_legend("Involucrado"))
        )
      }
      else if (input$variablemosaico=="Rol")
      {
        ggplotly(
          datos %>%dplyr::count(involucrado, rol) %>%
            ggplot() +
            geom_mosaic(aes(
              weight = n,
              x = product(rol),
              fill = involucrado),
              alpha=0.5) +
            labs(
              x = "Rol",
              y = "Proporción involucrado") +
            theme(
              panel.background = element_rect(
                fill="white"),
              plot.title = element_text(
                size = 16,
                face = "italic",
                colour = "grey20",
                vjust = -2),
              axis.title = element_text(
                size=12,
                colour="grey20")) +
            scale_fill_manual(
              values = colores) +
            guides(
              fill= guide_legend("Involucrado"))
        ) 
      }
      else if (input$variablemosaico=="Jurisdicción")
      {
        ggplotly(
          datos %>%dplyr::count(involucrado, jur) %>%
            ggplot() +
            geom_mosaic(aes(
              weight = n,
              x = product(jur),
              fill = involucrado),
              alpha=0.5) +
            labs(
              x = "Jurisdicción",
              y = "Proporción involucrado") +
            theme(
              panel.background = element_rect(
                fill="white"),
              plot.title = element_text(
                size = 16,
                face = "italic",
                colour = "grey20",
                vjust = -2),
              axis.title = element_text(
                size=12,
                colour="grey20")) +
            scale_fill_manual(
              values = colores) +
            guides(
              fill= guide_legend("Involucrado"))
        ) 
      }
    }
    else
      
    {
      if (input$variablemosaico=="Sexo") 
      {
        ggplotly(
          datos %>% filter(a== añosmosaicoInput()) %>%dplyr::count(involucrado, sexo) %>%
            ggplot() +
            geom_mosaic(aes(
              weight = n,
              x = product(sexo),
              fill = involucrado),
              alpha=0.5) +
            labs(
              x = "Sexo",
              y = "Proporción involucrado") +
            theme(
              panel.background = element_rect(
                fill="white"),
              plot.title = element_text(
                size = 16,
                face = "italic",
                colour = "grey20",
                vjust = -2),
              axis.title = element_text(
                size=12,
                colour="grey20")) +
            scale_fill_manual(
              values = colores) +
            guides(
              fill= guide_legend("Involucrado"))
        )
      }
      else if (input$variablemosaico=="Rol")
      {
        ggplotly(
          datos %>%  filter(a== añosmosaicoInput())%>%dplyr::count(involucrado, rol) %>%
            ggplot() +
            geom_mosaic(aes(
              weight = n,
              x = product(rol),
              fill = involucrado),
              alpha=0.5) +
            labs(
              x = "Rol",
              y = "Proporción involucrado") +
            theme(
              panel.background = element_rect(
                fill="white"),
              plot.title = element_text(
                size = 16,
                face = "italic",
                colour = "grey20",
                vjust = -2),
              axis.title = element_text(
                size=12,
                colour="grey20")) +
            scale_fill_manual(
              values = colores) +
            guides(
              fill= guide_legend("Involucrado"))
        ) 
      }
      else if (input$variablemosaico=="Jurisdicción")
      {
        ggplotly(
          datos %>%  filter(a== añosmosaicoInput()) %>%dplyr::count(involucrado, jur) %>%
            ggplot() +
            geom_mosaic(aes(
              weight = n,
              x = product(jur),
              fill = involucrado),
              alpha=0.5) +
            labs(
              x = "Jurisdicción",
              y = "Proporción involucrado") +
            theme(
              panel.background = element_rect(
                fill="white"),
              plot.title = element_text(
                size = 16,
                face = "italic",
                colour = "grey20",
                vjust = -2),
              axis.title = element_text(
                size=12,
                colour="grey20")) +
            scale_fill_manual(
              values = colores) +
            guides(
              fill= guide_legend("Involucrado"))
        ) 
      }
    }
    
    
    
    
    
    
    
    
  })
  
  
  
  
  
  #SERIES DE TIEMPO
  
  intervaloInput <- reactive({
    switch(input$intervalo,
           "1 mes"= "1 month",
           "3 meses"= "3 months",
           "6 meses" = "6 months"
    )
  })
  
  output$fecha <- renderPlot({
    
    if(input$zoom==T)
    {
      
      
      
      if(input$fallecidos=="Total")
      {
        if(input$puntoylinea==T) {
          
          data.frame(datos,f) %>%
            count( a.mes=    floor_date(f.h, intervaloInput()))  %>%
            ggplot( 
              aes(
                as_date(a.mes),
                n
              ) ) +geom_point()+   
            geom_line(
              stat="identity")+
            geom_smooth(
              method = "loess",
              se=input$error,
              cex=1.4)+
            labs(
              x = "Año",
              y = "Frecuencia absoluta de fallecidos") +
            theme_minimal() +
            theme(
              axis.title = element_text(
                colour="grey30", 
                size=12),
              axis.text =element_text(
                colour = "grey27",
                size=9)       )  +
            scale_x_date( 
              date_labels =("%Y"),
              date_breaks = "1 year" )+
            scale_color_manual(values=colores)}
        
        else {  data.frame(datos,f) %>%
            count( a.mes=    floor_date(f.h, intervaloInput()))  %>%
            ggplot( 
              aes(
                as_date(a.mes),
                n
              ) ) +
            geom_smooth(
              method = "loess",
              se=input$error,
              cex=1.4)+
            labs(
              x = "Año",
              y = "Frecuencia absoluta de fallecidos") +
            theme_minimal() +
            theme(
              axis.title = element_text(
                colour="grey30", 
                size=12),
              axis.text =element_text(
                colour = "grey27",
                size=9)       )  +
            scale_x_date( 
              date_labels =("%Y"),
              date_breaks = "1 year" )+
            scale_color_manual(values=colores)}
        
        
      } 
      
      else if(input$fallecidos=="Sexo"){
        
        if(input$puntoylinea==T) {
          
          data.frame(datos,f) %>%
            count(sexo, a.mes=    floor_date(f.h, intervaloInput()))  %>%
            ggplot( 
              aes(
                as_date(a.mes),
                n,
                colour=sexo
              ) ) + 
            geom_point()  + 
            geom_line(
              stat="identity")+ 
            geom_smooth(
              method = "loess",
              se=input$error,
              cex=1.4)+
            labs(
              x = "Año",
              y = "Frecuencia absoluta de fallecidos") +
            theme_minimal() +
            theme(
              axis.title = element_text(
                colour="grey30", 
                size=12),
              axis.text =element_text(
                colour = "grey27",
                size=9)       )  +
            scale_x_date( 
              date_labels =("%Y"),
              date_breaks = "1 year" ) }
        
        else {data.frame(datos,f) %>%
            count(sexo, a.mes=    floor_date(f.h, intervaloInput()))  %>%
            ggplot( 
              aes(
                as_date(a.mes),
                n,
                colour=sexo
              ) ) + 
            geom_smooth(
              method = "loess",
              se=input$error,
              cex=1.4)+
            labs(
              x = "Año",
              y = "Frecuencia absoluta de fallecidos") +
            theme_minimal() +
            theme(
              axis.title = element_text(
                colour="grey30", 
                size=12),
              axis.text =element_text(
                colour = "grey27",
                size=9)       )  +
            scale_x_date( 
              date_labels =("%Y"),
              date_breaks = "1 year" )}
        
        
      }
      else if(input$fallecidos=="Rol"){
        
        if(input$puntoylinea==T) {
          
          data.frame(datos,f) %>%
            count(rol, a.mes=    floor_date(f.h, intervaloInput()))  %>%
            ggplot( 
              aes(
                as_date(a.mes),
                n,
                colour=rol
              ) ) + 
            geom_point()  + 
            geom_line(
              stat="identity")+ 
            geom_smooth(
              method = "loess",
              se=input$error,
              cex=1.4)+
            labs(
              x = "Año",
              y = "Frecuencia absoluta de fallecidos") +
            theme_minimal() +
            theme(
              axis.title = element_text(
                colour="grey30", 
                size=12),
              axis.text =element_text(
                colour = "grey27",
                size=9)       )  +
            scale_x_date( 
              date_labels =("%Y"),
              date_breaks = "1 year" ) +
            scale_color_manual(values=colores)}
        
        else {   data.frame(datos,f) %>%
            count(rol, a.mes=    floor_date(f.h, intervaloInput()))  %>%
            ggplot( 
              aes(
                as_date(a.mes),
                n,
                colour=rol
              ) ) + 
            geom_smooth(
              method = "loess",
              se=input$error,
              cex=1.4)+
            labs(
              x = "Año",
              y = "Frecuencia absoluta de fallecidos") +
            theme_minimal() +
            theme(
              axis.title = element_text(
                colour="grey30", 
                size=12),
              axis.text =element_text(
                colour = "grey27",
                size=9)       )  +
            scale_x_date( 
              date_labels =("%Y"),
              date_breaks = "1 year" ) +
            scale_color_manual(values=colores)}
        
      }
      else if(input$fallecidos=="Jurisdicción"){
        
        if(input$puntoylinea==T) {
          
          data.frame(datos,f) %>%
            count(jur, a.mes=    floor_date(f.h, intervaloInput()))  %>%
            ggplot( 
              aes(
                as_date(a.mes),
                n,
                colour=jur
              ) ) + 
            geom_point()  + 
            geom_line(
              stat="identity")+ 
            geom_smooth(
              method = "loess",
              se=input$error,
              cex=1.4)+
            labs(
              x = "Año",
              y = "Frecuencia absoluta de fallecidos") +
            theme_minimal() +
            theme(
              axis.title = element_text(
                colour="grey30", 
                size=12),
              axis.text =element_text(
                colour = "grey27",
                size=9)       )  +
            scale_x_date( 
              date_labels =("%Y"),
              date_breaks = "1 year" )+
            scale_color_manual(values=colores)}
        else {  
          data.frame(datos,f) %>%
            count(jur, a.mes=    floor_date(f.h, intervaloInput() ))  %>%
            ggplot( 
              aes(
                as_date(a.mes),
                n,
                colour=jur
              ) ) + 
            geom_smooth(
              method = "loess",
              se=input$error,
              cex=1.4)+
            labs(
              x = "Año",
              y = "Frecuencia absoluta de fallecidos") +
            theme_minimal() +
            theme(
              axis.title = element_text(
                colour="grey30", 
                size=12),
              axis.text =element_text(
                colour = "grey27",
                size=9)       )  +
            scale_x_date( 
              date_labels =("%Y"),
              date_breaks = "1 year" ) +
            scale_color_manual(values=colores)}
        
      }
      
      
      
    }
    
    
    
    else
      #MARCA ZOOM
      
    {
      
      
      
      if(input$fallecidos=="Total")
      {
        if(input$puntoylinea==T) {
          
          data.frame(datos,f) %>%
            count( a.mes=    floor_date(f.h, intervaloInput()))  %>%
            ggplot( 
              aes(
                as_date(a.mes),
                n
              ) ) +geom_point()+   
            geom_line(
              stat="identity")+
            geom_smooth(
              method = "loess",
              se=input$error,
              cex=1.4)+
            labs(
              x = "Año",
              y = "Frecuencia absoluta de fallecidos") +
            theme_minimal() +
            theme(
              axis.title = element_text(
                colour="grey30", 
                size=12),
              axis.text =element_text(
                colour = "grey27",
                size=9)       )  +
            scale_x_date( 
              date_labels =("%Y"),
              date_breaks = "1 year" )+
            scale_color_manual(values=colores) +
            coord_cartesian(ylim=c(0,max( (data.frame(datos,f) %>%
                                             count( a.mes=    floor_date(f.h, intervaloInput())))$n )))}
        
        else {  data.frame(datos,f) %>%
            count( a.mes=    floor_date(f.h, intervaloInput()))  %>%
            ggplot( 
              aes(
                as_date(a.mes),
                n
              ) ) +
            geom_smooth(
              method = "loess",
              se=input$error,
              cex=1.4)+
            labs(
              x = "Año",
              y = "Frecuencia absoluta de fallecidos") +
            theme_minimal() +
            theme(
              axis.title = element_text(
                colour="grey30", 
                size=12),
              axis.text =element_text(
                colour = "grey27",
                size=9)       )  +
            scale_x_date( 
              date_labels =("%Y"),
              date_breaks = "1 year" )+
            scale_color_manual(values=colores)+
            coord_cartesian(ylim=c(0,max( (data.frame(datos,f) %>%
                                             count( a.mes=    floor_date(f.h, intervaloInput())) )$n )))  }
        
        
      } 
      
      else if(input$fallecidos=="Sexo"){
        
        if(input$puntoylinea==T) {
          
          data.frame(datos,f) %>%
            count(sexo, a.mes=    floor_date(f.h, intervaloInput()))  %>%
            ggplot( 
              aes(
                as_date(a.mes),
                n,
                colour=sexo
              ) ) + 
            geom_point()  + 
            geom_line(
              stat="identity")+ 
            geom_smooth(
              method = "loess",
              se=input$error,
              cex=1.4)+
            labs(
              x = "Año",
              y = "Frecuencia absoluta de fallecidos") +
            theme_minimal() +
            theme(
              axis.title = element_text(
                colour="grey30", 
                size=12),
              axis.text =element_text(
                colour = "grey27",
                size=9)       )  +
            scale_x_date( 
              date_labels =("%Y"),
              date_breaks = "1 year" )+
            coord_cartesian(ylim=c(0,max( (data.frame(datos,f) %>%
                                             count(sexo, a.mes=    floor_date(f.h, intervaloInput())))$n )) ) }
        
        else {
          data.frame(datos,f) %>%
            count(sexo, a.mes=    floor_date(f.h, intervaloInput()))  %>%
            ggplot( 
              aes(
                as_date(a.mes),
                n,
                colour=sexo
              ) ) + 
            geom_smooth(
              method = "loess",
              se=input$error,
              cex=1.4)+
            labs(
              x = "Año",
              y = "Frecuencia absoluta de fallecidos") +
            theme_minimal() +
            theme(
              axis.title = element_text(
                colour="grey30", 
                size=12),
              axis.text =element_text(
                colour = "grey27",
                size=9)       )  +
            scale_x_date( 
              date_labels =("%Y"),
              date_breaks = "1 year" )+
            coord_cartesian(ylim=c(0,max((data.frame(datos,f) %>%
                                            count(sexo, a.mes=    floor_date(f.h, intervaloInput())) )$n)) )}
        
        
      }
      else if(input$fallecidos=="Rol"){
        
        if(input$puntoylinea==T) {
          
          data.frame(datos,f) %>%
            count(rol, a.mes=    floor_date(f.h, intervaloInput()))  %>%
            ggplot( 
              aes(
                as_date(a.mes),
                n,
                colour=rol
              ) ) + 
            geom_point()  + 
            geom_line(
              stat="identity")+ 
            geom_smooth(
              method = "loess",
              se=input$error,
              cex=1.4)+
            labs(
              x = "Año",
              y = "Frecuencia absoluta de fallecidos") +
            theme_minimal() +
            theme(
              axis.title = element_text(
                colour="grey30", 
                size=12),
              axis.text =element_text(
                colour = "grey27",
                size=9)       )  +
            scale_x_date( 
              date_labels =("%Y"),
              date_breaks = "1 year" ) +
            scale_color_manual(values=colores) +
            coord_cartesian(ylim=c(0,max( (data.frame(datos,f) %>%
                                             count(rol, a.mes=    floor_date(f.h, intervaloInput())))$n)))}
        
        else {   data.frame(datos,f) %>%
            count(rol, a.mes=    floor_date(f.h, intervaloInput()))  %>%
            ggplot( 
              aes(
                as_date(a.mes),
                n,
                colour=rol
              ) ) + 
            geom_smooth(
              method = "loess",
              se=input$error,
              cex=1.4)+
            labs(
              x = "Año",
              y = "Frecuencia absoluta de fallecidos") +
            theme_minimal() +
            theme(
              axis.title = element_text(
                colour="grey30", 
                size=12),
              axis.text =element_text(
                colour = "grey27",
                size=9)       )  +
            scale_x_date( 
              date_labels =("%Y"),
              date_breaks = "1 year" ) +
            scale_color_manual(values=colores)+
            coord_cartesian(ylim=c(0,max( ( data.frame(datos,f) %>%
                                              count(rol, a.mes=    floor_date(f.h, intervaloInput())))$n)))}
        
      }
      else if(input$fallecidos=="Jurisdicción"){
        
        if(input$puntoylinea==T) {
          
          data.frame(datos,f) %>%
            count(jur, a.mes=    floor_date(f.h, intervaloInput()))  %>%
            ggplot( 
              aes(
                as_date(a.mes),
                n,
                colour=jur
              ) ) + 
            geom_point()  + 
            geom_line(
              stat="identity")+ 
            geom_smooth(
              method = "loess",
              se=input$error,
              cex=1.4)+
            labs(
              x = "Año",
              y = "Frecuencia absoluta de fallecidos") +
            theme_minimal() +
            theme(
              axis.title = element_text(
                colour="grey30", 
                size=12),
              axis.text =element_text(
                colour = "grey27",
                size=9)       )  +
            scale_x_date( 
              date_labels =("%Y"),
              date_breaks = "1 year" )+
            scale_color_manual(values=colores)+
            coord_cartesian(ylim=c(0,max( (data.frame(datos,f) %>%
                                             count(jur, a.mes=    floor_date(f.h, intervaloInput())) )$n )))}
        else {  
          data.frame(datos,f) %>%
            count(jur, a.mes=    floor_date(f.h, intervaloInput() ))  %>%
            ggplot( 
              aes(
                as_date(a.mes),
                n,
                colour=jur
              ) ) + 
            geom_smooth(
              method = "loess",
              se=input$error,
              cex=1.4)+
            labs(
              x = "Año",
              y = "Frecuencia absoluta de fallecidos") +
            theme_minimal() +
            theme(
              axis.title = element_text(
                colour="grey30", 
                size=12),
              axis.text =element_text(
                colour = "grey27",
                size=9)       )  +
            scale_x_date( 
              date_labels =("%Y"),
              date_breaks = "1 year" ) +
            scale_color_manual(values=colores)+
            coord_cartesian(ylim=c(0,max( (data.frame(datos,f) %>%
                                             count(jur, a.mes=    floor_date(f.h, intervaloInput() )) )$n)))}
        
      }
    }
    
  })
  #serie de tiempo
  
  
  
  #FECHA Y HORA
  
  añosfechayhora<- reactive({input$añosfechayhora})
  
  
  sexofechayhoraInput<- reactive({
    switch (input$sexofechayhora,
            "Ambos"=data.frame(datos,f),
            "Femenino"= data.frame(datos,f) %>% filter(sexo=="F"),
            "Masculino"=data.frame(datos,f) %>% filter(sexo=="M")  )
  })

  
  output$tilefechayhora <- renderPlot({
     

    if(is.null(añosfechayhora()))
      #no marca año
    {
      
      sexofechayhoraInput() %>% filter(f.dias<=input$sdias) %>%  count(n.dia, hrs= format(f.h,"%H")) %>%
        ggplot(aes(hrs, ordered(
          n.dia,
          levels = c( "lunes",  "martes",  "miércoles",
                      "jueves",  "viernes","sábado", "domingo" )))) +
        geom_tile(aes(fill = n)) +
        scale_fill_gradient2(
          low = "white",
          high = "midnightblue",
          mid = "turquoise",
          midpoint = mean(
            (data.frame(datos,f) %>% count(n.dia, hrs=format(f.h,"%H")))$n  )) +
        labs(
          x = "Hora",
          y = "Día",
          fill="Fallecidos") +
        theme(
          axis.title = element_text(
            size=13,
            colour="grey34",
            face="bold"),
          axis.text = element_text(
            size=10.4),
          panel.background = element_rect(
            colour="white"))
    }
    else
      #marca al menos un año
    {
      sexofechayhoraInput() %>% filter(a%in%añosfechayhora() )%>% filter(f.dias<=input$sdias) %>% count(n.dia, hrs= format(f.h,"%H")) %>%
        ggplot(aes(hrs, ordered(
          n.dia,
          levels = c( "lunes",  "martes",  "miércoles",
                      "jueves",  "viernes","sábado", "domingo" )))) +
        geom_tile(aes(fill = n)) +
        scale_fill_gradient2(
          low = "white",
          high = "midnightblue",
          mid = "turquoise",
          midpoint = mean(
            (data.frame(datos,f) %>% filter(a%in%añosfechayhora())%>% count(n.dia, hrs=format(f.h,"%H")))$n  )) +
        labs(
          x = "Hora",
          y = "Día",
          fill="Fallecidos") +
        theme(
          axis.title = element_text(
            size=13,
            colour="grey34",
            face="bold"),
          axis.text = element_text(
            size=10.4),
          panel.background = element_rect(
            colour="white")) 
    }
    
    
  })
  
  
}
#SERVER

################################################################################################

shinyApp(ui = ui, server = server)

################################################################################################
