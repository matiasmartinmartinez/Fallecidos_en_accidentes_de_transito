################################################################################################
#A continuación se cargan paquetes y datos a utilizar, además se genera una paleta de colores elegida arbitariamente.

#Función que instala y carga paquetes necesarios para correr el código.
ipack <- function( pkg ) {
  new.pkg <-  pkg[ ! (pkg %in% installed.packages()[, "Package"]) ]
  if ( length(new.pkg) ) 
    install.packages(
      new.pkg, dependencies = TRUE )
  sapply( pkg, require, character.only = TRUE ) }

paquetes.a.utilizar<- c( "tidyverse", "rmarkdown", "shiny", "ggmosaic", "plotly", "ggmap", "raster", "rgdal", "knitr", "scales", "lubridate", "devtools","grid", "gridExtra")
ipack(paquetes.a.utilizar)

#Datos a utilizar
#load("base_datos___fallecidos_transito_uruguay_2013-2017.RData")
#Paleta
colores<-c("darkolivegreen3","turquoise4","tan2","indianred",
           "khaki3", "thistle4", "lightsteelblue","grey80")

#Se cargan polígonos y coordenadas pertenecientes al territorio uruguayo, delimitados por departamentos.
uruguay <- getData("GADM", country = "UY", level = 0)
uruguay_states <- getData("GADM", country = "UY", level = 1)
uystates_UTM <-spTransform(uruguay_states, CRS("+init=EPSG:5383"))
NAME_1 <- uystates_UTM@data$NAME_1

# Función mapa
mapeo<-  function(n) 
{
  
  df <- data.frame(NAME_1,n)
  uystates_UTM@data$id <- rownames(uystates_UTM@data)
  uystates_UTM@data <- plyr::join(uystates_UTM@data, df, by="NAME_1")
  uystates_df <- fortify(uystates_UTM)
  uystates_df <- plyr::join(uystates_df,uystates_UTM@data, by="id")
  uystates_df <-uystates_df %>% filter(!(NAME_1=="Rivera"& lat<6400000)) 
  
  theme_opts <-  list(  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    plot.background =  element_blank(),
    axis.line =    element_blank(),
    axis.text.x =  element_blank(),
    axis.text.y =  element_blank(),
    axis.ticks =   element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title =   element_text(
      size=16.4,
      colour="darkslategrey",
      hjust=0.25))   )
  
  ggplot() + geom_polygon(
    data = uystates_df,
    aes(
      x = long,
      y = lat,
      group = group,
      fill = n),
    color = "black",
    size = 0.25) +
    theme(aspect.ratio = 1) + 
    theme_opts
}

#Población por departamento para el año más reciente. Censo 2011, fuente: INE.
censo<- c(73378,520187,84698,123203,57088, 25050,67048,58815,164300,1319108,   113124,54765,103493,68088,124878,  108309,82595,90053,48134)

# Tasa de fallecidos en accidentes de tránsito cada 10.000 habitantes.
pob.dep<- cbind((datos %>% count(dep) %>% arrange(dep)),censo)%>% mutate(tasa=(n/censo)*10000)

#Cargar fechas
fecha<- datos %>% separate( fecha,c("dia","mes") ) %>%
  mutate( f.h= paste( paste(a,mes,dia,sep="-") , hora ) ) %>%
  mutate( f.h= as.POSIXct(f.h, format="%Y-%m-%d %H:%M:%S",tz="GMT"))%>%
  dplyr::select(f.h)

n.dia <- weekdays( as.Date( as.vector(t (fecha[1]) ) ) )

f<- data.frame(fecha,n.dia)

################################################################################################

ui <- fluidPage(
  
  
  titlePanel( h1("Siniestros fatales de tránsito en Uruguay, 2013-2017")),
  
  tabsetPanel(
    
    tabPanel(
      title = h6("Distribución territorial"),
      hr(),
      
      sidebarLayout(
        
        sidebarPanel(
          
          checkboxGroupInput("años", "Elegir Año:",
                             choiceNames = c("2013", "2014", "2015", "2016","2017"),
                             choiceValues =  c("2013", "2014", "2015", "2016","2017"))
        ),
        
        
        mainPanel(
          plotOutput("mapa")
          
        )
      )
    ),  
    
    
    
    #DENSIDAD
    
    tabPanel(
      title = h6("Densidad según vehículo"),
      hr(),
      
      sidebarLayout(
        
        sidebarPanel(
          
          selectInput("sexodensidad", "Sexo:",
                      choices = c("Ambos", "Femenino", "Masculino")),
          
          selectInput("añosdensidad", "Elegir año:",
                      choices = c("Todos","2017", "2016", "2015", "2014","2013")),
          
          checkboxGroupInput("vehiculodensidad", "Vehículo:",
                             choiceNames = c("Auto","Camión","Camioneta" ,"Moto", "Peatón"),
                             choiceValues =  c("AUTO", "CAMION", "CAMIONETA", "MOTO","PEATON"))
          
        ),
        mainPanel(   
          plotlyOutput("densidad"),
          p(" facetear por sexo, indicar vehiculo y año")
        )
      )
    ),
    
    
    tabPanel(
      title = h6("Mosaicos"),
      hr(),
      
      sidebarLayout(
        
        sidebarPanel(
          
          selectInput("variablemosaico", "Variable:",
                      choices = c("Sexo","Rol", "Jurisdicción")),
          
          conditionalPanel(
            condition = "input.variablemosaico!='Sexo'",
            selectInput("sexomosaico", "Sexo:",
                        choices = c("Facetear", "No facetear") ))          ,
          
          checkboxGroupInput("añosmosaico", "Elegir Año:",
                             choiceNames = c("2013", "2014", "2015", "2016","2017"),
                             choiceValues =  c("2013", "2014", "2015", "2016","2017"))
          
          
          
          
        ),
        mainPanel( 
          
          plotlyOutput("rol"),
          plotlyOutput("involucrado"),
          p("sexo y año para ambos,   rol y jurisdiccion  para involucrado ")
        )
      )
    ),
    
    tabPanel(
      title = h6("Series de tiempo"),
      sidebarLayout(
        
        
        sidebarPanel(
          
          selectInput("fallecidos", "Fallecidos:",
                      choices = c("Total", "Sexo", "Rol","Jurisdicción")),
          selectInput("intervalo", "Intervalo:",
                      choices = c("1 mes","3 mes", "6 meses", "1 año")),
          checkboxInput("error", "Error", TRUE),
          checkboxInput("puntoylinea", "Punto y línea", TRUE)
        ),
        mainPanel(
          plotOutput("fecha")
          
        )
      )
    )))

################################################################################################

server <- function(input, output) {
  
  output$mapa <- renderPlot({
    
    if(input$años==FALSE)
      
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
    else {
    
    mapeo(n= (datos %>% filter( a==input$años |
                                  a==input$años |
                                  a==input$años |
                                  a==input$años |
                                  a==input$años |
                                  a==input$años )%>% count(a,dep) %>% arrange(a) %>%
                mutate(censo=(rep(censo,(length(table(a)))))) %>% mutate(n= ((n/censo)*10000))) ) +
      labs(
        fill = "Tasa",
        x = NULL,
        y = NULL) +
      scale_fill_gradient2(
        low = "#d8b365",
        mid = "white",
        high = "#5ab4ac",
        midpoint =  mean((datos %>% filter( a==input$años |
                                              a==input$años |
                                              a==input$años |
                                              a==input$años |
                                              a==input$años |
                                              a==input$años  ) %>% count(a,dep) %>% arrange(a) %>% mutate(
                                                censo= (rep(censo,(length(table(a)))))) %>% mutate(n=(n/censo)*10000))$n )) +
      facet_wrap( ~ a)
    }
    
    })
  
  
  
  
  
  
  
  
                                     #DENSIDAD
  
  sexodensidadInput <- reactive({   
    switch(input$sexodensidad,
           "Femenino"= "F",
           "Masculino" = "M")
    })
  
  
  añosdensidadInput <- reactive({   
    switch(input$añosdensidad,
           "2017"="2017",
           "2016"="2016",
           "2015"="2015",
           "2014"="2014",
           "2013"="2013"
           )
  })
  
  
  output$densidad <- renderPlotly({
    
    if(input$añosdensidad=="Todos")
      {
      if (input$sexodensidad=="Ambos")      
        {  
        ggplotly(
          datos  %>% filter(vehi== "AUTO" | vehi=="MOTO")%>% 
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
            datos %>% filter(sexo==sexodensidadInput()) %>% 
              filter(vehi== "AUTO" | vehi=="MOTO")%>% 
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
    }# iis.null
    else
      {
        if (input$sexodensidad=="Ambos")
          {
          ggplotly(
            datos %>% filter(a==añosdensidadInput())%>%filter(vehi== "AUTO" | vehi=="MOTO")%>% 
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
              datos %>% filter(sexo==sexodensidadInput()) %>% 
                filter(a==añosdensidadInput())%>% filter(vehi== "AUTO" | vehi=="MOTO")%>% 
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
    #if else is.null
    
  })
  
  output$rol <- renderPlotly({
    ggplotly(
      datos %>% filter(a=="2013")%>% filter(sexo=="F")%>%dplyr::count(vehi, rol) %>% filter(vehi!="PEATON") %>%
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
  })
  
  output$involucrado <- renderPlotly({
    ggplotly(
      datos %>% filter(a=="2017") %>%dplyr::count(involucrado, sexo) %>%
        ggplot() +
        geom_mosaic(aes(
          weight = n,
          x = product(sexo),
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
          fill= guide_legend("Involucrado")))
    
  })
  
  #SERIES DE TIEMPO
  
  intervaloInput <- reactive({
    switch(input$intervalo,
           "1 mes"= "1 month",
           "3 mes"= "3 months",
           "6 meses" = "6 months",
           "1 año" = "1 year")
    
  })
  
  output$fecha <- renderPlot({
    
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
          count(jur, a.mes=    floor_date(f.h, intervaloInput()))  %>%
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
    
  })
  
}

################################################################################################

shinyApp(ui = ui, server = server)

################################################################################################
