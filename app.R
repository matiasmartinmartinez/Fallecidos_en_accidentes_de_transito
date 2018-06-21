
################################################################################################
################################################################################################

#A continuación se cargan paquetes y datos a utilizar, además se genera una paleta de colores elegida arbitariamente.

#Función que instala y carga paquetes necesarios para correr el código.
ipack <- function( pkg )
{
  new.pkg <-  pkg[ ! (pkg %in% installed.packages()[, "Package"]) ]
  if ( length(new.pkg) ) 
    install.packages(
      new.pkg, dependencies = TRUE )
  sapply( pkg, require, character.only = TRUE )
}
paquetes.a.utilizar<- c( "tidyverse", "rmarkdown", "shiny", "ggmosaic", "plotly",
                         "ggmap", "raster", "rgdal","knitr", 
                         "scales", "lubridate", "devtools","grid","gridExtra")
ipack(paquetes.a.utilizar)

#Datos a utilizar
load("base_datos___fallecidos_transito_uruguay_2013-2017.RData")

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
censo<- c(73378,520187,84698,123203,57088,
          25050,67048,58815,164300,1319108,
          113124,54765,103493,68088,124878,
          108309,82595,90053,48134)

# Tasa de fallecidos en accidentes de tránsito cada 10.000 habitantes.
pob.dep<- cbind((datos %>% count(dep) %>% arrange(dep)),censo)%>% mutate(tasa=(n/censo)*10000)

################################################################################################
################################################################################################







ui <- fluidPage(
  
  titlePanel( h1("Siniestros fatales de tránsito en Uruguay, 2013-2017")),
  
  tabsetPanel(
    
    
    tabPanel(
      title = h6("Distribución territorial"),
      hr(),
      
      plotOutput("mapa")
      
      
      
      
      
      ),  
    tabPanel(
      title = h6("Densidad según vehículo"),
      hr()
      ),
    tabPanel(
      title = h6("Comportamiento del rol"),
      hr()
     ),
    tabPanel(
      title = h6("Fecha del siniestro")
      )
    )
  )
  

################################################################################################
################################################################################################





server <-  function(input, output) {
  output$mapa <- renderPlot({
  
    
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
  })
  }



################################################################################################
################################################################################################

shinyApp(ui = ui, server = server)

################################################################################################
################################################################################################

