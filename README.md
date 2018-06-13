# Trabajo-Final-NT_Fallecidos_transito
Datos de siniestros de tránsito fatales ocurridos en Uruguay desde 2013 a 2017.



A realizar:

- Siendo la jurisdicción departamental, ¿qué vehículo ocasiona mayores fallecimientos de transeúntes?
- Shiny
- as_date()
- paleta colores
- probar:

datos %>% separate(fecha,c("dia","mes")) %>% 
  mutate(a.mes=paste(a,mes,sep="-") ) %>%
  count(a.mes) %>% arrange(desc(a.mes)) %>%mutate(  a.mes=as_date(as.POSixtc(a.mes,"%Y%m"),"%Y%m")  )  %>%

     ggplot( 
       aes(a.mes,n,group=1) ) + 
     geom_point()  + 
  geom_line(
    stat="identity")+ 
  geom_smooth(
    method = "loess",
    colour=colores[2])+
     labs(
         x = "Año y mes",
         y = "Frecuencia absoluta") +
     theme_minimal() +
     theme(
         plot.title = element_text(
             size = 19,
             colour = "grey39",
             face = "bold",
             hjust = 0.4),
         axis.text.x =   element_text(
             colour = "grey27",angle=90,size=6)) +
  coord_cartesian( ylim = c(0,75)  )+ scale_x_date( labels = date_format("%Y%-m") ) 
