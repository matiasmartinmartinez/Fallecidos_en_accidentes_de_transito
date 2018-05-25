# Trabajo-Final-NT_Fallecidos_transito
Datos de siniestros de tránsito fatales ocurridos en Uruguay desde 2013 a 2017.



A realizar:

- posible densidad(0, segun edad) 
- mosaico jur (solo u otro vehi) crear variable solo o no
- mosaico filtrado por n>=x  tipo.sin segun jurisdiccion 
datos %>% count(tipo.sin,jur) %>% filter(n>=5) %>% ggplot() + geom_mosaic(aes(weight=n,x=product(tipo.sin,jur),fill=jur))
