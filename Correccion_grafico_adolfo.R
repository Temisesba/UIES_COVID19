
library(tidyverse)
mortgraf <- read.csv("C:\\Users\\alan2\\Downloads\\mortgraf.csv") %>% 
  mutate(fecha = as.Date(fecha)) %>% 
  select(fecha,media_mortalidad = media )

tmediagraf <- read.csv("C:\\Users\\alan2\\Downloads\\tmediagraf.csv") %>% 
  mutate(fecha = as.Date(fecha))%>% 
  select(fecha,media_temperatura = media )

#Ajustamos la escala entre los valores máximos
factors <- max(mortgraf$media_mortalidad) / max(tmediagraf$media_temperatura)



ggplot() +
  geom_point(data = mortgraf, aes(x=fecha, y = media_mortalidad), size = 1, alpha = 1)+
  geom_line(data = mortgraf, aes(x=fecha, y = media_mortalidad), size = 0.3, alpha = 0.3)+
  geom_smooth(data = mortgraf, aes(x=fecha, 
                                   y = media_mortalidad), 
              alpha = 0.1,
              linetype = "dashed")+
  theme_light()+
  theme(legend.position = "none") +
  
  
  geom_point(data = tmediagraf, aes(x=fecha, y = media_temperatura*factors), size = 1, alpha = 1)+
  geom_line(data = tmediagraf, aes(x=fecha, y = media_temperatura*factors), size = 0.3, alpha = 0.3)+
  geom_smooth(data = tmediagraf, aes(x=fecha, 
                                     y = media_temperatura*factors), 
              alpha = 0.1, 
              linetype = "dashed") + 
  
  
  scale_y_continuous(name = "Defunciones", 
                     sec.axis = sec_axis(~./factors,
                                         name = "Temperatura Media °C"))+
  
  theme(axis.text.y.right = element_text(color = "#A5103D"),
        axis.title.y.right = element_text(color = "#A5103D"))+
  
  labs(title = "Temperatura y Mortalidad Nacional 2014 - 2020",
       x = "Años")



##PROPUESTA##
#Creamos un solo dataframe y utilizamos el método pivot_longer
mort_tmedia<-left_join(mortgraf, tmediagraf , by="fecha" ) %>% 
  select(fecha, Mortalidad = media_mortalidad, Temperatura = media_temperatura ) %>%
  mutate(Temperatura = Temperatura*factors) %>% 
  pivot_longer(., c("Mortalidad", "Temperatura"), names_to = "Categoria", values_to = "Valor" )


ggplot(mort_tmedia)+
  geom_line(aes(x= fecha , y = Valor, col = Categoria),size = 0.3, alpha = 0.3)+
  geom_point(aes(x=fecha, y = Valor, col = Categoria),  size = 1, alpha = 0.5)+
  geom_smooth(aes(x= fecha , y = Valor, col = Categoria, fill = Categoria), 
              linetype = "dashed", alpha= 0.2)+
  
  
  theme_light()+
  
  scale_y_continuous(name = "Defunciones", 
                     sec.axis = sec_axis(~./factors,
                                         name = "Temperatura Media °C", 
                                         labels = function(Valor) { paste0(round(Valor, 0), "°")} ))+
  
  theme(axis.text.y.right = element_text(color = "#A5103D"),
        axis.title.y.right = element_text(color = "#A5103D"))+
  
  scale_color_manual( values = c("Mortalidad"= "#806153", 
                                 "Temperatura" = "#A5103D"))+
  scale_fill_manual( values = c("Mortalidad"= "#806153", 
                                 "Temperatura" = "#A5103D"))+
  labs(title = "Temperatura y Mortalidad Nacional 2014 - 2020",
       x = "Años")

