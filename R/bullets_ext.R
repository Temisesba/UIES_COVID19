
source("R/funcion_bullets.R", encoding= "UTF-8")
pacman::p_load(ggthemes, tidyverse, data.table, extrafont, showtext, lubridate,
               magrittr, flextable, officer, stringi)

fecha_de_trabajo <- Sys.Date()-3
OMS<-reporte_de_situacion(fecha_de_trabajo)

situacion <- data.frame(OMS[1])
longitud <- OMS[2]
situacion_mapa <- data.frame(OMS[3])

situacion_mapa1<-situacion_mapa %>%
  filter(Fecha==fecha_de_trabajo) %>%
  select(Region_OMS, Casos_acumulados) %>%
  group_by(Region_OMS) %>%
  summarise(Casos_acumulados = sum(Casos_acumulados)) %>%
  arrange(desc(Casos_acumulados))

#Creamos los factores de ordenamiento de la leyenda en el gráfico
regiones_fact <- c("América",
                   "Europa",
                   "Asia Sudoriental",
                   "Mediterráneo Oriental",
                   "África",
                   "Pacífico Occidental" )

#Creamos el factor para el color de cada región
colores_fact <- c("América" = "#8a1339",
                  "Europa" = "#164569",
                  "Asia Sudoriental"="#4d3565",
                  "Mediterráneo Oriental" ="#3f7433",
                  "África"="#7db76f",
                  "Pacífico Occidental" ="#a37c4b")



grafico2<-
  ggplot()+
  geom_bar(data = situacion_mapa1,
           aes(x= factor(Region_OMS,
                         levels=Region_OMS),
               y = Casos_acumulados,
               fill = Region_OMS),
           stat = "identity")+
  scale_fill_manual(values = colores_fact )+
  labs(title = expression(paste(bold("Gráfico 1.")," Distribución de casos acumulados de COVID-19 por SARS-CoV-2 por regiones de la OMS")),
       fill = "Regiones OMS",
       x = "",
       y = "")+
  theme_classic()+
  theme(text=element_text(size=14, family="Montserrat", hjust = 0.5 ),
        axis.text.x = element_text(size = 11,face = "bold"),
        axis.text.y = element_text(size = 11,face = "bold"),
        legend.position = "none")+
  scale_y_continuous(labels = scales::comma)+

  geom_text(data = situacion_mapa1 ,
            aes(x=Region_OMS,
                y = Casos_acumulados,
                label=prettyNum(Casos_acumulados,big.mark = ",")),
            vjust=-1, size=4, family="Montserrat", fontface='bold')


########letalidad####

grafico4 <-situacion %>%
  select(Date_reported,Cumulative_cases, Cumulative_deaths) %>%
  mutate(Date_reported=as.Date(Date_reported)) %>%
  filter(Date_reported>="2020-01-20") %>%
  group_by(Date_reported) %>%
  summarise(sum(Cumulative_cases), sum(Cumulative_deaths)) %>%
  rename("Cumulative_cases"=2, "Cumulative_deaths"=3) %>%
  mutate(letalidad=round((Cumulative_deaths/Cumulative_cases)*100,1)) %>%
  mutate(Date_reported=as.Date(Date_reported)) %>%

  ggplot()+
  geom_line(aes(x=Date_reported, y=letalidad, color="#C0322B", group=1), size=1.5)+
  labs(title = expression(paste(bold("Gráfico 2."), " Tasa de letalidad* global de COVID-19 por SARS-CoV-2")),
       x = "",
       y = "Letalidad")+
  theme_hc()+
  scale_y_continuous(limits = c(0,8))+

  scale_x_date(date_breaks = "1 month",
               limits = c(min(as.Date("2020-01-20")), max = max(fecha_de_trabajo)),
               #labels = ~ paste(stringi::stri_trans_totitle( months(., F)), year(.)),
               expand=c(0,0))+

  theme(text=element_text(size=14,
                          family="Montserrat",
                          hjust = 0.5),

        plot.title = element_text(hjust = 0.5),

        axis.text.x = element_text(angle=90,
                                   hjust = 1,
                                   size = 11,
                                   family = "Montserrat Bold"),

        axis.title.x = element_text(size = 12,
                                    family = "Montserrat Bold"),

        axis.text.y = element_text(size = 12, family = "Montserrat"),
        axis.title.y = element_text(size = 12,
                                    family = "Montserrat Bold"),
        legend.position = "none")


###############################################################################
####### Tabla Resumen por Región #####
# global_t2 <- situacion %>%
#   select(Fecha=1,
#          Region_OMS=WHO_region,
#          Casos_acumulados=Cumulative_cases,
#          Casos_nuevos=New_cases,
#          Defunciones_acumuladas=Cumulative_deaths,
#          Defunciones_nuevas=New_deaths) %>%
#   mutate(Fecha=as.Date(Fecha)) %>%
#   filter(Fecha==fecha_de_trabajo) %>%
#   group_by(Region_OMS) %>%
#   summarise(Casos_nuevos=sum(Casos_nuevos)) %>%
#   spread(Region_OMS, Casos_nuevos) %>%
#   rename("África"=1,
#          "América"=2,
#          "Mediterráneo Oriental"=3,
#          "Europa"=4,
#          "Asia Sudoriental"=6,
#          "Pacífico Occidental"=7,
#          "Other"=5) %>%
#   mutate(`Pacífico Occidental` = Other + `Pacífico Occidental`) %>%
#   select(-Other) %>%
#   mutate(total= rowSums(.[,1:6])) %>%
#   gather(Region, Casos_nuevos , 1:7) %>%
#   arrange(desc(Casos_nuevos)) %>%
#   mutate(pct_dist_casos = round(Casos_nuevos/max(Casos_nuevos)*100,1),
#          Casos_nuevos = prettyNum(Casos_nuevos, big.mark = ","),
#          pct_dist_casos = paste0(pct_dist_casos,"%")) %>%
#   rename("Región OMS"=1,
#          "Casos en las últimas 24 h" = 2,
#          "% de distribución de casos en las últimas 24 h"=3) %>%
#   mutate(`Región OMS` = recode(`Región OMS`, "total" = "Región OMS"))
###############################################################################

global_t2 <- situacion %>%
  select(Fecha=1,
         Region_OMS=WHO_region,
         Casos_acumulados=Cumulative_cases,
         Casos_nuevos=New_cases,
         Defunciones_acumuladas=Cumulative_deaths,
         Defunciones_nuevas=New_deaths) %>%
  mutate(Fecha=as.Date(Fecha)) %>%
  filter(Fecha==fecha_de_trabajo) %>%
  group_by(Region_OMS) %>%
  summarise(Casos_nuevos=sum(Casos_nuevos))

global_t2 <- data.frame(t(global_t2[-1])) %>%
  select("África"=1,
         "América"=2,
         "Mediterráneo Oriental"=3,
         "Europa"=4,
         "Asia Sudoriental"=6,
         "Pacífico Occidental"=7,
         "Other"=5) %>%
  mutate(`Pacífico Occidental` = Other + `Pacífico Occidental`) %>%
  select(-Other) %>%
  mutate(total= rowSums(.[,1:6]))


global_t2 <- data.frame(t(global_t2)) %>%
  #select("Casos reportados al día de hoy"=1) %>%
  arrange(desc(Casos_nuevos)) %>%
  mutate(pct_dist_casos = round(Casos_nuevos/max(Casos_nuevos)*100,1),
         Casos_nuevos = prettyNum(Casos_nuevos, big.mark = ","),
         pct_dist_casos = paste0(pct_dist_casos,"%"))

global_t2<-setDT(global_t2, keep.rownames = TRUE)[] %>%
  rename("Región OMS"=1,
         "Casos en las últimas 24 h" = 2,
         "% de distribución de casos en las últimas 24 h"=3)

regiones_global_t2 <- global_t2$`Región OMS`
regiones_global_t2[1] <- "Región OMS"

global_t2 <- as.data.frame(t(global_t2))
names(global_t2) <- global_t2[1,]
global_t2 <- global_t2[-c(1),]
global_t2 <- setDT(global_t2, keep.rownames = TRUE)[] %>%
  rename("Región OMS"=1) %>%
  select(-total)

###Dar orden a las columnas####
global_t2 <- subset(global_t2, select=c(regiones_global_t2))
flextable(global_t2)



global_t2_<- flextable(head(global_t2)) %>%
  align(align = "center", part = "all") %>%
  color(color = "white", part = "header") %>%
  color(j=1, color = "black", part = "all") %>%
  bg(j = "América", bg = "#8a1339", part = "header") %>%
  bg(j = "Europa", bg = "#164569", part = "header") %>%
  bg(j = "Asia Sudoriental", bg = "#4d3565", part = "header") %>%
  bg(j = "Mediterráneo Oriental", bg = "#3f7433", part = "header") %>%
  bg(j = "África", bg = "#7db76f", part = "header") %>%
  bg(j = "Pacífico Occidental", bg = "#a37c4b", part = "header") %>%
  bg(j = "Región OMS", bg = "white", part = "header") %>%

  border(part = "all", border = officer::fp_border(color = "black", style = "solid", width = 2.5) ) %>%
  bold(part ="all") %>%
  font(fontname = "Montserrat", part = "all") %>%
  fontsize(size = 11, part = "all")

#fit_to_width(width(global_t2_, width = 4), max_width = 6)




# set_table_properties(global_t2, width = 1, layout = "fixed" )
#
# bullet

# width(b, width = 1)
#
# grafico2
#
# grafico4

if(!dir.exists("productos")){
  dir.create("productos")
}else{
  print("Directorio existente")
}

pacman::p_load(tidyverse, officer, magrittr)
fecha_de_trabajo<-(fecha_de_trabajo)

Sys.setlocale("LC_TIME", "es_ES")
Fecha <-toupper(as.character(fecha_de_trabajo,format="%d %B %Y"))
Fecha_min <-as.character(fecha_de_trabajo,format="%A, %d de %B de %Y")

#Cargamos el documento de la OMS
# situacion <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv",
#                       encoding = "UTF-8") %>%
#   rename(Date_reported = 1)

bullet <- bullet(situacion, fecha_de_trabajo)

longitud<-length((situacion_mapa %>%
                    select(Fecha,Pais_ing ) %>%
                    filter(Fecha == fecha_de_trabajo))$Pais_ing)


tabla_a<-bullet[-8]
a<-data.frame(Reduce(rbind, tabla_a))


a <- flextable((a)) %>%
  delete_part (part  =  "header" ) %>%
  bg(i = c(1,3,4,6), bg = "#dda95f", part = "all") %>%
  color(i = c(1,3,4,6), color = "white", part = "all") %>%
  bold(part = "all") %>%
  align(align = "center", part = "all") %>%
  font(fontname = "Montserrat", part = "all") %>%
  border(border = officer::fp_border(color = "#dda95f", style = "solid", width = 1) ) %>%
  fontsize(size = 12, part = "all")



tabla_b<-bullet[-c(1,2,3,4,5,8)]
b<-data.frame(Reduce(rbind, tabla_b))
b <- flextable((b)) %>%
  delete_part (part  =  "header" ) %>%
  bg(i = 1, bg = "#dda95f", part = "all") %>%
  color(i = 1, color = "white", part = "all") %>%
  bold(part = "all") %>%
  align(align = "center", part = "all") %>%
  font(fontname = "Montserrat", part = "all") %>%
  border(border = officer::fp_border(color = "#dda95f", style = "solid", width = 1) ) %>%
  fontsize(size = 10, part = "all")



#Realizamos los cáluclos para los casos y defunciones acumulados, por semana, día y letalidad.
Casos_acumulados<-(situacion %>%
                     filter(Date_reported==fecha_de_trabajo) %>%
                     summarise(Cumulative_cases=sum(Cumulative_cases)))$Cumulative_cases
Casos_acumulados1<-prettyNum(Casos_acumulados,big.mark=",",scientific=FALSE)


Casos_nuevos<-(situacion %>%
                 filter(Date_reported==fecha_de_trabajo) %>%
                 summarise(New_cases=sum(New_cases)))$New_cases
Casos_nuevos<-prettyNum(Casos_nuevos,big.mark=",",scientific=FALSE)


Defunciones_acumuladas<-(situacion %>%
                           filter(Date_reported==fecha_de_trabajo) %>%
                           summarise(Cumulative_deaths=sum(Cumulative_deaths)))$Cumulative_deaths
Defunciones_acumuladas1<-prettyNum(Defunciones_acumuladas,big.mark=",",scientific=FALSE)


Defunciones_nuevas<-prettyNum((situacion %>%
                                 filter(Date_reported==fecha_de_trabajo) %>%
                                 summarise(New_deaths=sum(New_deaths)))$New_deaths,big.mark=",",scientific=FALSE )


letalidad<- paste0(round((Defunciones_acumuladas/Casos_acumulados)*100,1)," %")


ultimas_24<-situacion %>%
  mutate(Fecha=as.Date(Date_reported)) %>%
  filter(Fecha==fecha_de_trabajo) %>%
  summarise( Casos_nuevos= sum(New_cases),
             Defunciones_nuevas = sum(New_deaths))





texto_n <- fp_text(font.size = 11, font.family = "Montserrat Bold")
texto_ <- fp_text(font.size = 11, font.family = "Montserrat")
texto_subt <- fp_text(font.size = 6, font.family = "Montserrat")

parrafo <- fp_par(text.align = "justify")
parrafo2 <- fp_par(text.align = "center")
subtitulo <- fp_par(text.align = "left")

enunciado1<-fpar(ftext("Al día ", prop = texto_),
                 ftext(Fecha_min, prop = texto_n),
                 ftext(" a nivel mundial, se han reportado ", prop = texto_),
                 ftext(paste0(Casos_acumulados1," casos confirmados "), prop = texto_n),
                 ftext(paste0("(",Casos_nuevos," casos nuevos) y "), prop = texto_),
                 ftext(paste0(Defunciones_acumuladas1," defunciones "), prop = texto_n),
                 ftext(paste0("(",Defunciones_nuevas," nuevas defunciones)."), prop = texto_),fp_p = parrafo)


enunciado1_1 <- fpar(ftext("La OMS clasifica su distribución de casos por regiones.",prop = texto_), fp_p = parrafo)



enunciado1_2 <- fpar(ftext(paste("El total de casos y defunciones acumuladas a nivel mundial incluyen las embarcaciones internacionales. \nFuente: Panel de control de la enfermedad por coronavirus de la OMS (COVID-19). Ginebra: Organización Mundial de la Salud, 2022. Disponible en línea: https://covid19.who.int/ (última cita: [",format(fecha_de_trabajo,"%d/%B/%Y"),"]."),
                           prop = texto_subt), fp_p = parrafo )


enunciado1_3 <- fpar(ftext("Con un acumulado de ", prop = texto_),
                     ftext(763, prop = texto_n),
                     ftext(" casos y ", prop = texto_),
                     ftext(13, prop = texto_n),
                     ftext(" defunciones ocurridas en embarcaciones internacionales.", prop = texto_),
                     fp_p = parrafo )


enunciado1_4 <- fpar(ftext("En los últimos 14 días el número de casos nuevos representa el ", prop = texto_),
                     ftext(as.character(bullet[4]), prop = texto_n),
                     ftext(paste0("(",bullet[3],")."), prop = texto_),
                     fp_p = parrafo )



enunciado1_5 <- fpar(ftext("Hasta la fecha, se han reportado casos en ", prop = texto_),
                     ftext(paste(longitud, "países, territorios y áreas, "), prop = texto_n),
                     ftext("los casos se han notificado en las ", prop = texto_),
                     ftext("seis regiones de la OMS ", prop = texto_n),
                     ftext("América, Europa, Asia Sudoriental, Mediterráneo Oriental, Pacífico Occidental y África).", prop = texto_),
                     fp_p = parrafo )


enunciado1_6<-fpar(ftext("La letalidad global es de ", prop = texto_),
                 ftext(paste0(letalidad,"."), prop = texto_n),fp_p = parrafo)

enunciado1_7 <- fpar(ftext(paste("Fuente: Panel de control de la enfermedad por coronavirus de la OMS (COVID-19). Ginebra: Organización Mundial de la Salud, 2022. Disponible en línea: https://covid19.who.int/ (última cita: [",format(fecha_de_trabajo,"%d/%B/%Y"),"]."),
                           prop = texto_subt), fp_p = parrafo )


enunciado1_8 <- fpar(ftext(paste("*Tasa de letalidad: Personas que enfermaron y murieron por COVID-19 a nivel global."),
                           prop = texto_subt), fp_p = parrafo )





#estilo_flex<- fit_to_width(width(global_t2_, width = 8), max_width = 8, inc = 1)
estilo_flex <- width(global_t2_, width = 1)

ggsave("productos/grafico_barra.png", grafico2, width=17, height=10, units = "cm", dpi=900, scale = 1.8)
ggsave("productos/grafico_letalidad.png", grafico4, width=17, height=10, units = "cm", dpi=900, scale = 1.8)

#width(a, width = 3)

save_as_image(width(a, width = 3), path = "productos/tabla_a.png")
save_as_image(width(b, width = 1), path = "productos/tabla_b.png")

read_docx() %>%
  body_add_fpar(fpar(ftext(paste("INFORMACIÓN CORONAVIRUS",
                                 str_to_upper(format(as.Date(fecha_de_trabajo),
                                                     format = "%d de %B de %Y")) ),
                           prop = texto_n), fp_p = parrafo2)) %>%
  body_add_fpar(fpar(ftext(""))) %>%

  body_add_fpar(enunciado1) %>%
  body_add_fpar(fpar(ftext(""))) %>%

  body_add_fpar(enunciado1_1) %>%
  body_add_fpar(fpar(ftext(""))) %>%
  body_add_flextable(., value =estilo_flex , align = "center") %>%

  body_add_fpar(fpar(ftext(""))) %>%


  #body_add_gg( value = grafico2, style = "centered", width=15.59, height=9) %>%
  #body_add_gg( value = grafico2, style = "centered", width=6.5, height=4) %>%

  body_add_img(src = "productos/grafico_barra.png", style = "centered", width=6.5, height=4) %>%

  body_add_fpar(enunciado1_2) %>%

  body_add_fpar(fpar(ftext(""))) %>%

  body_add_fpar(enunciado1_3) %>%

  body_add_fpar(enunciado1_4) %>%

  body_add_fpar(enunciado1_5) %>%

  body_add_fpar(enunciado1_6) %>%

  #body_add_gg(., value = grafico4, style = "centered", width=15.59, height=9) %>%

  body_add_img(src = "productos/grafico_letalidad.png", style = "centered", width=6.5, height=4) %>%

  body_add_fpar(enunciado1_7) %>%


  body_add_fpar(fpar(ftext(""))) %>%
  body_add_fpar(enunciado1_8) %>%

  #body_add_flextable(., value = width(a, width = 3) , align = "center") %>%
  body_add_img(src = "productos/tabla_a.png", style = "centered", width=1.3, height=1) %>%

  body_add_fpar(fpar(ftext(""))) %>%
  body_add_img(src = "productos/tabla_b.png", style = "centered", width=1, height=1) %>%

  print(target = paste0("productos/EXT_BULLETS_COVID-19_",format(Sys.Date(), "%d-%m-%Y"),".docx"), overwrite = T)


