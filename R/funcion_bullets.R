#' funcion bullets
#' @export

reporte_de_situacion<-function(fecha_de_trabajo){ ####
 fecha_de_trabajo<-fecha_de_trabajo
  #Cargamos la base de datos de la OMS
  situacion <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv",
                        encoding = "UTF-8") %>%
    rename(Date_reported=1)

  #Cambiamos la configuración de las fechas y creamos un objeto para almacenar la fecha
  Sys.setlocale("LC_TIME", "es_ES")
  Fecha <-as.character(fecha_de_trabajo,format="%A, %d de %B de %Y")

   #Renombramos y recodificamos todo el DataFrame, renombramos variables
  #y recodificamos las observaciones de la región

  situacion_mapa<-situacion%>%
    select(Fecha=1,
           Pais_ing=3,
           Region_OMS=WHO_region,
           Casos_nuevos=New_cases,
           Casos_acumulados=Cumulative_cases,
           Defunciones_nuevas=New_deaths,
           Defunciones_acumuladas=Cumulative_deaths) %>%
    mutate(Fecha=as.Date(Fecha),
           Region_OMS = recode(Region_OMS,
                               "AFRO" = "África",
                               "AMRO" = "América",
                               "EMRO" = "Mediterraneo Oriental",
                               "EURO"="Europa",
                               "SEARO" = "Asia Sudoriental",
                               "WPRO" = "Pacífico Occidental",
                               "Other" = "Pacífico Occidental"))

  #Revisamos el número de países que han actualizado

  longitud<-length((situacion_mapa %>%
                      select(Fecha,Pais_ing ) %>%
                      filter(Fecha == fecha_de_trabajo))$Pais_ing)
  return(list(situacion,longitud,situacion_mapa))
}




graficas_tablas<-function(situacion,situacion_mapa){
  #Creamos los factores de ordenamiento de la leyenda en el gráfico
  regiones_fact <- c("América",
                     "Europa",
                     "Asia Sudoriental",
                     "Mediterraneo Oriental",
                     "África",
                     "Pacífico Occidental" )

  #Creamos el factor para el color de cada región
  colores_fact <- c("América" = "#8a1339",
                    "Europa" = "#164569",
                    "Asia Sudoriental"="#4d3565",
                    "Mediterraneo Oriental" ="#3f7433",
                    "África"="#7db76f",
                    "Pacífico Occidental" ="#a37c4b")


  #Recuperamos los valores globales para generar las tablas de los globales
  global_t1 <- situacion %>%
    select(Fecha=1,
           Region_OMS=WHO_region,
           Casos_acumulados=Cumulative_cases,
           Casos_nuevos=New_cases,
           Defunciones_acumuladas=Cumulative_deaths,
           Defunciones_nuevas=New_deaths) %>%
    mutate(Fecha=as.Date(Fecha),
           Region_OMS = recode(Region_OMS, "Other" = "SEARO")) %>%
    filter(Fecha==fecha_de_trabajo) %>%
    group_by(Region_OMS) %>%
    summarise(Casos_acumulados=sum(Casos_acumulados),
              Casos_nuevos=sum(Casos_nuevos),
              Defunciones_acumuladas=sum(Defunciones_acumuladas),
              Defunciones_nuevas=sum(Defunciones_nuevas))

  #Realizamos la primera transpuesta para cambiar los valores de las filas por columnas
  #Esto permite crear una nueva columna para sumar todos los totales del filtro
  global_t1 <- data.frame(t(global_t1[-1])) %>%
    select("AFRO"=1,
           "AMRO"=2,
           "EMRO"=3,
           "EURO"=4,
           "SEARO"=5,
           "WPRO"=6) %>%
    mutate(total= rowSums(.[,1:6]))

  #Transponemos nuevamente el dataframe y nos quedamos únicamente con casos acumulados
  #y defunciones acumuladas
  global_t1 <- data.frame(t(global_t1)) %>%
    select("Casos acumulados"=1,
           #"Casos reportados al día de hoy"=2,
           "Defunciones acumuladas"=3)
  #"Defunciones nuevas"=4)

  #Liberamos la primer columna como una columna manteniendo los valores verdaderos
  #Y recodificamos las observaciones, añadiendo a SEARO todos los casos de Other
  global_t1<-setDT(global_t1, keep.rownames = TRUE)[] %>%
    mutate(rn = recode(rn,
                       "total" = "Total",
                       "AFRO" = "África",
                       "AMRO" = "América",
                       "EMRO" = "Mediterraneo Oriental",
                       "EURO"="Europa",
                       "SEARO" = "Asia Sudoriental",
                       "WPRO" = "Pacífico Occidental")) %>%
    rename("Región OMS"=1)




  #######tabla2
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

  global_t2 <- flextable(head(global_t2)) %>%
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
    #theme_box ( global_t2 ) %>%
    border(part = "all", border = officer::fp_border(color = "black", style = "solid", width = 2.5) ) %>%
    bold(part ="all") %>%
    font(fontname = "Montserrat", part = "all")
  global_t2



  #Creamos el gráfico
  #Creamos el gráfico con los casos de cada día
  #Eliminamos los casos negativos
  grafico1 <- situacion_mapa %>%
    filter(Casos_nuevos > 0) %>%

    ggplot()+
    geom_bar(aes(x= Fecha,
                 y = Casos_nuevos,
                 fill = factor(Region_OMS,
                               levels = regiones_fact)),
             stat = "identity")+
    scale_fill_manual(values = colores_fact )+

    labs(title = paste("Casos diarios por regiones de la OMS",Fecha,"."),
         fill = "Regiones OMS",
         x = "Fecha de reporte",
         y = "Número de casos confirmados",
         caption = "*Los casos y defunciones incluyen las embarcaciones internacionales.")+
    theme_minimal()+

    scale_x_date(labels=scales::date_format("%d-%b-%y"),
                 date_breaks = "3 weeks")+

    theme(text=element_text(size=16,
                            family="Montserrat", ),
          axis.text.x = element_text(angle=90,
                                     hjust = 1,
                                     size = 11),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.title = element_text(face = "bold"))+

    scale_y_continuous(labels = scales::comma)

  ########Grafico2
  situacion_mapa1<-situacion_mapa %>%
    filter(Fecha==fecha_de_trabajo) %>%
    select(Region_OMS, Casos_acumulados) %>%
    group_by(Region_OMS) %>%
    summarise(Casos_acumulados = sum(Casos_acumulados)) %>%
    arrange(desc(Casos_acumulados))

  regiones_fact_1 <- situacion_mapa1$Region_OMS

  grafico2<-ggplot()+
    geom_bar(data = situacion_mapa1,
             aes(x= factor(Region_OMS, levels=regiones_fact_1),
                 y = Casos_acumulados,
                 fill = Region_OMS),
             stat = "identity")+
    scale_fill_manual(values = colores_fact )+
    labs(title = expression(paste(bold("Gráfico 1.")," Distribución de casos acumulados de COVID-19 por SARS-CoV-2 por regiones de la OMS.")),
         fill = "Regiones OMS",
         x = "",
         y = "")+
    theme_classic()+
    theme(text=element_text(size=14,
                            family="Montserrat", ),
          axis.text.x = element_text(size = 11,face = "bold"),
          axis.text.y = element_text(size = 11,face = "bold"),
          legend.position = "none")+
    scale_y_continuous(labels = scales::comma)+
    geom_text(data = situacion_mapa1 ,
              aes(x=Region_OMS,
                  y = Casos_acumulados, label=prettyNum(Casos_acumulados,big.mark = ",")),
              vjust=-1, size=5, family="Montserrat", fontface='bold')

  ############Gráfico 14 días
  situacion_14<-situacion_mapa %>%
    filter(Fecha >= (fecha_de_trabajo-13) & Fecha <= fecha_de_trabajo) %>%
    select(Region_OMS, Casos_nuevos) %>%
    group_by(Region_OMS) %>%
    summarise(Casos_nuevos = sum(Casos_nuevos)) %>%
    mutate(porcentaje=round((Casos_nuevos/sum(Casos_nuevos))*100,2)) %>%
    arrange(desc(Casos_nuevos))

  regiones_fact_2 <- situacion_14$Region_OMS

  grafico3<-ggplot()+
    geom_bar(data = situacion_14,
             aes(x= factor(Region_OMS, levels=regiones_fact_2),
                 y = Casos_nuevos,
                 fill = Region_OMS),
             stat = "identity")+
    scale_fill_manual(values = colores_fact )+
    labs(fill = "Regiones OMS",
         title = "",
         x = "",
         y = "")+
    theme_classic()+
    theme(text=element_text(size=11,
                            family="Montserrat", ),
          axis.text.x = element_text(size = 11,face = "bold"),
          axis.text.y = element_text(size = 11,face = "bold"),
          legend.position = "none")+
    scale_y_continuous(labels = scales::comma, limits = c(0,2500000))+
    geom_text(data = situacion_14 ,
              aes(x=Region_OMS,
                  y = Casos_nuevos, label=paste(prettyNum(Casos_nuevos,big.mark = ","), "")),
              vjust=-1.85, size=5, family="Montserrat", fontface='bold')+

    geom_text(data = situacion_14 ,
              aes(x=Region_OMS,
                  y = Casos_nuevos, label=paste(porcentaje,"%")),
              vjust=-.3, size=5, family="Montserrat", fontface='bold', color="#8a1339")

  ############Gráfico acumulado - % días

  grafico5<-ggplot()+
    geom_bar(data = situacion_mapa1,
             aes(x= factor(Region_OMS, levels=regiones_fact_1),
                 y = Casos_acumulados,
                 fill = Region_OMS),
             stat = "identity")+
    scale_fill_manual(values = colores_fact )+
    labs(title = expression(paste(bold("Gráfico 1.")," Distribución de casos acumulados de COVID-19 por SARS-CoV-2 por regiones de la OMS.")),
         fill = "Regiones OMS",
         x = "",
         y = "")+
    theme_classic()+
    theme(text=element_text(size=14,
                            family="Montserrat", ),
          axis.text.x = element_text(size = 11,face = "bold"),
          axis.text.y = element_text(size = 11,face = "bold"),
          legend.position = "none")+
    scale_y_continuous(labels = scales::comma, limits = c(0,80000000))+
    geom_text(data = situacion_mapa1 ,
              aes(x=Region_OMS,
                  y = Casos_acumulados, label=prettyNum(Casos_acumulados,big.mark = ",")),
              vjust=-1.85, size=5, family="Montserrat", fontface='bold')+

    geom_text(data = situacion_mapa1 ,
              aes(x=Region_OMS,
                  y = Casos_acumulados, label=paste(round((Casos_acumulados/sum(Casos_acumulados))*100,1),"%")),
              vjust=-.3, size=5, family="Montserrat", fontface='bold', color="#8a1339")


  ########letalidad####

  letalidad<-situacion %>%
    select(Date_reported,Cumulative_cases, Cumulative_deaths) %>%
    mutate(Date_reported=as.Date(Date_reported)) %>%
    filter(Date_reported>="2020-01-20") %>%
    group_by(Date_reported) %>%
    summarise(sum(Cumulative_cases), sum(Cumulative_deaths)) %>%
    rename("Cumulative_cases"=2, "Cumulative_deaths"=3) %>%
    mutate(letalidad=round((Cumulative_deaths/Cumulative_cases)*100,1)) %>%
    mutate(Date_reported=as.Date(Date_reported))


  grafico4<-letalidad %>%
    ggplot()+
    geom_line(aes(x=Date_reported, y=letalidad, color="#C0322B", group=1), size=1.5)+
    labs(title = expression(paste(bold("Gráfico 2."), " Tasa de letalidad* global de COVID-19 por SARS-CoV-2.")),
         x = "",
         y = "Letalidad")+
    theme_hc()+
    scale_y_continuous(limits = c(0,8))+
    scale_x_date(date_breaks = "3 weeks",
                 limits = c(min(as.Date("2020-01-20")), max = max(fecha_de_trabajo)),
                 expand=c(0,0))+
    theme(text=element_text(size=16,
                            family="Montserrat"),
          axis.text.x = element_text(angle=90,
                                     hjust = 1,
                                     size = 11,
                                     face = "bold"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none")


  #####tablas####
  bullet <- bullet(situacion, fecha_de_trabajo)

  bullets<-bullet[["enunciado"]]

  tabla_a<-bullet[-8]
  a<-data.frame(Reduce(rbind, tabla_a))
  flextable(a)

  a <- flextable((a)) %>%
    delete_part (part  =  "header" ) %>%
    bg(i = c(1,3,4,6), bg = "#dda95f", part = "all") %>%
    color(i = c(1,3,4,6), color = "white", part = "all") %>%
    bold(part = "all") %>%
    align(align = "center", part = "all") %>%
    font(fontname = "Montserrat", part = "all") %>%
    border(border = officer::fp_border(color = "#dda95f", style = "solid", width = 1) ) %>%
    fontsize(size = 12, part = "all")
  a

  tabla_b<-bullet[-c(1,2,3,4,5,8)]

  b<-data.frame(Reduce(rbind, tabla_b))
  flextable(b)

  b <- flextable((b)) %>%
    delete_part (part  =  "header" ) %>%
    bg(i = 1, bg = "#dda95f", part = "all") %>%
    color(i = 1, color = "white", part = "all") %>%
    bold(part = "all") %>%
    align(align = "center", part = "all") %>%
    font(fontname = "Montserrat", part = "all") %>%
    border(border = officer::fp_border(color = "#dda95f", style = "solid", width = 1) ) %>%
    fontsize(size = 12, part = "all")
  b


  ####llamar objetos

  bullets <- bullets
  global_t1 <- flextable::flextable(global_t1)
  print(grafico2)
  print(grafico3)
  print(grafico4)
  print(grafico5)
  print(grafico1)
  global_t2<-(global_t2)
  a<-(a)
  b<-(b)

  return(grafico1, grafico2, grafico3, grafico4, grafico5, bullets, global_t1,
         global_t2,a, b)

}
