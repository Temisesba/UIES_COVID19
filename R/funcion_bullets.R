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
                               "EMRO" = "Mediterráneo Oriental",
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




graficas_tablas<-function(fecha_de_trabajo, situacion,situacion_mapa){
  pacman::p_load(ggthemes, tidyverse, data.table, extrafont, showtext, lubridate,
          magrittr, flextable)
  Sys.setlocale("LC_TIME", "es_ES")
  Fecha <-as.character(fecha_de_trabajo,format="%A, %d de %B de %Y")

  situacion<-as.data.frame(situacion)
  situacion_mapa<-as.data.frame(situacion_mapa)
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
                       "EMRO" = "Mediterráneo Oriental",
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

  #Creamos el gráfico1####
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

    labs(title = paste0("Casos diarios por regiones de la OMS ",Fecha),
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

  ########Grafico2####
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

  ############Gráfico 14 días####
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
    scale_y_continuous(labels = scales::comma, limits = c(0, max(situacion_14$Casos_nuevos)+500000))+
    geom_text(data = situacion_14 ,
              aes(x=Region_OMS,
                  y = Casos_nuevos, label=paste(prettyNum(Casos_nuevos,big.mark = ","), "")),
              vjust=-1.85, size=5, family="Montserrat", fontface='bold')+

    geom_text(data = situacion_14 ,
              aes(x=Region_OMS,
                  y = Casos_nuevos, label=paste(porcentaje,"%")),
              vjust=-.3, size=5, family="Montserrat", fontface='bold', color="#8a1339")

  ############Gráfico acumulado - % días####

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
    scale_y_continuous(labels = scales::comma, limits = c(0,max(situacion_mapa1$Casos_acumulados)+5000000))+
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

  bullets<-bullet[[8]]

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


###llamar objetos

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

  #####ppt y word#####
  generar_pptx_V2(bullets, grafico1, global_t1, grafico2, global_t2, grafico3, grafico4, a, b, fecha_de_trabajo, grafico5)

  bullet_word_V2(grafico1, situacion, fecha_de_trabajo)

  return(list(global_t1, global_t2, grafico1, grafico2, grafico3, grafico4,
               grafico5,bullets, a, b))


}

#bullets######
bullet<-function(situacion, fecha_de_trabajo){
  fecha_de_trabajo<-(fecha_de_trabajo)

  Sys.setlocale("LC_TIME", "es_ES")
  Fecha <-as.character(fecha_de_trabajo,format="%A, %d de %B de %Y")

  #Cargamos el documento de la OMS
  # situacion <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv",
  #                       encoding = "UTF-8")
  #

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

  ########para tablas
  ####nuevo
  Casos_14d<-(situacion %>%
                filter(Date_reported >= (fecha_de_trabajo-13) & Date_reported <= fecha_de_trabajo) %>%
                summarise(New_cases=sum(New_cases)))$New_cases
  Casos14d<-prettyNum(Casos_14d,big.mark=",",scientific=FALSE)

  porcentaje_14<-paste0(round((Casos_14d/Casos_acumulados)*100,0),"%")

  a<-"Total de casos confirmados a nivel mundial"
  b<-"Casos confirmados en los últimos 14 días"
  c<-"Tasa de letalidad global"

  ##############################################################################
  #Escribimos los tres enunciados que devolverá la función en forma de lista

  enunciado1<-print(paste0("Al día ",Fecha," a nivel mundial, se han reportado ",
                           Casos_acumulados1," casos confirmados (",Casos_nuevos,
                           " casos nuevos) y ",Defunciones_acumuladas1,
                           " defunciones (",Defunciones_nuevas,
                           " nuevas defunciones).\n"))

  enunciado3<-print(paste0("\nLa letalidad global es de ",letalidad,"."))


  enunciado2<-print(paste("\nEn las últimas 24 horas se reportaron",
                          prettyNum(ultimas_24$Casos_nuevos, big.mark = ","),
                          "casos y",
                          prettyNum(ultimas_24$Defunciones_nuevas, big.mark = ","),
                          "defunciones a nivel global.\n"))

  enunciado<-paste(enunciado1, enunciado2, enunciado3)

  return(list(Casos_acumulados1=Casos_acumulados1,
              a=a,
              Casos14d=Casos14d,
              porcentaje_14=porcentaje_14,
              b=b,
              letalidad=letalidad,
              c=c,
              enunciado=enunciado))
}



####función ppt####
generar_pptx_V2 <- function(bullets, grafico1, global_t1, grafico2, global_t2, grafico3, grafico4, a, b,fecha_de_trabajo, grafico5){
  if(!dir.exists("productos")){
    dir.create("productos")
  }else{
    print("Directorio existente")
  }

  if(!dir.exists("bin")){
    dir.create("bin")
  }else{
    print("Directorio existente")
  }

  fecha_de_trabajo<-fecha_de_trabajo

  pacman::p_load(flextable, officer, magrittr, tidyverse)

  #DISTRIBUCIÓN Y TIPO DE TITULO
  Titulo <- ph_location_type(type = "title")
  subTitulo <- ph_location_type(type = "subTitle")

  if( !file.exists("bin/Plantilla.pptx") ){

    download.file("https://github.com/Temisesba/UIES_COVID19/raw/main/bin/Plantilla.pptx", "bin/Plantilla.pptx", mode = "wb")

  }



  my_pres <- read_pptx("bin/Plantilla.pptx")




  layout_summary(my_pres)

  # Ver marcador de posición
  layout_properties(my_pres, layout = "Titulo", master = "Tema de Office")


  SE<-lubridate::isoweek(fecha_de_trabajo)
  aaaa<- lubridate::isoyear(fecha_de_trabajo)

  #fuente <- fp_text(font.size = 24, font.family = "Montserrat")
  enunciado <- block_list(fpar(ftext(bullets,
                                     fp_text(font.size = 24,
                                             font.family = "Montserrat"))))

  # fecha_gen <- block_list(fpar(ftext(paste("Documento generado el día", Fecha),
  #                                    fp_text(font.size = 24,
  #                                            font.family = "Montserrat"))))


  ########################## 1. PORTADA
  my_pres <- add_slide(my_pres,
                       layout = "Titulo",
                       master = "Tema de Office") %>%
    ph_with( "REPORTE DE SITUACIÓN INTERNACIONAL", location= Titulo) %>%
    #Establecemos el título
    ph_with( paste("SEMANA EPIDEMIOLÓGICA" ,SE, "DE", aaaa ),
             location = subTitulo) %>%

    #Establecemos el subtitulo
    # ph_with( paste("Documento generado" ,SE, "DE", aaaa ),
    #          location = ph_location_type(ph_label = "body")) %>%
    #
    # ph_with(value = "aaa",
    #         ph_location_label(ph_label = "CuadroTexto 13")) %>%

    ########################### 2. BULLETS

  add_slide(layout = "Tablas", master = "Tema de Office") %>%
    ph_with(value = enunciado,
            ph_location_label(ph_label = "CuadroTexto 3")) %>%

    ph_with(value = global_t1, cwidth = 100, cheight = 500,
            location = ph_location_label(ph_label = "CuadroTexto 6")) %>%

    ########################### 3. GRAFICO
  add_slide(layout = "Grafico1", master = "Tema de Office") %>%
    ph_with(value = grafico1,
            ph_location_label(ph_label = "CuadroTexto 13")) %>%


    ########################### 4. GRAFICO
  add_slide(layout = "Grafico1", master = "Tema de Office") %>%
    ph_with(value = grafico2,
            ph_location_label(ph_label = "CuadroTexto 13")) %>%


    ########################### 5. OTRAS TABLA

  add_slide(layout = "Tablas_", master = "Tema de Office") %>%
    ph_with(value = global_t2, cwidth = 9, cheight = 10,
            location = ph_location_label(ph_label = "CuadroTexto 7")) %>%

    ph_with(value = a, cwidth = 10, cheight = 10,
            location = ph_location_label(ph_label = "CuadroTexto 6")) %>%

    ph_with(value = b, cwidth = 10, cheight = 10,
            location = ph_location_label(ph_label = "CuadroTexto 11")) %>%

    ########################### 6. GRAFICO
  add_slide(layout = "Grafico1", master = "Tema de Office") %>%
    ph_with(value = grafico3,
            ph_location_label(ph_label = "CuadroTexto 13")) %>%


    #   ########################### 7. GRAFICO
  add_slide(layout = "Grafico1", master = "Tema de Office") %>%
    ph_with(value = grafico5,
            ph_location_label(ph_label = "CuadroTexto 13")) %>%


    ########################### 8. GRAFICO
  add_slide(layout = "Grafico1", master = "Tema de Office") %>%
    ph_with(value = grafico4,
            ph_location_label(ph_label = "CuadroTexto 13")) %>%




    ####ultimo paso#####


    print(my_pres, target = "productos/Situacion_Internacional.pptx") %>%
    invisible()

}


##generar WORD#####
bullet_word_V2<-function(grafico1, situacion, fecha_de_trabajo){

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
  situacion <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv",
                        encoding = "UTF-8") %>%
    rename(Date_reported = 1)


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


  #####Creamos documento en WORD

  texto_n <- fp_text(font.size = 11, font.family = "Montserrat Bold")
  texto_ <- fp_text(font.size = 11, font.family = "Montserrat")
  parrafo <- fp_par(text.align = "justify")
  parrafo2 <- fp_par(text.align = "center")

  enunciado1<-fpar(ftext("Al día ", prop = texto_),
                   ftext(Fecha_min, prop = texto_n),
                   ftext(" a nivel mundial, se han reportado ", prop = texto_),
                   ftext(paste0(Casos_acumulados1," casos confirmados "), prop = texto_n),
                   ftext(paste0("(",Casos_nuevos," casos nuevos) y "), prop = texto_),
                   ftext(paste0(Defunciones_acumuladas1," defunciones "), prop = texto_n),
                   ftext(paste0("(",Defunciones_nuevas," nuevas defunciones)."), prop = texto_),fp_p = parrafo)

  enunciado2<-fpar(ftext(paste("En las últimas 24 horas se reportaron",
                               prettyNum(ultimas_24$Casos_nuevos, big.mark = ","),
                               "casos y",
                               prettyNum(ultimas_24$Defunciones_nuevas, big.mark = ","),
                               "defunciones a nivel global."), prop = texto_),fp_p = parrafo)

  enunciado3<-fpar(ftext("La letalidad global es de ", prop = texto_),
                   ftext(paste0(letalidad,"."), prop = texto_n),fp_p = parrafo)

  grafica<-ggsave("productos/grafico1.png", grafico1, width=17, height=10, units = "cm", dpi=900, scale = 1.8)

  My_doc <- read_docx()
  My_doc %>%
    body_add_fpar(fpar(ftext("SECCIÓN INTERNACIONAL", prop = texto_n), fp_p = parrafo2)) %>%
    body_add_fpar(fpar(ftext(""))) %>%
    body_add_fpar(fpar(ftext("COVID-19", prop = texto_n), fp_p = parrafo2)) %>%
    body_add_fpar(fpar(ftext(""))) %>%
    body_add_fpar(fpar(ftext(Fecha, prop = texto_n), fp_p = parrafo2)) %>%
    body_add_fpar(fpar(ftext(""))) %>%
    body_add_fpar(enunciado1) %>%
    body_add_fpar(fpar(ftext("")))%>%
    body_add_fpar(enunciado2) %>%
    body_add_fpar(fpar(ftext(""))) %>%
    body_add_fpar(enunciado3) %>%
    body_add_img(src = "productos/grafico1.png", style = "centered", width=6.5, height=4) %>%
    print(target = paste0("productos/BULLETS_COVID-19_",format(Sys.Date(), "%d-%m-%Y"),".docx"))

}


quantil_color <-function(x){
  ############### CUANTILES DE COLOR #######
  quantileNum <- 7
  probs <- seq(0, 1, length.out = quantileNum )
  bins <- quantile(x, probs, na.rm = TRUE, names = FALSE)

  while (length(unique(bins)) != length(bins)) {

    quantileNum <- quantileNum - 1
    probs <- seq(0, 1, length.out = quantileNum + 1)
    bins <- quantile(x, probs, na.rm = TRUE, names = FALSE)
  }
  return(bins)
}

