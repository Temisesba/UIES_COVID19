#' bullets COVID-19
#' @export

indicadores<-function(fecha_de_trabajo){

  ########### Funcion para establecer rango de colores al diseño del Flextable

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


  #Paquetes requeridos adicionales
  #devtools::install_github("rensa/ggflags")
  #devtools::install_github("hadley/emo")
  if(!require("devtools")) webshot::install_phantomjs()

  if(!require("devtools")) install.packages("devtools")
  if(!require("pacman")) install.packages("pacman")
  if(!require("ggflags")) devtools::install_github("jimjam-slam/ggflags")
  if(!require("emo")) devtools::install_github("hadley/emo")


  #Cargamos la fecha y corregimos el formato de tiempo a español
  fecha_de_trabajo<-fecha_de_trabajo
  Sys.setlocale("LC_TIME", "es_ES")


  #Cargamos los paquetes a utilizar
  pacman::p_load(tidyverse, flextable, ftExtra, webshot, emo, countrycode, officer,
                 magrittr, Rcpp)

  #Indetificamos el directorio dónde se crearán los productos
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


  if(!dir.exists("bin/banderas")){
    dir.create("bin/banderas")
  }else{
    print("Directorio existente")
  }

  #Cargamos la base de datos de la OMS y los paises en español y los unimos
  situacion<-  merge(read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv",
                              encoding = "UTF-8") %>% rename( Date_reported = 1, Clave=2),
                     read.csv("https://raw.githubusercontent.com/Temisesba/P-blico/main/sabana_poblacion_bd.csv",
                              encoding = "UTF-8") %>%
                       select(Pais_esp,Clave=Codigo_pais), by = "Clave")



  #Renombramos y recodificamos todo el DataFrame, renombramos variables

  siete_dias<-situacion%>%
    select(Fecha="Date_reported",
           Pais="Pais_esp",
           Casos_nuevos=New_cases,
           Defunciones_nuevas=New_deaths,
           Country) %>%
    mutate(Fecha=as.Date(Fecha)) %>%
    mutate(Country= gsub(" ","_", Country)) %>%
    mutate(Country= recode(Country, "Other"="smile",
                           "Bolivia_(Plurinational_State_of)"="Bolivia",
                           "Micronesia_(Federated_States_of)"="Micronesia",
                           "The_United_Kingdom"="United_Kingdom",
                           "Iran_(Islamic_Republic_of)"="Iran",
                           "Republic_of_Moldova"="Moldova",
                           "Russian_Federation"="Russia",
                           "United_Republic_of_Tanzania"="Tanzania",
                           "United_States_of_America"="United_States",
                           "Venezuela_(Bolivarian_Republic_of)"="Venezuela",
                           "Viet_Nam"="Vietnam",
                           "Kosovo[1]"="Kosovo",
                           "Timor-Leste"="Timor_Leste", "United_States_Virgin_Islands"="us_virgin_islands",
                           "Holy_See"="vatican_city", "Saint_Vincent_and_the_Grenadines"="st_vincent_grenadines",
                           "Syrian_Arab_Republic"="syria", "Eswatini"="swaziland",
                           "Curaçao"="curacao","occupied_Palestinian_territory,_including_east_Jerusalem"="palestinian_territories",
                           "Réunion"="reunion","Saint_Helena"="st_helena",
                           "Northern_Mariana_Islands_(Commonwealth_of_the)"="Northern_Mariana_Islands", "Myanmar"="myanmar",
                           "Cook_Islands"="Cook_Islands","Cabo_Verde"="cape_verde",
                           "Saint_Kitts_and_Nevis"="St_Kitts_and_Nevis", "Democratic_People's_Republic_of_Korea"="north_korea",
                           "Republic_of_Korea"="South_Korea","Lao_People's_Democratic_Republic"="Laos",
                           "North_Macedonia"="macedonia", "Saint_Pierre_and_Miquelon"="st_pierre_miquelon",
                           "Saint_Lucia"="st_lucia","Saint_Martin"="St_Martin",
                           "Saint_Barthélemy"="St_Barthelemy", "Brunei_Darussalam"="Brunei",
                           "Democratic_Republic_of_the_Congo"="congo_kinshasa", "Congo"="congo_brazzaville",
                           "Central_African_Republic"="Central_African_Republic", "Côte_d’Ivoire"="Cote_d_Ivoire",
                           "Falkland_Islands_(Malvinas)"="falkland_islands","Guinea-Bissau"="Guinea_Bissau",
                           "Sint_Eustatius"="smile", "Saba"="smile", "Türkiye" =  "Turkey")) %>%
    #filter(Country == "South_Korea") %>%
    mutate(bandera = map_chr(Country, emo::ji)) %>%
    mutate(code = tolower(countrycode(Country, origin = 'country.name', destination = 'iso2c'))) %>%

    #Surcorea es uno de esos países que tienen problema con su código de país
    #Tenemos que volver a recodificarlo quizá sea mejor aplicar which para no usar el mutate
    #o un ifelse (pendiente)
    #Turquia es otro de los países que requieren que se actualice su información
    mutate(Country = recode(Country,
                            "South_Korea" =  "The Republic of Korea",
                            "Türkiye" =  "Turkey")) %>%
    mutate(code = tolower(countrycode(Country, origin = 'country.name', destination = 'iso2c')))


  # https://github.com/hadley/emo/tree/master/data
  # a=data.frame(ji_name[3727:4239])
  # a$paises=rownames(a)

  #agregar codigo de dos digitos de país (se colocó arriba en el mutate)
  #siete_dias$code<-tolower( countrycode(siete_dias$Country,origin = 'country.name', destination = 'iso2c') )

  ##ciclofor para todos####

  # banderas<-unique(siete_dias$code)
  # band<-banderas[-(1)]
  # band
  #
  #
  # for (i in 1:length(band)) {
  #   download.file(paste0("https://flagcdn.com/w320/",band[i],".png"), paste0("../banderas/",band[i],".png"), mode = "wb")
  # }

  ######Filtramos últimas dos semanas
  Ultima_semana<-siete_dias %>%
    filter( Fecha>fecha_de_trabajo-7) %>%
    group_by(bandera, Pais, code) %>%
    summarise(Casos_nuevos_1=sum(Casos_nuevos),
              Defunciones_nuevas_1=sum(Defunciones_nuevas))

  Semana_previa<-siete_dias %>%
    filter(Fecha >= (fecha_de_trabajo-13) &
             Fecha <= fecha_de_trabajo-7) %>%
    group_by(Pais) %>%
    summarise(Casos_nuevos_2=sum(Casos_nuevos),
              Defunciones_nuevas_2=sum(Defunciones_nuevas))

  #####Unimos ambas semanas y hacemos los cálculos necesarios para obtener los porcentajes requeridos
  completo<-merge(Ultima_semana,Semana_previa, by = "Pais") %>%
    mutate(porcentaje_casos = paste0(round((Casos_nuevos_1/sum(Casos_nuevos_1))*100,1),"%")) %>%
    mutate(porcentaje_cambio_casos = paste0(round(((Casos_nuevos_1-Casos_nuevos_2)/Casos_nuevos_2)*100,1),"%")) %>%

    #Cambio numerico
    #mutate(porcentaje_cambio = round(((Casos_nuevos_1-Casos_nuevos_2)/Casos_nuevos_2)*100,1))  %>%

    mutate(porcentaje_defunciones = paste0(round((Defunciones_nuevas_1/sum(Defunciones_nuevas_1))*100,1),"%")) %>%
    mutate(porcentaje_cambio_defunciones = paste0(round(((Defunciones_nuevas_1-Defunciones_nuevas_2)/Defunciones_nuevas_2)*100,1),"%"))

  ######Dividimos en dos dataframe uno de casos y uno de defunciones cada uno se ordena en orden descendente por número
  #de casos o de defunciones nuevas, se agrega el número de posición
  casos<-completo %>%
    arrange(desc(Casos_nuevos_1))
  casos$Posicion<-1:(length(unique(casos$Pais)))

  defunciones<-completo %>%
    arrange(desc(Defunciones_nuevas_1))
  defunciones$Posicion<-1:(length(unique(casos$Pais)))

  ####### Se convierten casos y defunciones a formato con comas divisoras y se corta a los primeros 15 datos
  casos <- casos %>%
    select(Posicion, bandera, Pais, Casos_nuevos_1, porcentaje_casos, porcentaje_cambio_casos, code) %>%
    mutate(Casos_nuevos_1 = prettyNum(Casos_nuevos_1, big.mark = ","),)
  casos<-head(casos, 15)

  defunciones <- defunciones %>%
    select(Posicion, bandera, Pais, Defunciones_nuevas_1, porcentaje_defunciones, porcentaje_cambio_defunciones, code) %>%
    mutate(Defunciones_nuevas_1 = prettyNum(Defunciones_nuevas_1, big.mark = ","),)
  defunciones<-head(defunciones, 15)

  #Vector único con las banderas de los 15 países con casos
  banderasc<-unique(casos$code)

  #Removemos los NA si es que hubiera
  banderasc<-banderasc[!is.na(banderasc) ]


  #####Se cargan imagenes de arriba y abajo####
  if( (!file.exists("bin/banderas/arriba.png") | !file.exists("bin/banderas/abajo.png") ) ){

    download.file("https://github.com/Temisesba/P-blico/raw/main/arriba.png", "bin/banderas/arriba.png", mode = "wb")
    download.file("https://github.com/Temisesba/P-blico/raw/main/abajo.png", "bin/banderas/abajo.png", mode = "wb")

  }


  for (i in 1:length(banderasc)) {

    if( !file.exists(paste0("bin/banderas/",banderasc[i], ".png")) ){
      download.file(paste0("https://flagcdn.com/w320/",banderasc[i],".png"),
                    paste0("bin/banderas/",banderasc[i],".png"), mode = 'wb')



    }

  }



  casos$code <- paste0("bin/banderas/",casos$code,".png")

  #Vector único con las banderas de los 15 países con defunciones
  banderasd<-unique(defunciones$code)

  for (i in 1:length(banderasd)) {

    if( !file.exists(paste0("bin/banderas/",banderasd[i], ".png")) ){
      download.file(paste0("https://flagcdn.com/w320/",banderasd[i],".png"),
                    paste0("bin/banderas/",banderasd[i],".png"), mode = "wb")
    }

  }

  defunciones$code <- paste0("bin/banderas/",defunciones$code,".png")

  #fs::dir_tree("bin/banderas/")


  ##############Se realiza el date frame con los casos nuevos de cada país por fecha
  ####con la función nest se crean una lista de dataframes de fecha y casos de cada país.
  curvas<-situacion %>%
    select(Date_reported,
           Pais=Pais_esp,
           Casos_nuevos="New_cases",
           Defunciones_nuevas="New_deaths") %>%
    group_by(Pais) %>%
    nest()

  ##########Se agrega el formato para el gráfico tanto para casos(m) como defunciones(n) y se junta con el resto de variables
  #####EJEMPLO DE GRÁFICO EN GRANDE#######
  # ggplot()+
  #   geom_bar(data = quiza, aes(x=Date_reported, y=Casos_nuevos), fill="#a37c4b", stat = "identity")+
  #   geom_vline(aes(xintercept=as.character(quiza[which.max(quiza$Casos),2]), color="max"), size=1.5, linetype = "dashed")+
  #   theme(axis.text.x=element_blank(),
  #         axis.text.y=element_blank(),
  #         axis.title.x=element_blank(),
  #         axis.title.y=element_blank())

  m<-curvas %>%
    mutate(plot = map2(data, Pais, ~ ggplot(data = .x) +
                         geom_bar(aes(x=Date_reported, y=Casos_nuevos), fill="#a37c4b", stat = "identity")+
                         geom_vline(aes(xintercept=as.character(.x[which.max(.x$Casos_nuevos),1])),
                                    size=0.7, linetype = "solid", color="red")+
                         theme_minimal()+
                         theme(axis.text.x=element_blank(),
                               axis.text.y=element_blank(),
                               axis.title.x=element_blank(),
                               axis.title.y=element_blank())))

  n<-curvas %>%
    mutate(plot = map2(data,Pais, ~ ggplot(data = .x) +
                         geom_bar(aes(x=Date_reported, y=Defunciones_nuevas), fill="#9E2B49", stat = "identity")+
                         geom_vline(aes(xintercept=as.character(.x[which.max(.x$Defunciones_nuevas),1])),
                                    size=0.7, linetype = "solid", color="red")+
                         theme_minimal()+
                         theme(axis.text.x=element_blank(),
                               axis.text.y=element_blank(),
                               axis.title.x=element_blank(),
                               axis.title.y=element_blank())))

  #Se une el el dataframe con los gráficos por país junto con los data frame de casos y el de defunciones que se hizo al###
  #inicio donde viene el resto de la información para la tabla

  m <- left_join(casos, m, by=c("Pais")) %>%
    select(Posicion=1, bandera=2, Pais=3, data=8, plot=9, Casos_nuevos_1=4, porcentaje_casos=5, porcentaje_cambio_casos=6, code=7, cambio = )

  n <- left_join(defunciones, n, by=c("Pais")) %>%
    select(Posicion=1, bandera=2, Pais= 3, data=8, plot=9, Defunciones_nuevas_1=4, porcentaje_defunciones=5, porcentaje_cambio_defunciones=6, code=7)



  #Obtenemos los vectores con los casos de la tabla
  porcentaje_def <-(n %>% mutate(cambio = as.numeric(gsub("%","",porcentaje_defunciones))) %>%
                      select(cambio))$cambio

  porcentaje_casos <-(m %>% mutate(cambio = as.numeric(gsub("%","",porcentaje_casos))) %>%
                        select(cambio))$cambio


  #Se necesita enviar estos dos elementos de tipo vector al entorno global
  # debido a que el parámetro bg necesita alimentarse desde el entorno global

  .GlobalEnv$quantil_casos<- quantil_color(porcentaje_casos)
  .GlobalEnv$quantil_defunciones<- quantil_color(porcentaje_def)


  tabla.casos<- m %>%
    mutate(cambio = as.numeric(gsub("%","",porcentaje_casos))) %>%
    select(-data) %>%
    flextable(col_keys = c("Posicion", "bandera",
                           "Pais", "plot",
                           "Casos_nuevos_1", "porcentaje_casos",
                           "porcentaje_cambio_casos",
                           "code")) %>%
    mk_par(
      j = "plot",
      value = as_paragraph(gg_chunk(value = plot, height = 0.7, width = 2))) %>%
    bg(i = 1, bg = "#2B614D", part = "header") %>%
    color(i = 1, color = "white", part = "header") %>%
    bold(part = "all") %>%
    align(align = "center", part = "all") %>%
    font(fontname = "Montserrat", part = "all") %>%
    fontsize(size = 20, part = "all") %>%
    merge_at(i = 1, j=2:3, part = "header") %>%
    fontsize(j=2, size = 40, part = "body") %>%  #tamaño de bandera cuando es emoji
    # compose(j=2, value = as_paragraph(as_image(src = code, width = .50, height = .35),
    #                                   " ", as_chunk(m$Pais))) %>%  #para que quede junto al nombre

    compose(j=2, value = as_paragraph(as_chunk(""),
                                      as_image(src = code, width = .50, height = .35))) %>% #bandera rectangular en su propia columna
    hline(part="all", border = officer::fp_border(color = "#2B614D", style = "solid", width = 3)) %>%
    color(i = ~ `porcentaje_cambio_casos` > 0.1, j=7, color = "red") %>%
    autofit() %>%

    bg(i = ~ (`cambio` >= quantil_casos[7]), j=6, bg = "#F8696B" ) %>%
    bg(i = ~ (`cambio` >= quantil_casos[6] & `cambio` < quantil_casos[7]), j=6, bg = "#FB9F76" ) %>%
    bg(i = ~ (`cambio` >= quantil_casos[5] & `cambio` < quantil_casos[6]), j=6, bg = "#FDBE7C" ) %>%
    bg(i = ~ (`cambio` >= quantil_casos[4] & `cambio` < quantil_casos[5]), j=6, bg = "#FFDA81" ) %>%
    bg(i = ~ (`cambio` >= quantil_casos[3] &  `cambio` < quantil_casos[4]), j=6, bg = "#F5E883" ) %>%
    bg(i = ~ (`cambio` >= quantil_casos[2] & `cambio` < quantil_casos[3]), j=6, bg = "#B0D47F" ) %>%
    bg(i = ~ (`cambio` >= quantil_casos[1] & `cambio` < quantil_casos[2] ), j=6, bg = "#63BE7B" ) %>%


    bg(j = 8, bg = "#D4C19C", part = "body") %>%
    mk_par(i = ~ `porcentaje_cambio_casos` < 0.1, j=8,
           value = as_paragraph(as_chunk("")," ",
                                as_image(src = "bin/banderas/abajo.png", width = .30, height = .30)), part = "body") %>%
    mk_par(i = ~ `porcentaje_cambio_casos` > 0.1, j=8,
           value = as_paragraph(as_chunk("")," ",
                                as_image(src = "bin/banderas/arriba.png", width = .30, height = .30)), part = "body") %>%
    merge_at(i = 1, j=7:8, part = "header") %>%

    set_header_labels(Posicion="Posición",
                      bandera="País",
                      plot = "Casos nuevos",
                      Casos_nuevos_1="No. Casos en los últimos 7 días",
                      porcentaje_casos="%",
                      porcentaje_cambio_casos="% de cambio respecto a los 7 días previos")


  tabla.defunciones <- n %>%
    mutate(cambio = as.numeric(gsub("%","",porcentaje_defunciones))) %>%
    select(-data) %>%
    flextable(col_keys = c("Posicion", "bandera",
                           "Pais", "plot",
                           "Defunciones_nuevas_1", "porcentaje_defunciones",
                           "porcentaje_cambio_defunciones",
                           "code") ) %>%
    mk_par(
      j = "plot",
      value = as_paragraph(gg_chunk(value = plot, height = 0.7, width = 2))) %>%

    bg(i = 1, bg = "#2B614D", part = "header") %>%
    color(i = 1, color = "white", part = "header") %>%
    bold(part = "all") %>%
    align(align = "center", part = "all") %>%
    font(fontname = "Montserrat", part = "all") %>%
    fontsize(size = 20, part = "all") %>%
    merge_at(i = 1, j=2:3, part = "header") %>%
    fontsize(j=2, size = 40, part = "body") %>%  #tamaño de bandera cuando es emoji
    # compose(j=2, value = as_paragraph(as_image(src = code, width = .50, height = .35),
    #                                   " ", as_chunk(m$Pais))) %>%  #para que quede junto al nombre

    compose(j=2, value = as_paragraph(as_chunk(""), as_image(src = code, width = .50, height = .35))) %>% #bandera rectangular en su propia columna
    hline(part="all", border = officer::fp_border(color = "#2B614D", style = "solid", width = 3)) %>%
    color(i = ~ `porcentaje_cambio_defunciones` > 0.1, j=7, color = "red") %>%
    #autofit(add_w =  60 , add_h =  0 , part =  c ( "all" )) %>%

    #Cambiamos el color de la variable con la escala de color

    bg(i = ~ (`cambio` >= quantil_defunciones[7]), j=6, bg = "#F8696B" ) %>%
    bg(i = ~ (`cambio` >= quantil_defunciones[6] & `cambio` < quantil_defunciones[7]), j=6, bg = "#FB9F76" ) %>%
    bg(i = ~ (`cambio` >= quantil_defunciones[5] & `cambio` < quantil_defunciones[6]), j=6, bg = "#FDBE7C" ) %>%
    bg(i = ~ (`cambio` >= quantil_defunciones[4] & `cambio` < quantil_defunciones[5]), j=6, bg = "#FFDA81" ) %>%
    bg(i = ~ (`cambio` >= quantil_defunciones[3] &  `cambio` < quantil_defunciones[4]), j=6, bg = "#F5E883" ) %>%
    bg(i = ~ (`cambio` >= quantil_defunciones[2] & `cambio` < quantil_defunciones[3]), j=6, bg = "#B0D47F" ) %>%
    bg(i = ~ (`cambio` >= quantil_defunciones[1] & `cambio` < quantil_defunciones[2] ), j=6, bg = "#63BE7B" ) %>%

    bg(j = 8, bg = "#D4C19C", part = "body") %>%
    mk_par(i = ~ `porcentaje_cambio_defunciones` < 0.1, j=8,
           value = as_paragraph(as_chunk("")," ",
                                as_image(src = "bin/banderas/abajo.png", width = .30, height = .30)), part = "body") %>%
    mk_par(i = ~ `porcentaje_cambio_defunciones` > 0.1, j=8,
           value = as_paragraph(as_chunk("")," ",
                                as_image(src = "bin/banderas/arriba.png", width = .30, height = .30)), part = "body") %>%
    merge_at(i = 1, j=7:8, part = "header") %>%
    set_header_labels(Posicion="Posición",
                      bandera="País",
                      plot = "Defunciones nuevas",
                      Defunciones_nuevas_1="No. Defunciones en los últimos 7 días",
                      porcentaje_defunciones="%",
                      porcentaje_cambio_defunciones="% de cambio respecto a los 7 días previos") %>%
    #fit_to_width (max_width , inc =  1L)
    #autofit(add_w =  50 , add_h =  0 , part =  c ( "all" ))
    width(j=c(6), width=2) %>%
    width(j=c(7), width=2)

  print(tabla.casos)
  print(tabla.defunciones)

#dim_pretty(tabla.defunciones)


save_as_image(tabla.casos, path="productos/casos.png", webshot = "webshot")
save_as_image(tabla.defunciones, path="productos/defunciones.png", webshot = "webshot")

####ppt####


# my_pres <- read_pptx("Documents/GitHub/Situation-Report-Who/Pandas r/src/Plantilla.pptx")
# add_slide(my_pres, layout = "Tablas_", master = "Tema de Office") %>%
#   ph_with(value = tabla.defunciones,
#           location = ph_location_label(ph_label = "CuadroTexto 6")) %>%
#   print(my_pres, target = "Documents/GitHub/Situation-Report-Who/graficas dr Alessio/prueba.pptx")
ppt <- read_pptx()
layout_summary(ppt)
layout_properties(ppt, layout = "Two Content", master = "Office Theme")
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
#ppt <- ph_with(ppt, value = pba, location = ph_location(left = 3, top = 5))
ppt <- ph_with(x = ppt, external_img("productos/casos.png"),
               location = ph_location_left())
ppt <- ph_with(x = ppt, external_img("productos/defunciones.png"),
               location = ph_location_right())
print(ppt, target = "productos/indicadores_internacional.pptx")
#alternativa: pero solo guarda tabla sin imagenes
#save_as_pptx ( tabla.casos , tabla.defunciones,  path  =  "dir/temis.pptx" )

print("Ya estan listos los indicadores internacionales")




}



