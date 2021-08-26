#' Datos internacionales COVID-19 7 dias
#' @export

incidencias_siete_dias<-function(fecha_de_trabajo){
  fecha_de_trabajo<-fecha_de_trabajo

  pacman::p_load(tidyverse)

  #Cargamos la base de datos de la OMS
  situacion <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv",
                        encoding = "UTF-8")

  #Cambiamos la configuración de las fechas y creamos un objeto para almacenar la fecha
  Sys.setlocale("LC_TIME", "es_ES")
  Fecha <-as.character(fecha_de_trabajo,format="%A, %d de %B de %Y")


  #Renombramos y recodificamos todo el DataFrame, renombramos variables
  #y recodificamos las observaciones de la región

  siete_dias<-situacion%>%
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

  longitud<-length((siete_dias %>%
                      select(Fecha,Pais_ing ) %>%
                      filter(Fecha == fecha_de_trabajo))$Pais_ing)

  if(longitud>=236){

    #Filtramos ultimos siete días
    siete_dias<-siete_dias %>%
      filter(Fecha>fecha_de_trabajo-7) %>%
      group_by(Pais_ing) %>%
      summarise(Casos_nuevos=sum(Casos_nuevos), Defunciones_nuevas=sum(Defunciones_nuevas))

    write.csv(siete_dias, "productos/Incidencias_Siete_dias.csv",row.names=F)

    return(print(paste("Ya estan listos los siete días.")))
  } else {
    return(print(paste("La OMS no ha terminado de actualizar, van",longitud,"países.")))
  }

}

