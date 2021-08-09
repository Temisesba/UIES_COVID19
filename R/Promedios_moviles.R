#' Promedios moviles COVID-19
#' @export

Promedios_moviles<-function(fecha_de_trabajo){
  fecha_de_trabajo<-fecha_de_trabajo
  library(tidyverse)
  library(lubridate)
  library(data.table)
  Sys.setlocale("LC_TIME", "es_ES")
  Fecha <-as.character(fecha_de_trabajo,format="%A, %d de %B de %Y")
  poblacion<-read.csv("https://github.com/Temisesba/P-blico/raw/main/sabana_poblacion_bd.csv", encoding = "UTF-8")

  situacion <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv",
                        encoding = "UTF-8") %>%
    rename(Date_reported =1)

  #####correr desde aqui
  situacion_PM<-situacion%>%
    select(Fecha=1,
           Pais_ing=3,
           Region_OMS=WHO_region,
           Casos_nuevos=New_cases,
           Casos_acumulados=Cumulative_cases,
           Defunciones_nuevas=New_deaths,
           Defunciones_acumuladas=Cumulative_deaths) %>%
    mutate(Fecha=as.Date(Fecha))


  situacion_PM <- left_join(situacion_PM,poblacion, by = "Pais_ing") %>%
    mutate(IA.pmh=round((Casos_nuevos/Poblacion)*1000000,2)) %>%
    mutate(Defunciones_acumuladas_pmh=round((Defunciones_acumuladas/Poblacion)*1000000,2)) %>%
    mutate(Casos_acumulados_pmh=round((Casos_acumulados/Poblacion)*1000000,2)) %>%
    mutate(Defunciones_nuevas_pmh=((Defunciones_nuevas/Poblacion)*1000000)) %>%
    mutate(Letalidad=round((Defunciones_acumuladas/Casos_acumulados),4)) %>%
    mutate(mortalidad=round((Defunciones_acumuladas_pmh/Poblacion)*1000000,2)) %>%
    # filter(Pais_ing != "Other") %>%
    arrange(Codigo_pais) %>%
    select("Fecha",
           "País ingles"=Pais_ing,
           "País español"=Pais_esp,
           "Código"=Codigo_pais,
           "Continente"=Continente,
           "Region OMS"=Region_OMS,
           "Poblacion",
           "Casos nuevos"=Casos_nuevos,
           "Casos acumulados"=Casos_acumulados,
           "Defunciones nuevas"=Defunciones_nuevas,
           "Defunciones acumuladas"=Defunciones_acumuladas,
           "Casos nuevos millón de habitantes"=IA.pmh,
           "Casos acumulados millon de habitantes"=Casos_acumulados_pmh,
           "Defunciones nuevas millon de habitantes"=Defunciones_nuevas_pmh,
           "Defunciones acumuladas millón de habitantes"=Defunciones_acumuladas_pmh,
           Letalidad,
           "Mortalidad"=mortalidad)

  situacion_PM$`Casos nuevos millón de habitantes`[is.na(situacion_PM$`Casos nuevos millón de habitantes`)]<-0
  situacion_PM$`Casos acumulados millon de habitantes`[is.na(situacion_PM$`Casos acumulados millon de habitantes`)]<-0
  situacion_PM$`Defunciones nuevas millon de habitantes`[is.na(situacion_PM$`Defunciones nuevas millon de habitantes`)]<-0
  situacion_PM$`Defunciones acumuladas millón de habitantes`[is.na(situacion_PM$`Defunciones acumuladas millón de habitantes`)]<-0
  situacion_PM$Letalidad[is.na(situacion_PM$Letalidad)]<-0
  situacion_PM$Mortalidad[is.na(situacion_PM$Mortalidad)]<-0

  situacion_PM$`Casos nuevos millón de habitantes`[is.infinite(situacion_PM$`Casos nuevos millón de habitantes`)]<-0
  situacion_PM$`Casos acumulados millon de habitantes`[is.infinite(situacion_PM$`Casos acumulados millon de habitantes`)]<-0
  situacion_PM$`Defunciones nuevas millon de habitantes`[is.infinite(situacion_PM$`Defunciones nuevas millon de habitantes`)]<-0
  situacion_PM$`Defunciones acumuladas millón de habitantes`[is.infinite(situacion_PM$`Defunciones acumuladas millón de habitantes`)]<-0
  situacion_PM$Letalidad[is.infinite(situacion_PM$Letalidad)]<-0
  situacion_PM$Mortalidad[is.infinite(situacion_PM$Mortalidad)]<-0

  # mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>%
  # mutate_all(function(x) ifelse(is.na(x), 0, x))

  situacion_PM$Código[is.na(situacion_PM$Código)]<-"NA"

  ###2da parte
  dias<-as.integer((length(unique(situacion$Date_reported)))-7)

  promedios_moviles <- list()

  for (i in 1:dias) {
    promedios_moviles[[i]]<-situacion_PM %>%
      filter(Fecha >= ((fecha_de_trabajo-(1*i)+2)-7) & Fecha <= fecha_de_trabajo-(1*i)+1)  %>%
      group_by(max(Fecha), `País ingles`) %>%
      summarise(`PM Casos nuevos millón de habitantes`=round(mean(`Casos nuevos millón de habitantes`),2),
                `PM Defunciones nuevas millon de habitantes`=round(mean(`Defunciones nuevas millon de habitantes`),2))
  }

  PM<-plyr::rbind.fill(promedios_moviles) %>%
    rename(Fecha=1)


  PM_final<-left_join(situacion_PM,PM)


  # write.csv(PM_final, "Promedios_moviles.csv",row.names=F)


  # return(print("Ya estan listos los promedios moviles."))
  return(PM_final)
}

