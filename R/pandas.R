#' Datos internacionales COVID-19
#' @export

pandas <- function(fecha_de_trabajo) {
  fecha_de_trabajo<-fecha_de_trabajo
  library(tidyverse)
  library(lubridate)
  library(data.table)
  Sys.setlocale("LC_TIME", "es_ES")
  Fecha <-as.character(fecha_de_trabajo,format="%A, %d de %B de %Y")

  situacion <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv",
                        encoding = "UTF-8")


  situacion_mapa<-situacion%>%
    select(Fecha=1,
           Pais_ing=3,
           Region_OMS=WHO_region,
           Casos_nuevos=New_cases,
           Casos_acumulados=Cumulative_cases,
           Defunciones_nuevas=New_deaths,
           Defunciones_acumuladas=Cumulative_deaths) %>%
    mutate(Fecha=as.Date(Fecha)) %>%
    filter(Fecha==fecha_de_trabajo)
  longitud<-length(unique(situacion_mapa$Pais_ing))

  if(longitud>=237){

    poblacion<-read.csv("https://github.com/Temisesba/P-blico/raw/main/sabana_poblacion_bd.csv", encoding = "UTF-8")

    situacion_mapa <- left_join(situacion_mapa,poblacion, by = "Pais_ing") %>%
      mutate(IA.pmh=round((Casos_acumulados/Poblacion)*1000000,2)) %>%
      mutate(Tasa_mortalidad=round((Defunciones_acumuladas/Poblacion)*1000000,2)) %>%
      filter(Pais_ing != "Other") %>%
      arrange(Codigo_pais) %>%
      select("País español"=Pais_esp,
             "Código"=Codigo_pais,
             "Continente"=Continente,
             "Clave"=Clave,
             #"Region OMS"=Region_OMS,
             "Casos incidentes del día por país(día actual)"=Casos_nuevos,
             "Casos acumulados al día por país"=Casos_acumulados,
             "Incidencia acumulada por millón de habitantes"=IA.pmh,
             "Defunciones incidentes al día por país(día actual)"=Defunciones_nuevas,
             "Defunciones acumuladas al día por país"=Defunciones_acumuladas,
             "Tasa de Mortalidad por millón de habitantes"=Tasa_mortalidad) %>%
      mutate("Label de corte"=paste("Fuente:Reportes de situación de la Organización Mundial de la Salud. Corte al",Fecha))

    situacion_mapa$Código[is.na(situacion_mapa$Código)]<-"NA"

    situacion_mapa<-situacion_mapa %>%
      arrange(Código)

    situacion_mapa$`Label de corte`[2:236] <- ""

    global <- situacion %>%
      select(Fecha=1,
             Region_OMS=WHO_region,
             Casos_acumulados=Cumulative_cases,
             Casos_nuevos=New_cases,
             Defunciones_acumuladas=Cumulative_deaths,
             Defunciones_nuevas=New_deaths) %>%
      mutate(Fecha=as.Date(Fecha)) %>%
      filter(Fecha==fecha_de_trabajo) %>%
      group_by(Region_OMS) %>%
      summarise(Casos_acumulados=sum(Casos_acumulados),
                Casos_nuevos=sum(Casos_nuevos),
                Defunciones_acumuladas=sum(Defunciones_acumuladas),
                Defunciones_nuevas=sum(Defunciones_nuevas))

    global <- data.frame(t(global[-1])) %>%
      select("AFRO"=1,
             "AMRO"=2,
             "EMRO"=3,
             "EURO"=4,
             "SEARO"=6,
             "WPRO"=7,
             "Other"=5) %>%
      mutate(total= rowSums(.[,1:7]))

    global <- data.frame(t(global)) %>%
      select("Casos acumulados"=1,
             "Casos reportados al día de hoy"=2,
             "Defunciones acumuladas"=3,
             "Defunciones nuevas"=4)

    global<-setDT(global, keep.rownames = TRUE)[] %>%
      rename("Región OMS"=1)



    dir <-paste0("dir/",format(fecha_de_trabajo, format="%Y%m%d"))

    #Si el directorio no existe, lo crea...
    if (!dir.exists(dir)){
      dir.create(dir)
      print("Directorio creado satisfactoriamente")
      write.csv(global,paste0(dir,"/",format(fecha_de_trabajo, format="%Y%m%d")," Global.csv"),row.names = F)
      write.csv(situacion_mapa,paste0(dir,"/",format(fecha_de_trabajo, format="%Y%m%d")," Mapa.csv"),row.names=F)

    } else {
      print("Ya existe el directorio...")
      write.csv(global,paste0(dir,"/",format(fecha_de_trabajo, format="%Y%m%d")," Global.csv"),row.names = F)
      write.csv(situacion_mapa,paste0(dir,"/",format(fecha_de_trabajo, format="%Y%m%d")," Mapa.csv"),row.names=F)
    }


    return(print(paste("La OMS ya actualizó",longitud,"países. Ya estan listos los pandas.")))
  } else {
    return(print(paste("La OMS no ha terminado de actualizar, van",longitud,"países.")))
  }
}






