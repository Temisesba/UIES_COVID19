#' bullets COVID-19
#' @export

bullets<-function(fecha_de_trabajo){
  fecha_de_trabajo<-(fecha_de_trabajo)

  library(ggthemes)
  library(tidyverse)
  library(data.table)
  library(extrafont)
  library(showtext)
  library(lubridate)
  library(magrittr)
  library(flextable)



  OMS<-reporte_de_situacion(fecha_de_trabajo)

  #Regresa: 1: situacion, 2: longitud, 3:situacion_mapa

if(OMS[2]>=237){

  DATOS<-graficas_tablas(fecha_de_trabajo,OMS[1],OMS[3])

  return(DATOS)
  }

}


