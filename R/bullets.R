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


  }
