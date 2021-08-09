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
}
