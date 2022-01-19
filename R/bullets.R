#' bullets COVID-19
#' @export

bullets<-function(fecha_de_trabajo){

  fecha_de_trabajo<-(fecha_de_trabajo)

  #### INSTALACIÓN DE LA FUENTE MONTSERRAT AUTOMÁTICAMENTE ####
  if(!require("extrafont")){
    print("No lo tenemos")
    install.packages("extrafont")

    if(!is.null(windowsFonts()[["Montserrat"]])){
      print("Omitiendo instalación de fuente Montserrat")

    }else{
      print("No tenemos la fuente Montserrat, se instalará...")
      font_import(paths = "bin/font/")
      loadfonts()
    }

  }else{
    print("Ya tenemos fuente Montserrat")

  }

  pacman::p_load(ggthemes, tidyverse, data.table, extrafont, showtext, lubridate,
             magrittr, flextable)



  OMS<-reporte_de_situacion(fecha_de_trabajo)

  #Regresa: 1: situacion, 2: longitud, 3:situacion_mapa

if(OMS[2]>=237){

  invisible(capture.output(DATOS<-graficas_tablas(fecha_de_trabajo,OMS[1],OMS[3])))

  print(paste("La OMS ya actualizó",OMS[2],"países. Ya estan listos los bullets"))
  return(DATOS)
} else {
  return(print(paste("La OMS no ha terminado de actualizar, van",OMS[2],"países.")))
}

}


