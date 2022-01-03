#' Alertómetro
#' @export

alertometro <- function(){

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

  if (!file.exists("bin/Alertometro_plantilla.pptx")) {
    download.file("https://github.com/Temisesba/UIES_COVID19/raw/main/bin/Alertometro_plantilla.pptx",
                  "bin/Alertometro_plantilla.pptx", mode = "wb")
    print("Plantilla lista")
  }

  pacman::p_load(tidyverse,flextable, ggplot2, ggrepel, webshot, officer, Hmisc,readxl )

  #Colocamos las propiedades para el formato de salida de la fecha
  Sys.setlocale("LC_TIME", "es_ES")
  Fecha <-toupper(as.character(Sys.Date(),
                               format="%d %b %Y"))

  #Utilizamos el selector de file.choose para elegir el documento que se tiene que cargar
  ppt <- read_pptx("bin/Alertometro_plantilla.pptx")
  alertometroxlsx <- file.choose()


  texto <- read_excel(alertometroxlsx) %>%
    mutate(Numero = 1:nrow(.) ) %>%
    select(Numero=1,
           Fecha = `Fecha`,
           Tema = `Tema`,
           Resumen =`Contenido`,
           Verificacion = `Verificación`,
           "Liga de verificacion"= `Liga de verificación`) %>%
    mutate(color = recode(Verificacion,
                          "Ratificado" = "verde",
                          "Rectificado" = "rojo",
                          "Pendiente" = "amarillo"))

  #Obtenemos el número máximo de noticias
  max <-max(texto$Numero)

  #Obtenemos y seleccionamos la dorección de las imágenes según la orientación y número de noticias
  texto<-texto %>%
    mutate(direccion = rep(c("derecha", "izquierda"), 5)[1:max]) %>%
    unite("direccion_color", color:direccion, sep = "_", remove = FALSE) %>%
    select(-color,-direccion) %>%
    mutate(direccion_inter = rep(c("left", "right"), 5)[1:max])

  #Obtenemos las variables para las ligas de verificación y el número de noticias
  liga_verif<-paste(texto$`Liga de verificacion`, collapse = "\n")
  numero_noticias<-nrow(texto)

  #Propiedades del pptx
  #layout_summary(ppt)
  #layout_properties(ppt, layout = "Alertometro1", master = "Tema de Office")


  # Creamos el objeto donde crecerá el pptx
  ppt <- add_slide(ppt, layout = "titulo", master = "Tema de Office")

  ########### PORTADA ################
  ppt <- ph_with(x = ppt, "Alertómetro",
                 location= ph_location_label(ph_label = "titulo")) %>%
    ph_with(Fecha,
            location= ph_location_label(ph_label = "fecha"))


  #Creamos la condición para los valores que tomará el alertómetro de 4 - 7 noticias
  if(numero_noticias == 4){
    layout <- "Alertometro0"

  }else if(numero_noticias == 5){
    layout <- "Alertometro1"

  }else if (numero_noticias == 6) {
    layout <- "Alertometro2"
  }else{
    layout <- "Alertometro3"
  }

  ####### AÑADIMOS EL SLIDE DE LA PRESENTACION DEL ALERTÓMETRO #######
  ppt <- add_slide(ppt, layout = layout, master = "Tema de Office" ) %>%

    ph_with(block_list(fpar(
      ftext(Fecha,
            fp_text(font.size = 18,
                    bold = T,
                    color = "white",
                    font.family = "Helvetica" )),

      fp_p = fp_par(text.align = "right"))),

      location = ph_location_label(ph_label = "cuadro_fecha")) %>%

    ph_with(block_list(fpar(
      ftext(liga_verif,
            fp_text(font.size = 11,
                    bold = F,
                    color = "black",
                    font.family = "Helvetica" )),

      fp_p = fp_par(text.align = "justify"))),

      location = ph_location_label(ph_label = "fuentes"))



  #Se crea la iteración principal para destinar la posición de los elementos del alertómetro
  #Se distribuye según la posición de la noticia

  for (i in 1:numero_noticias){

    ppt<-ph_with(x = ppt, external_img(paste0("bin/imagenes/", texto$direccion_color[i], ".png")),
                 location = ph_location_label(ph_label = as.character(texto$Numero[i])),
                 use_loc_size = T) %>%

      ph_with(block_list(fpar(

        ftext(texto$Resumen[i],
              fp_text(font.size = 11,
                      bold = F,
                      color = "black",
                      font.family = "Helvetica" )),

        fp_p = fp_par(text.align = texto$direccion_inter[i]))),

        location = ph_location_label(ph_label = paste0("cuadro_",texto$Numero[i])))

  }


  print(ppt, target = paste0("productos/Alertómetro_",(Sys.Date()) ,".pptx"))
  print("Se ha generado correctamente el alertometro del día.")



}
