# install.packages("pdftools")
library(pdftools)
library(stringr)

#Obtenemos el patrón de los documentos en pdf
constancias<-list.files("Constancias/", pattern = "pdf$")


DataFrame <- list()
for (i in 1:length(constancias)) {

  documento <- paste0("Constancias/", constancias[i])

  pdf <- lapply(documento, pdf_text)

  texto_documento<-str_split(str_trim(pdf[[1]]), pattern="\n")

  nombre_persona<-trimws(texto_documento[[1]][9])

  numero_documento <- as.integer(stringr::str_extract(constancias[i], "\\d+"))

  #Creamos el DataFrame con información de las personas
  DataFrame[[i]]<-data.frame(NOMBRE =nombre_persona, NUMERO =  numero_documento, DOCUMENTO = constancias[i])

}

Constancias_DataFrame_Completo<-plyr::rbind.fill(DataFrame)
