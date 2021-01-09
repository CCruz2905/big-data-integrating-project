library(httr)
library(jsonlite)
library(rjson)
library(ggplot2)
library(RCurl)

## LEER JSON##
leerApi <- function(url) {
   flujoDatos <- 0

   # Obtener datos de la api
   respuesta<-GET(url)
   datosGenerales<-content(respuesta,"text")
   flujoDatos<-paste(datosGenerales,collapse = " ")

   #Obtención de la lista de observaciones 
   flujoDatos<-fromJSON(flujoDatos)

   # Variables para graficar
   estados <- 0
   temperaturas <- 0

   # Recolección de datos
   j <- 1

   for (i in 1:length(flujoDatos$results)) {
      estados[j] = flujoDatos$results[[i]]$state
      temperaturas[j] = flujoDatos$results[[i]]$tempc
      j <- j + 1
   }

   # Creación de la tabla
   tablaDatos <- data.frame(cbind(estados, temperaturas))
   return (tablaDatos)
}

# Creación de gráfica
graficaCondicion <- function(tablaDatos) {
   ggplot(data, aes(x = temperaturas, fill = estados)) +
   geom_bar(position = 'identity', alpha = 0.5) +
   theme_minimal()
}
# Enlace donde obtendremos la api
url <- "https://api.datos.gob.mx/v1/condiciones-atmosfericas"
data <- leerApi(url)
graficaCondicion(data)
