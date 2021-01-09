library(httr)
library(jsonlite)
library(rjson)
library(ggplot2)
library(RCurl)

leerApi <- function(url, opcion) {
   flujoDatos <- 0

   # Obtener datos de la api
   respuesta<-GET(url)
   datosGenerales<-content(respuesta,"text")
   flujoDatos<-paste(datosGenerales,collapse = " ")

   #Obtención de la lista de observaciones 
   flujoDatos<-fromJSON(flujoDatos)

   # Variables para graficar
   ciudades <- 0
   temperaturas <- 0

   # Recolección de datos
   j <- 1

   # Se desea conocer unicamente el estado de las últimas 10 ciudades,
   # ya que al ser 100 datos, la gráfica no queda legible
   if (opcion == 1) {
      for (i in 90:100) {
         ciudades[j] = flujoDatos$results[[i]]$name
         temperaturas[j] = flujoDatos$results[[i]]$tempc
         j <- j + 1
      }
   }

   # Segunda opción para utilizar toda la información para el wordcount y wordcloud
   if (opcion == 2) {
      for (i in 1:length(flujoDatos$results)) {
         ciudades[j] = flujoDatos$results[[i]]$name
         temperaturas[j] = flujoDatos$results[[i]]$tempc
      j <- j + 1
      }
   }

   # Creación de la tabla
   tablaDatos <- data.frame(cbind(ciudades, temperaturas))
   return (tablaDatos)
}

# Creación de gráfica
graficaCondicion <- function(tablaDatos) {
   # Inside bars
   ggplot(data = tablaDatos, aes(x = ciudades, y = temperaturas)) +
   geom_bar(stat = "identity", fill = "steelblue")+
   geom_text(aes(label = temperaturas), vjust = 1.6, color = "white", size = 3.5)+
   theme_minimal()
}

# Enlace donde obtendremos la api
url <- "https://api.datos.gob.mx/v1/condiciones-atmosfericas"
data <- leerApi(url, 1)
graficaCondicion(data)
