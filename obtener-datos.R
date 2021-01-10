library(httr)
library(jsonlite)
library(rjson)
library(ggplot2)
library(RCurl)
library(readxl)
library(dplyr)

################### LEER TEXTO ###################
crearTabla <- function() {
   # Lectura del archivo
   mortalidad <- read_excel("mortalidad_05.xlsx")
   # De toda la información tomamos únicamente la de Saltillo
   data_mortalidad <- mortalidad %>% filter(desc_municipio == "Saltillo")
   # La información que se requiere está en las columnas, así que las tomamos
   years <- data.frame(names(data_mortalidad[,7:32]))
   names(years)[1] <- "years"
   # Obtendremos las muertes generales, de las columnas correspondientes a los años
   deaths <- data.frame(data_mortalidad[1, 7:32])

   # Crearemos un arreglo de las muertes
   n_death <- 0
   for (i in 1:length(deaths)) {
      n_death[i] = deaths[[i]]
   }
   n_death <- data.frame(n_death)

   # Unimos ambos dataframe, de esta forma tenemos una columna años y le corresponde
   # una columna con la cantidad de muertes por año
   tablaDatos <- data.frame(cbind(years, n_death))

   # Se convierten a númerico los valores del data.frame para poder aplicar el modelo
   # de regresión lineal simple
   tablaDatos[] <- lapply(tablaDatos, function(x) as.numeric(as.character(x)))

   return (tablaDatos)
}

# Funcion que crea el modelo de regresión lineal simple
crearModelo <- function() {
   modelo <- lm(n_death ~ years, data = tablaDatos2)

   return (modelo)
}

# Función para generar la gráfica de regresión lineal simple
graficarMortalidad <- function(data, predict) {                                                                              qplot(x = years, y = n_death, data = data,
   main = "Muertes en Saltillo por año", ylab = "Muertes generales",
   xlab = "Años", geom = c("point"),
   method = "lm") + geom_line(aes(y = deathp), lwd = 1.2, color = 4) +
   theme_minimal()
}

#Valores Predichos
data_mortalidad <- crearTabla()
modelo <- crearModelo()
summary(modelo)
# Predicción del modelo
deathp <- predict(modelo)
graficarMortalidad(data_mortalidad, deathp)

################### FIN TEXTO ####################

################### LEER JSON ####################
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

# Creación de gráfica (Análisis discriminante)
graficaCondicion <- function(tablaDatos) {
   ggplot(data, aes(x = temperaturas, fill = estados)) +
   geom_bar(position = 'identity', alpha = 0.5) +
   theme_minimal()
}
# Enlace donde obtendremos la api
url <- "https://api.datos.gob.mx/v1/condiciones-atmosfericas"
data_temperaturas <- leerApi(url)
graficaCondicion(data)

################### FIN JSON #####################

################### DATABASE #####################

################### FIN DB #######################
