library(httr)
library(jsonlite)
library(rjson)
library(ggplot2)

cat('
############################################################\n
####################    LEER JSON      ####################\n
############################################################\n
')

url <- "https://api.datos.gob.mx/v1/condiciones-atmosfericas"
print(url)
# Obtener datos de la api
respuesta <- GET(url)
datosGenerales <- content(respuesta,"text")
datosJSON <- paste(datosGenerales,collapse = " ")

# Obtención de la lista de observaciones
datosJSON <- fromJSON(datosJSON)

# Variables para graficar
estados <- 0
temperaturas <- 0

# Recolección de datos
j <- 1

for (i in 1:length(datosJSON$results)) {
   estados[j] = datosJSON$results[[i]]$state
   temperaturas[j] = datosJSON$results[[i]]$tempc
   j <- j + 1
}

# Creación de la tabla
temperaturas.df <- data.frame(cbind(estados, temperaturas))
print(temperaturas.df)

# Gráfica
ggplot(temperaturas.df, aes(x = temperaturas, fill = estados)) +
   geom_bar(position = 'identity', alpha = 0.5) +
   theme_minimal()

cat('
############################################################\n
####################	FIN TEXTO      ####################\n
############################################################\n
')

# Crear archivo para wordcount
temperaturas.data <- data.frame(temperaturas.df)
write.csv(temperaturas.data, file = './R-OUTPUT/temperaturas.data', row.names = FALSE)

print("Archivo creado")
