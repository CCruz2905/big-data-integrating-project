library(readxl)
library(dplyr)
library(ggplot2)

cat('
############################################################\n
####################    LEER TEXTO      ####################\n
############################################################\n
')

# Lectura del archivo
mortalidad <- read_excel("mortalidad_05.xlsx")
print(mortalidad)

# De toda la información tomamos únicamente la de Saltillo
data_mortalidad <- mortalidad %>% filter(desc_municipio == "Saltillo")
print(data_mortalidad)

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
mortalidad.df <- data.frame(cbind(years, n_death))

# Se convierten a númerico los valores del data.frame para poder aplicar el modelo
# de regresión lineal simple
mortalidad.df[] <- lapply(mortalidad.df, function(x) as.numeric(as.character(x)))
print(mortalidad.df)

# Modelo para regresión lineal
modelo <- lm(n_death ~ years, data = mortalidad.df)
print(summary(modelo))
deathp <- predict(modelo)

# Gráfica
qplot(x = years, y = n_death, data = mortalidad.df,
   main = "Defunciones en Saltillo por año 1994-2019",
   xlab = "Años", ylab = "Cantidad de defunciones") +
   geom_point() +
   geom_line(aes(y = deathp), lwd = 1.2, color = 5) +
   theme_minimal()

cat('
############################################################\n
####################    FIN TEXTO       ####################\n
############################################################\n
')

# Crear archivo para wordcount
mortalidad.data <- data.frame(mortalidad[,7:31])
write.csv(mortalidad[,7:31], file = './R-OUTPUT/mortalidad.data', row.names = FALSE)
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
library(ggplot2)

cat('
############################################################\n
####################     DATABASES      ####################\n
############################################################\n
')

# Obtener información
database <- read.table(file="database", sep=",")

# Renombrar columnas
names(database)[1] <- "no_emp"
   names(database)[2] <- "birth_date"
   names(database)[3] <- "first_name"
   names(database)[4] <- "last_name"
   names(database)[5] <- "gender"
   names(database)[6] <- "hire_date"
   names(database)[7] <- "job_title"

ggplot(database, aes(x = job_title, y = as.Date(hire_date), colour = gender)) +
   geom_boxplot(outlier.shape = NA) +
   geom_jitter(size = 0.2) +
   theme_minimal()

cat('
############################################################\n
####################	 DATABASES      ####################\n
############################################################\n
')

# Crear archivo para wordcount
empleados.data <- data.frame(database)
write.csv(empleados.data, file = './R-OUTPUT/empleados.data', row.names = FALSE)

cat('Archivo creado')
library(twitteR)
library(NLP)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

cat('
############################################################\n
####################    LEER TWITTER    ####################\n
############################################################\n
')

# Configuración del acceso desde R para Twitter
api_key <- "LPsLW0ffDNhLcLoZSQYu9BCqm"
api_secret_key <- "xK1AarZoRnF1koRzQgphZxCxoBtGNpBqXfrE3RaUMsjv6mJb5c"
access_token <- "1333965401340878848-9qFWneiS7MKgLs4JAfQYdEVe23QTki"
access_token_secret <- "UqnyeNPDZ626EUOAR4HCuaOWGvj6RmtPShW8PinTj4nWD"
setup_twitter_oauth(api_key, api_secret_key, access_token, access_token_secret)

# Se define el criterio o criterios (por medio de vectores) que se desea buscar, así como la cantidad de tweets a bajar
tweets <- searchTwitter("#EGOLAND", n = 200)

# Se crea un dataframe con la información proveniente de los tweets
tweets.df <- twListToDF(tweets)

# Se crea el Vector Curpus para graficar en cloud
mycorpus <- VCorpus(VectorSource(tweets.df$text))

print('Grafica 1')
# Graficar
wordcloud(mycorpus, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

## Limpieza de Datos
# Remueve palabras reservadas
mycorpus <- tm_map(mycorpus, removeWords, stopwords())

# Limpia el cuerpo de las puntuaciones, de forma manual
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
mycorpus <- tm_map(mycorpus, content_transformer(removeNumPunct))

remove_url <- function(x) gsub("http[^[:space:]]*", "", x)
mycorpus <- tm_map(mycorpus, content_transformer(remove_url))

# Elimina puntuaciones standard
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus, content_transformer(tolower))
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus <- tm_map(mycorpus, stemDocument)

myStopWords <- c("now", "this", "new", "news", "infoegoland", "egoland", "luzugam", "youtub", "pero", "está", "habiai", "que", "est", "esta", "mymalkpon", "nos", "del", "fue", "van", "cada")
mycorpus <- tm_map(mycorpus, removeWords, myStopWords)

print('Grafica 2')
# Graficar
wordcloud(mycorpus, random.order = FALSE, min.freq = 3, colors = brewer.pal(8, "Dark2"))

cat('
############################################################\n
####################	FIN TWITTER     ####################\n
############################################################\n
')

# Crear archivo para wordcount
twitter.data <- data.frame(tweets.df$text)
write.csv(twitter.data, file = './R-OUTPUT/twitter.data', row.names = FALSE)

print("Archivo creado")

