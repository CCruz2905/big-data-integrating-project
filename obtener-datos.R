cargarLibrerias <- function() {
   library(httr)
   library(jsonlite)
   library(rjson)
   library(ggplot2)
   library(RCurl)
   library(readxl)
   library(dplyr)
   library(twitteR)
   library(NLP)
   library(tm)
   library(RColorBrewer)
   library(wordcloud)
}

################### LEER TEXTO ###################
crearTexto <- function() {
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
crearModelo <- function(tablaDatos) {
   modelo <- lm(n_death ~ years, data = tablaDatos)

   return (modelo)
}

# Función para generar la gráfica de regresión lineal simple
graficarTexto <- function(data, predict) {                                                                              qplot(x = years, y = n_death, data = data,
   main = "Muertes en Saltillo por año", ylab = "Muertes generales",
   xlab = "Años", geom = c("point"),
   method = "lm") + geom_line(aes(y = deathp), lwd = 1.2, color = 4) +
   theme_light()
}

#Valores Predichos
data_mortalidad <- crearTexto()
modelo <- crearModelo(data_mortalidad)
summary(modelo)
# Predicción del modelo
deathp <- predict(modelo)
graficarTexto(data_mortalidad, deathp)

################### FIN TEXTO ####################

################### LEER JSON ####################
leerJson <- function(url) {
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
graficaJson <- function(tablaDatos) {
   ggplot(data, aes(x = temperaturas, fill = estados)) +
   geom_bar(position = 'identity', alpha = 0.5) +
   theme_light()
}
# Enlace donde obtendremos la api
url <- "https://api.datos.gob.mx/v1/condiciones-atmosfericas"
data_temperaturas <- leerJson(url)
graficaJson(data)

################### FIN JSON #####################

################### DATABASE #####################
# Función que permite leer lo extraído de la base de datos
leerDatabase <- function() {
   varchivo <- read.table(file="database", sep=",")

   return (varchivo)
}

# Función para asignarle nombre a las columnas
cambiarColNames <- function(varchivo) {
   names(varchivo)[1] <- "no_emp"
   names(varchivo)[2] <- "birth_date"
   names(varchivo)[3] <- "first_name"
   names(varchivo)[4] <- "last_name"
   names(varchivo)[5] <- "gender"
   names(varchivo)[6] <- "hire_date"
   names(varchivo)[7] <- "job_title"

   return (varchivo)
}

# Función para realizar gráfica
graficarDatabase <- function(varchivo) {
   ggplot(varchivo, aes(x = job_title, y = as.Date(hire_date), colour = gender)) +
   geom_boxplot(outlier.shape = NA) +
   geom_jitter(size = 0.2) +
   theme_minimal()
}

# Inicio
varchivo <- leerDatabase()
varchivo <- cambiarColNames(varchivo)
graficarDatabase(varchivo)

################### FIN DB #######################

################### TWITTER ######################
accesoTwitter <- function() {
   # Configuración del acceso desde R para Twitter
   api_key <- "LPsLW0ffDNhLcLoZSQYu9BCqm"
   api_secret_key <- "xK1AarZoRnF1koRzQgphZxCxoBtGNpBqXfrE3RaUMsjv6mJb5c"
   access_token <- "1333965401340878848-9qFWneiS7MKgLs4JAfQYdEVe23QTki"
   access_token_secret <- "UqnyeNPDZ626EUOAR4HCuaOWGvj6RmtPShW8PinTj4nWD"
   setup_twitter_oauth(api_key, api_secret_key, access_token, access_token_secret)
}

crearCorpus <- function(palabra, cantidad) {
   # Se define el criterio o criterios (por medio de vectores) que se desea buscar, así como la cantidad de tweets a bajar
   tweets <- searchTwitter(palabra, n = cantidad)

   # Se crea un dataframe con la información proveniente de los tweets
   tweets.df <- twListToDF(tweets)

   # Se crea el Vector Curpus para graficar en cloud
   mycorpus <- VCorpus(VectorSource(tweets.df$text))

   return (mycorpus)
}

# Gráfica
graficarTwitter <- function(mycorpus) {
   wordcloud(mycorpus, min.freq = 3, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
}

# Abrir acceso a twitter
accesoTwitter()
mycorpus <- crearCorpus("#EGOLAND", 200)

graficarTwitter(mycorpus)

## Limpieza de Datos
# Remueve palabras reservadas
mycorpus <- tm_map(mycorpus, removeWords, stopwords())

# Limpia el cuerpo de las puntuaciones, de forma manual
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
mycorpus <- tm_map(mycorpus, content_transformer(removeNumPunct))

remove_url <- function(x) gsub("http[^[:space:]]*", "", x)
mycorpus <- tm_map(mycorpus, content_transformer(remove_url))

# Elimina puntuaciones standard
mycorpus <- tm_map(mycorpus, removePunctuations)
mycorpus <- tm_map(mycorpus, content_transformer(to_lower)
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus <- tm_map(mycorpus, stemDocument)

myStopWords <- c("now", "this", "new", "news", "egoland", "luzugam", "youtub", "pero", "está", "habiai", "que", "est", "esta")
mycorpus <- tm_map(mycorpus, removeWords, myStopWords)

graficarTwitter(mycorpus)

################### FIN TW #######################
