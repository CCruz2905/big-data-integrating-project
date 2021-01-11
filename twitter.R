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

