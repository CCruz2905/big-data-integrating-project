library(RColorBrewer)
library(wordcloud)

#########	TEXT	#########

mortalidadWordCloud <- read.table(file = 'Hadoop/mortalidad.txt', sep = '\t')


wordcloud(words = mortalidadWordCloud$V1,
  freq = mortalidadWordCloud$V2,
  min.freq = 2,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2"))

#########	JSON	#########

temperaturasWordCloud <- read.table(file = 'Hadoop/temperaturas.txt', sep = '\t')

wordcloud(words = temperaturasWordCloud$V1,
  freq = temperaturasWordCloud$V2,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2"))

#########	DATABASE	#########

empleadosWordCloud <- read.table(file = 'Hadoop/empleados.txt', sep = '\t')

wordcloud(words = empleadosWordCloud$V1,
  freq = empleadosWordCloud$V2,
  min.freq = 3,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2"))

#########       TWITTER        #########

egolandWordCloud <- read.table(file = 'Hadoop/egoland.txt', sep = '\t')

wordcloud(words = egolandWordCloud$V1,
  freq = egolandWordCloud$V2,
  min.freq = 3,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2"))

