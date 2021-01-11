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
