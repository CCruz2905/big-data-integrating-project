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
