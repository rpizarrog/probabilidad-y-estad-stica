# construir una poblacion y nuna muestra
# de una variable de interés llamada promedio escolar o rendimiento escolar
# El rendimiento escolar tiene que ver con el promedio que 
# que llevan los alumnos en su carrera profesional


# Población 
# Simular tener alumnos con promedio entre 70 y 100
set.seed(2022)  # Genera los mismos números al run de nuevo
poblacion <- sample(x = 70:100, 
                    size = 6500, 
                    replace = TRUE)   # Sacar una muestra
poblacion
# Muestra

N <- length(poblacion)# total de elementos de una poblacion
N

suma <- sum(poblacion)
suma

media1 <- suma / N
media1   # media población se le llama parámetro poblacional

media2 <- mean(poblacion)
media2


# Muestra
# Determinar el 10% de los elementos
n <- 650 # 10% de 6500 es 650
muestra <- sample(x = poblacion, 
                  size = n, 
                  replace = FALSE)
muestra

media.m <- mean(muestra)
media.m

# Los estadísticos que se obtienen 
# de una muestra son estimaciones de 
# una población.
# La muestra es parte de una población
# Se llaman estadísticos a los valores que se determinan a partir una muestra
# Se llaman parámetros a los valores que se determinan a partir de una población



# Estructura y Resumen de los datos
str(poblacion)
str(muestra)

summary(poblacion)
summary(muestra)
