# Complemento examen U5
# 21 Cuales son los valores de a y b de un modelo de regesión lineal simp,e con estos datos en donde la variable

library(readr)
datos <- read.csv("https://raw.githubusercontent.com/rpizarrog/probabilidad-y-estad-stica/master/Agosto-Diciembre%202022/datos/world%20ranking%20universities.csv")

modelo <- lm(data = datos, formula = rank ~ publications)
summary(modelo)
# Cuales son los valores de a y b

# 22

library(readr)
datos <- read.csv("https://raw.githubusercontent.com/rpizarrog/probabilidad-y-estad-stica/master/Agosto-Diciembre%202022/datos/world%20ranking%20universities.csv")

modelo <- lm(data = datos, formula = rank ~ publications)
summary(modelo)

# Cual es el valor de Multiple R Square y que significa? 
# Habiendo creado el modelo de regresión lineal simple con estos datos
# y en donde la variable depende de publications (rank ~ publications)

# 23 

library(readr)
datos <- read.csv("https://raw.githubusercontent.com/rpizarrog/probabilidad-y-estad-stica/master/Agosto-Diciembre%202022/datos/world%20ranking%20universities.csv")

modelo <- lm(data = datos, formula = rank ~ publications)
summary(modelo)

# Cual es el la predicción al publicar 10000
# Habiendo creado el modelo de regresión lineal simple con estos datos
# y en donde la variable depende de publications (rank ~ publications)


# 24

library(readr)
datos <- read.csv("https://raw.githubusercontent.com/rpizarrog/probabilidad-y-estad-stica/master/Agosto-Diciembre%202022/datos/world%20ranking%20universities.csv")

modelo <- lm(data = datos, formula = rank ~ publications)
summary(modelo)

# Cual es el la predicción al publicar 20000
# Habiendo creado el modelo de regresión lineal simple con estos datos
# y en donde la variable depende de publications (rank ~ publications)

# 25 Diagrama de dispersión ranking universidades
# ¿Cuál es el diagrama de dispersión que refleja la relación entre cantidad de pubicaciones y ranking de las universidades?


# 26 Datos de radiaciones y temperatura

library(readr)
datos <- read.csv("https://raw.githubusercontent.com/rpizarrog/probabilidad-y-estad-stica/master/Agosto-Diciembre%202022/datos/variables%20de%20ambiente%20y%20radiacion.csv")

# Temperature ~ Radiation

# Al calcular la correlación entre los pares de las variables Temperature y Radiation y Humidity y Radiation, 
# ¿Cuál para de variables tiene mayor correlación positiva?
# (Temperature, Radiation) ó (Humidity, Radiation)
cor(datos$Temperature, datos$Radiation)
cor(datos$Humidity, datos$Radiation)

# 27 Datos de Radiación

library(readr)
datos <- read.csv("https://raw.githubusercontent.com/rpizarrog/probabilidad-y-estad-stica/master/Agosto-Diciembre%202022/datos/variables%20de%20ambiente%20y%20radiacion.csv")


# Construir modelos de regresión lineal simple entre Temperature y Radiation

modelo <- lm(data = datos, formula = Radiation ~ Temperature)
summary(modelo)

# El valor de Multiple R-squared:  0.5402

# 28

library(readr)
datos <- read.csv("https://raw.githubusercontent.com/rpizarrog/probabilidad-y-estad-stica/master/Agosto-Diciembre%202022/datos/variables%20de%20ambiente%20y%20radiacion.csv")


# Construir modelos de regresión lineal simple entre Temperature y Radiation

modelo <- lm(data = datos, formula = Radiation ~ Temperature)
summary(modelo)

# ¿Cuál es el valor de los coeficientes a y b ?

# 29 
library(readr)
datos <- read.csv("https://raw.githubusercontent.com/rpizarrog/probabilidad-y-estad-stica/master/Agosto-Diciembre%202022/datos/variables%20de%20ambiente%20y%20radiacion.csv")


# Construir modelos de regresión lineal simple entre Temperature y Radiation

modelo <- lm(data = datos, formula = Radiation ~ Temperature)
summary(modelo)

# ¿Cuál es la predicción del valor de la radiación a una temperatura de 50 grados Fahrenheit

predict(object = modelo, newdata = data.frame(Temperature = 50))


# 30
library(readr)
datos <- read.csv("https://raw.githubusercontent.com/rpizarrog/probabilidad-y-estad-stica/master/Agosto-Diciembre%202022/datos/variables%20de%20ambiente%20y%20radiacion.csv")


# Construir modelos de regresión lineal simple entre Temperature y Radiation

modelo <- lm(data = datos, formula = Radiation ~ Temperature)
summary(modelo)

# ¿Cuál es la predicción del valor de la radiación a una temperatura de 50 grados Fahrenheit

predict(object = modelo, newdata = data.frame(Temperature = 60))
