# Correlacion y Regresi´pon 

library(ggplot2)
library(readr)
library(dplyr)
library(knitr)
library(PerformanceAnalytics) # Para coorelaciones gráficas


x <- c(60, 50, 40, 45, 20, 45, 65, 50, 80, 45, 67)
y <- c(100, 120, 130, 132, 120, 130, 110, 80, 90, 78, 120)

datos <- data.frame(x, y)
datos

chart.Correlation(datos)

r <- cor(datos$x, datos$y)
r

# Dispersion
ggplot(data = datos, aes(x = x, y = y)) +
    geom_point(inherit.aes = TRUE)
       
# Modelo de regresión linea
# Y = a + bx
# se requiere otener a
# obtener b

modelo <- lm(data = datos, formula = y ~ x)
modelo

resumen <- summary(modelo)

a <- modelo$coefficients[1]
b <- modelo$coefficients[2]


# Diagrama de dispersion con la recta de regresi´n
# La recta de regresiín es la estimación 
# por cada valor de x
# en R se le llama valroes ajustados

ggplot(data = datos, aes(x = x, y = y)) +
  geom_point(col='red') +
  geom_line(aes(x = x, y = modelo$fitted.values), col='blue')



# PREDECIR = ESTIMAR = PRONOSTICAR = FUTURO
# Y = a + b * x

xi = c(45, 40, 30, 60)


Y = a + b * xi
Y <- a + b * xi
Y


Y.prediccion <- predict(object = modelo, 
                newdata = data.frame(x = xi))

Y 
Y.prediccion



# Datos con women

women <- data.frame(women)
women

modelo <- lm(data = women, formula = weight ~ height)
modelo


ggplot(data = women, aes(x = height, y = weight)) +
  geom_point(col='red') +
  geom_line(aes(x = height, y = modelo$fitted.values), col='blue')

# PRedicción 
Y.prediccion <- predict(object = modelo, 
                        newdata = data.frame(height = c(70)))
Y.prediccion