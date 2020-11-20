# funcion par distribuciones
# 20 Nov 2020

library(gtools)


# Función que devuelve la probabildiad conforme y de acuerdo a la 
# la fórmula de distribuión binomial
# Recibe tres parámetros: 
# los valores de x, e valor de n y la probabildia de éxito
# Devuelve las proabilidades para cada valor de la variable aleatoria discreta
f.prob.binom <- function (x,n,exito) {
  fracaso <- 1 - exito
  prob <- (factorial(n) / (factorial(x) * factorial(n-x)))  * ((exito^x) * (fracaso ^ (n-x)))
  prob
}