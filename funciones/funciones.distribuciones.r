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


# Función de distribución de Poisson conforme a la Fórmula
f.prob.poisson <- function (media, x) {
  e <- 2.71828
  prob <- media^x * e^(-media) / factorial(x)
  prob
}

# Función de disrtibución hipergeométrica
# Recibe estos pa´rametros:
# N Total de elementos de la población
# n Elementos de la muestra o ensayos
# r número de elementos considerados como éxtio
# x Valores que puede tener la variabel aleatoria discreta
f.prob.hiper <- function (x, N, n, r) {
  numerador <- (factorial(r) / (factorial(x) * factorial(r-x))) * (factorial(N-r) / (factorial(n-x) * factorial((N-r)-(n-x)))) 
  denominador <- (factorial(N) / (factorial(n) * factorial(N-n)))
  
  prob <- numerador / denominador
  prob
}
