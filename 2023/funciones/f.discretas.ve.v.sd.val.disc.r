f.discretas.ve.v.sd.val.disc <- function(discretas, casos) {
  library(ggplot2)
  # Inicializando valores
  n <- sum(casos)
  x = discretas
  
  # Calculando columas
  prob_x <- casos / n
  acumulado <- cumsum(prob_x)
  x.prob_x <- x * prob_x
  
  
  # Valor esperado
  VE <- sum(x.prob_x)
  
  # Varianza y Desviación Std
  x_menos_VE <- x - VE
  x_menos_VE.CUAD <- x_menos_VE ^ 2
  x_menos_VE.CUAD.prob_x <- x_menos_VE.CUAD * prob_x
  varianza <- sum(x_menos_VE.CUAD.prob_x)
  desv.std <- sqrt(varianza)
  
  
  # Grafica
  #plot <- ggplot(mpg, aes(displ, hwy, colour = class)) +
  #        geom_point()
  
  tabla <- data.frame(x, casos, prob_x,
                      acumulado, x.prob_x,
                      VE,
                      x_menos_VE,
                      x_menos_VE.CUAD,
                      x_menos_VE.CUAD.prob_x
  )
  estadisticos <- list(tabla = tabla, x = x, N = n,
                       VE = VE, varianza = varianza, desv.std = desv.std)
  
  estadisticos
  
  
}