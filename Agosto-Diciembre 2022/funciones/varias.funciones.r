# Algunas funciones
f_prueba1 <- function(nombre) {
  cat("Hola", nombre)
}

f_prueba2 <- function(nombre = NULL, edad= NULL) {
  msj <- NULL
  if (is.null(nombre)) {
    stop("Al menos proporciona el nombre ...")
  }

  if(is.null(edad)) {
    msj <- "No proporcionaste la edad... ..."
    msj <- c(msj, "Hola", nombre)
  } else {
      msj <- c(msj, "Hola", nombre, "tienes ", edad, "años ")
  }

  cat(msj)
}

f.promedio <- function(nums) {
  if(!is.vector(nums)) {
    stop("El argumento nums, debe ser un vector")
  }
  else {
    n <- length(nums)
    promedio <- sum(nums) / n
  }
  return(promedio)
}


f.discretas.ve.v.sd <- function(casos) {
  # Inicializando valores
  n <- sum(casos)
  x = 0:(length(casos) - 1)

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

  tabla <- data.frame(x, casos, prob_x,
                      acumulado, x.prob_x,
                      VE,
                      x_menos_VE,
                      x_menos_VE.CUAD.prob_x
                      )
  estadisticos <- list(tabla = tabla, x = x, N = n,
                       VE = VE, varianza = varianza, desv.std = desv.std)

  estadisticos

}
