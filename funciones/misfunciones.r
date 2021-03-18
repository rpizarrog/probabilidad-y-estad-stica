# Crer Funciones en R
# Las funciones son bloques de código que tienen un identificador
# es decir se llaman de alguna manera
# Pueden recibir o no parámetros entre paréntesis con las cuales se hace operaciones
# dentro de la función se hacen operaciones diversas...
# Las funciones se utilzian para optimizar ñprogramas
# para reutilizar una y otra vez
# Dentro de los {} es el bloque de código de una función
# LAs funciones son sinónimos de métodos
#nombre <- function() {
#  
#}

sumar <- function(x, y) {
  suma <- x + y 
  suma
}

foperaciones <- function (x,y,tipo) {
  if (tipo == 1) {
    res <- x + y
  }
  if (tipo == 2) {
    res <- x - y
  }
  if (tipo == 3) {
    res <- x * y
  }
  if (tipo == 4) {
    res <- x / y
  }
  if (tipo == 5) {
    res <- x ^ y
  }
  res
}

# Esta función determina el CV de un vector o de un conjunto de datos
fcoefvar <- function (datos) {
      CV <- sd(datos) / mean(datos) * 100
      CV
}
