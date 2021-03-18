# Crear Funciones en R

# Las funciones son bloques de código que tienen un identificador
# es decir se llaman de alguna manera
# Pueden recibir o no parámetros entre paréntesis con las cuales se hace operaciones
# dentro de la función se hacen operaciones diversas...
# Las funciones se utilizan para optimizar programas
# para reutilizar una y otra vez
# Dentro de los {} es el bloque de código de una función
# Las funciones son sinónimos de métodos
#nombre <- function() {
#  
#}

funcionhola <- function() {
  print("Hola mundo de R desde la Función")
}

funcionholanom <- function(nombre) {
  paste("Hola", nombre, "te saludamos desde la Función")
}

sumar <- function(x, y) {
  suma <- x + y 
  suma # Se devuelve el valor de la suma
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
  res # Devolver 
}

# Esta función determina el CV de un vector o un conjunto de datos de una columna
fcoefvar <- function (datos) {
      CV <- sd(datos) / mean(datos) * 100
      CV
}

edades <- c(23,24,25,26,27,22)
nombres <- c("Juan", "Pedro", "Luis", "José", "Mary", "Laura")
datos.personas <- data.frame(nombres, edades)

