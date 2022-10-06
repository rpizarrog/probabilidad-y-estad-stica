# Función que muestra un saludo
f_saludo1 <- function() {
  print("Hola saludos")
}

f_saludo2 <- function(nombre) {
  paste("Hola", nombre, "como estás")
}


f_promedio <- function(numeros) {
  print(sum(numeros) / length(numeros))
}

f_saludo3 <- function(nombre, edad) {
  paste("Hola", nombre, "como estás.", "Tienes ", edad, "años")
}


saludo1 <- function() {
  print("Hola")
}

saludo2 <- function(nombre) {
  cat("Hola", nombre)
}

saludo3 <- function(nombre, edad) {
  cat("Hola", nombre, "tienes", edad, "años. Felicidades")
}

promedio <- function(numeros) {
  n <- length(numeros)
  promedio = sum(numeros) / n
  cat(promedio)
}
crea_datos <- function(r) {
  datos <- data.frame(x = sample(10:100, size = r),
                      y = sample(10:100, size = r))
  datos
}


