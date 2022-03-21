# Muestreos

# Aleatorio simple

edades <- round(rnorm(n = 30, mean = 20, sd = 2))
edades

# Selecciona 5 alumnos de manera simple

muestra <- sample(x = edades, size = 5, replace = FALSE)
muestra


# Construir alumnos
no.control <- as.character(seq(from =21040001, to =21045600, by = 1))
N <- length(no.control)
N

nombres <- NULL
for (r in 1:N) {
  letras <- sample(x = LETTERS, 8)
  nombres[r] <- paste(letras, collapse = '')
}
nombres
  
alumnos <- data.frame(no.control, nombres)

# Seleccionar 10 alumnos al azar

n <- 100

seleccionados <- sample(1:N, size = n, replace = FALSE)
seleccionados

alumnos[seleccionados, ]


# Muestreo aleatorio sistematizado

1:N
saltos <- N / n; saltos

secuencia <- seq(from = 1, to = N, 
                 by = saltos)
secuencia


alumnos.selecciondos <- alumnos[secuencia, ] # Renglón, columna
alumnos.selecciondos


