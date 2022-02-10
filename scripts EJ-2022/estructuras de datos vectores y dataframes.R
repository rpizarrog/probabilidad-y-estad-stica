

# Estructuras de datos en R

# Vectores
# c() función sirve para integrar o concatenar varios valores del mismo tipo
edades <- c(19, 34, 23, 25, 34, 54, 34, 28, 18, 36)
nombres <- c('JUAN', 'GUADALUPE', 'PATY', 'OSCAR', 'RAUL',
             'LILIANA', 'RUBEN', 'NANCY', 'LULU', 'ADRIANA')
generos <- c('M', 'F', 'F', 'M', 'M', 'F', 'M', 'F', 'F', 'F')


# Presentar los vectores
edades
nombres
generos

# Acceder a valores
edades[1]
nombres[1]

edades[5]
nombres[5]
generos[5]


# Accediendo a varios valores
edades[1:5]
nombres[1:5]

# No mostrar algunos valores
edades
edades[-4]

nombres
nombres[-4]

# Usando print()
print(edades)
edades

# Usando c() para mostrar algunos específicos
edades
edades[c(1, 3, 5, 7, 9)]
nombres
nombres[c(1, 3, 5, 7, 9)]


# usando variables
posiciones.pares <- c(2, 4, 6, 8, 10)
edades
edades[posiciones.pares]


# Data Frames o conjuntos de datos renglones columnas
# Data Frame es un conjunto de vectores
datos <- data.frame(nombres, generos, edades)

# Muestra los datos en consola
datos

# Muestra los datos tabular
View(datos)

# Acceder a valores de un data frame

# datos[renglon(es),columna(as)]

datos[1,c(1,2,3)]

datos[1:5, 1:2]

# Accediendo por nombres de columnas o atributos
datos[1:7, c('nombres', 'edades')]

# Accediendo por el signo de $
datos$nombres
datos$edades
datos$generos

# Accediento a valores específicos
datos$nombres[1:5]

datos$nombres[pares] # 2, 4, 6, 8, 10


