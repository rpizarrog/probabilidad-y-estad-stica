# Simular poblaciones y muestras

# Población edades

pob.edades <- sample(x = 20:27, size = 6600, 
                 replace = TRUE)
pob.edades

media.pob.edades <- mean(pob.edades)
media.pob.edades

# muestra edades

muestra.edades <- sample(x = pob.edades, size = 660)
muestra.edades

media.muestra.edades <- mean(muestra.edades)
media.muestra.edades


# OTRO EJEMPLO DE ... cALIFICACIONES DE ALUMNOS
# RANGO ENTRE 70 A 100
# Poblacion de calificaiones alumnos
pob.calificaciones <- sample(x = 70:100, size = 6600, 
                   replace = TRUE)
pob.calificaciones

media.califa <- mean(pob.calificaciones)
media.califa

# muestra edades

muestra.califica <- sample(x = pob.calificaciones, size = 660)
muestra.califica

media.muestra.califica <- mean(muestra.califica)
media.muestra.califica


# Otro ejempl NUMERO DE MATERIAS QUE CURSAN LOS ALUMNOS
# EN UN RANGO DE 3 A 7
pob.num.materias <- sample(x = 3:7, size = 6600, 
                             replace = TRUE)
pob.num.materias

# Algunas veces la media si arroja valores reales
# no significa nada porque la realidad es que son valores enteros
# Se redeondea con la función round() o se puede dejar con valores reales
# depende del signiicado de los datos y de la interpretación
media.num.materias <- mean(pob.num.materias)
media.num.materias

# muestra número de materias

muestra.num.materias <- sample(x = pob.num.materias, size = 660)
muestra.num.materias

media.muestra.num.materias <- mean(muestra.num.materias)
media.muestra.num.materias


# Simular datos de aves
# edad: de 2 a 4 años
# peso: de 1 a 2 kgs.
# color: NEGRO, GRIS, BLANCO
# long.pico: 1 a 3 cms. 
# long.ala: 10 a 15 cms

# Muestras representativas  de aves 1000
ave.edad <- sample(x = 2:4, size = 1000, 
                   replace = TRUE)
ave.edad

ave.peso <- sample(x = 1:2, size = 1000, 
               replace = TRUE)
ave.peso

ave.color <- sample(x = c("NEGRO", "GRIS", "BLANCO"), size = 1000, 
                    replace = TRUE)
ave.color

ave.long.pico <- sample(x = 1:3, size = 1000, 
                               replace = TRUE)
ave.long.pico

ave.long.ala <- sample(x = 10:15, size = 1000, 
                       replace = TRUE)
ave.long.ala

datos.aves <- data.frame(ave.edad, ave.peso, 
                         ave.color, ave.long.ala,
                         ave.long.pico)
datos.aves



