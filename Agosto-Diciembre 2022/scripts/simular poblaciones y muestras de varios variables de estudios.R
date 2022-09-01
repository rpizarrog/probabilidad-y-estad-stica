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

