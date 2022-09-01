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

