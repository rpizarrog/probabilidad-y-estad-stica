# Generar data.frame de productos, marcas  y tallas

# Instalar antes la librería "dplyr"
# install.packages("dplyr")

# Cargar librerías
library(dplyr)

# Hay cuatro tipos de productos
productos <- c('PANTALON', 'CAMISA', 'PLAYERA', 'ZAPATO')

# Todos los productos tienes las mismas tres tipos de marcas
marcas <- c("X", "Y", "Z")

# Los pantalones, las camisas y las playeras tienen tallas diferentes
# C Chica, M Mediana, G Grande, X Extra Grande
tallas.PANTALON <- c("C", "M", "G")
tallas.CAMISAS <- c("M", "G")
tallas.PLAYERAS <- c("C", "M", "G", "X")

# Los zapatos tienen medidas particulares
tallas.ZAPATOS <- as.character(c(24:28))


productos
marcas
tallas.PANTALON
tallas.CAMISAS
tallas.PLAYERAS
tallas.ZAPATOS

generos <- c("Femenino", "Masculino")
generos

# La función union() del paquete dplyr permite unir 
# conjuntos de datos
# LA función cbind() del paquete base agrega columnas

pantalones <- union(cbind("productos" = productos[1], merge(marcas, tallas.PANTALON), "generos" = generos[1]), 
                    cbind("productos" = productos[1],merge(marcas, tallas.PANTALON), "generos" = generos[2]))
                         
                      

pantalones


camisas <- union(cbind("productos" = productos[2], merge(marcas, tallas.CAMISAS), "generos" = generos[1]), 
                 cbind("productos" = productos[2], merge(marcas, tallas.CAMISAS), "generos" = generos[2]))



camisas

playeras <- union(cbind("productos" = productos[3], merge(marcas, tallas.PLAYERAS), "generos" = generos[1]), 
                   cbind("productos" = productos[3], merge(marcas, tallas.PLAYERAS), "generos" = generos[2]))
playeras

zapatos <- union(cbind("productos" = productos[4], merge(marcas, tallas.ZAPATOS), "generos" = generos[1]), 
                 cbind("productos" = productos[4], merge(marcas, tallas.ZAPATOS), "generos" = generos[2]))
zapatos


# ESPACIO MUESTRAL
S <- rbind(pantalones, camisas, playeras, zapatos)

# La función names() indica el nombre a columnas de un conjunto de datos o data.frame
names(S) <- c("productos", "marcas", "tallas", "generos")
S
