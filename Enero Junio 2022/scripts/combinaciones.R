# Combinaciones
# install.packages("gtools")
library(gtools)
ingredientes <- c('tomate', 'zanahoria', 'papa', 'brócoli')
n <- length(ingredientes)
k <- 2 # combinar de dos en dos

combinaciones <- combinations(n = n, r = k, v = ingredientes)
combinaciones

# permutaciones
# Si importa el orden en que se agreguen los ingredientes por aquello de los sabores
permutaciones <- permutations(n = n, r = k, v = ingredientes)
permutaciones
