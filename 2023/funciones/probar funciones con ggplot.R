# Función para tabla binomial
f_tabla_binom <- function(n, exito) {
  tabla <- data.frame(x = 0:n,
                      f.x = dbinom(x = 0:n, size = n, prob = exito),
                      F.x = pbinom(q = 0:4, size = 4, prob = 0.80))
  tabla
}

# Devolver histograma, histograma con densidad y acumulado
f.hist.dens.discreta <- function(datos) {
  library(ggplot2)

  g1 <- ggplot(data = datos) +
    geom_col(aes(x = x, y = f.x), fill='blue')
  
  casos <- NULL
  for(r in 1:nrow(datos)) {
    casos <- c(casos, c(rep(datos$x[r], round(datos[r,2] * 100))))
  }
  
  g2 <- ggplot() +
    geom_col(data = datos, aes(x = x, y = f.x)) +
    geom_density(aes(x = casos), color = 'red')

  g3 <- ggplot(data = datos) +
    geom_line(aes(x = x, y = F.x), color = 'red') +
    geom_point(aes(x = x, y = F.x), color = 'blue') 
    
    

    

  lista <- list(hist = g1, dens = g2, acum = g3)
  
  return(lista)
  
}

# Los datos
N = 100
n = 6
discretas <- 0:n
probs <- c(0.08, 0.12, 0.22, 0.30, 0.16, 0.08, 0.04)

datos <- data.frame(x = 0:n, 
                    frec  = probs * N,
                    f.x = probs,
                    F.x = cumsum(probs))

g <- ggplot(data = datos) +
  geom_col(aes(x = x, y = casos))

g

ggplot(data = datos, aes(x = casos)) + 
  geom_density(color = 'red')


ggplot(data = datos,) + 
  geom_line(aes(frec), 
            stat="density", 
            color = 'red')






