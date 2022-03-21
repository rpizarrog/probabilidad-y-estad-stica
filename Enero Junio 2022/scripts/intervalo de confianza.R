library (visualize)
confianza = 0.95
z.critico <- abs(qnorm(p = (1 - confianza) / 2))
visualize.norm(stat = c(-z.critico, z.critico),  section = "tails") +
  text(0, 0.2, paste(confianza * 100, "%", "\n", 
                     "alfa=", (1 - confianza), "\n",  "alfa / 2 = ", 
                     (1 - confianza) /  2, sep = ""),  col = "black")


source("https://raw.githubusercontent.com/rpizarrog/Probabilidad-y-EstadIstica-VIRTUAL-DISTANCIA/main/funciones/funciones.para.distribuciones.r")

# Datos
media.m <- 4.507
desv.p <- 0.04
n <- 64
confianza = 0.95

f.intervalo.confianza(media = media.m, desv = desv.p, confianza = confianza, n = 64)

# o esta es la misma
f.intervalo.confianza.z(media = media.m, desv = desv.p, confianza = confianza, n = 64)


# Intervalo de confianza para t
# Datos
confianza = 0.90
n <- 15
desv.m <- 1.464
media.m <- 5

t.critico <- abs(qt(p = (1 - confianza) / 2, n-1))
t.critico

t.critico <- f.t.int.conf(confianza = confianza, n = n)
t.critico

intervalo <- f.intervalo.confianza.t(media = media.m, desv = desv.m, confianza = confianza, n = n)
intervalo

# Visualizar t
visualize.t(stat = c(-t.critico, t.critico),  section = "tails") +
  text(0, 0.2, paste(confianza * 100, "%", "\n", 
                     "alfa=", (1 - confianza), "\n",  "alfa / 2 = ", 
                     (1 - confianza) /  2, sep = ""),  col = "black")


# Ejercicio Falla de San Andrés

confianza = 0.95
n <- 50
desv.m <- 17.2
desv.p <- 17.2 # Considerando que se conoce la desv. std
media.m <- 39.8

SE <- desv.m / sqrt(n) # Error estándar
SE


# Si se conoce desviación 
z.critico <- f.z.int.conf(confianza = confianza)
z.critico

ME <- z.critico * SE # ME Margen de Error
ME

intervalo <- f.intervalo.confianza.z(media = media.m, desv = desv.m, confianza = confianza, n = n)
intervalo

# No se conoce desviación 

t.critico <- f.t.int.conf(confianza = confianza, n = n)
t.critico

ME <- t.critico * SE # ME Margen de Error
ME

intervalo <- f.intervalo.confianza.t(media = media.m, desv = desv.m, confianza = confianza, n = n)
intervalo



# Para distribución de la proporción
# Tiene que ver con la aproximación de una binomial
# a una normal 
# Mendenhall, William, Robert J. Beaver, and Barbara M. Beaver. 2010. Introducción a La Probabilidad y Estadística. 13th ed. Cengage Learning Editores, S.A. de C.V.,.
# Página 276-277
# Ejercicio 7.6 del libro

# Datos
p = 0.73
q = 1 - p
n = 100

SE <- sqrt(p * q / n)
SE

z.critico <- abs(qnorm(p = (1 - 0.95)/2))
z.critico

ME <- z.critico * 0.04439595
ME

# IC Interalo de confianza de la proporción 
intervalo[1] <- p - ME
intervalo[2] <- p + ME

intervalo




visualize.binom(stat = c(290, 350), 
                size = 500, 
                prob = 0.6, 
                section = "bounded", 
                strict = c(1,1)) 



