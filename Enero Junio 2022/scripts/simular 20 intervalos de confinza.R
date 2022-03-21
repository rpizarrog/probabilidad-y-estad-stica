# Simular una población de la edad de 1000 personas
# en un rango de 18 a 64 años y con características de ser normal
# con media poblacional alrededor de 32 y 
# desviación poblacional de 5

set.seed(2021)

source("https://raw.githubusercontent.com/rpizarrog/Probabilidad-y-EstadIstica-VIRTUAL-DISTANCIA/main/funciones/funciones.para.distribuciones.r")

edad <- round(rnorm(n = 1000, mean = 32, sd = 5))
# Simular 20 muestras de 100 elementos

edad

desv.p <- sd(edad)
desv.p
confianza <- 0.90

nm <- 20 # número de muestras 20
n <- 100  # tamaño de cada muestra 
muestra <- NULL
for (i in 1:nm) {
  #print(i)
  muestra[[i]] = sample(edad, size = n, replace = TRUE)
}
muestra

# Determianr estadísticos de la muestra
media.m <- NULL
desv.m <- NULL
for (i in 1:nm) {
  media.m[i] <- mean(muestra[[i]])
   desv.m[i] <- sd(muestra[[i]])
}


tabla <- data.frame(m = 1:nm, media.m, desv.m, desv.p, 
                    intervalo.a = f.intervalo.confianza.z(media = media.m, 
                                                          desv = desv.p, 
                                                          confianza = confianza, 
                                                          n = n)[1:20],
                    intervalo.b = f.intervalo.confianza.z(media = media.m, 
                                                          desv.p = desv.p, 
                                                          confianza = confianza, 
                                                          n = n)[31:40]
)
tabla


# Pruebas de normalidad de las muestras
# Densidad
par(mfrow=c(4, 5))
# library(ggplo)


for (i in 1:n) {
  plot(density(muestra[[i]]), main=paste("Muestra", i),
       ylab='Densidad', col='blue3', xlab='x', las=1, lwd=4)
}

# Qplot

library(car)
qqPlot(muestra[[11]], pch=19, las=1, main='QQplot',
       xlab='Cuantiles teóricos', ylab='Cuantiles muestrales')


# PRueba Shapiro Test
shapiro.test(muestra[[1]])



# Prueba de hipótesis
# Caso de planta química de libro: 
# Anderson, D., Sweeney, D., & Williams,
# T. (2008). 
# Estadística para administración y economía Estadística para administración y economía. 10a. Edición. México, D.F: 
# Cengage Learning Editores,S.A. de C.V.

# Datos 
media.p <- 880
media.m <- 871
desv.m <- 21
n <- 50

source("https://raw.githubusercontent.com/rpizarrog/Probabilidad-y-EstadIstica-VIRTUAL-DISTANCIA/main/funciones/funciones.para.distribuciones.r")

z <- f.devolver.z.prueba(media.m = media.m, media.p = media.p, 
                    desv.m = desv.m, n = n)
z


confianza <- 0.95
alfa <- 1 - confianza
z.critico <- abs(qnorm(alfa / 2))

-z.critico; z.critico

conclusion <- "Acepta Ho"
if (z < z.critico | z > z.critico)
    conclusion = "Se rechaza Ho"

conclusion

# Visualizar
library(visualize)

par(mfrow=c(1, 1))
visualize.norm(stat = c(-z.critico, z.critico),  
               section = "tails") +
  text(0, 0.2, paste(confianza * 100, "%", "\n", 
                     "alfa=", (1 - confianza), "\n",  "alfa / 2 = ", 
                     (1 - confianza) /  2, sep = ""),  col = "black") +
  abline(v = z, col = 'red', lwd = 1, lty= 4)


# Valor de p
p = pnorm(z) + pnorm(abs(z), lower.tail = FALSE)
p

visualize.norm(stat = c(-z, z),  
               section = "tails") +
  text(2, 0.35, paste("p ",intToUtf8(8804)," 0.10 ***,", "\n", 
                      "p ",intToUtf8(8804),"0.05 ** ,", "\n",
                      "p ",intToUtf8(8804),"0.01 *  ,"),  
       col = "black", cex=.8)
  



# Valores de p
# Es la región azul de la gráfica de densidad de la distribución
pnorm(q = -z.critico)

pnorm(q = z.critico, lower.tail = FALSE)
visualize.norm(stat = c(-z.critico, z.critico),  
               section = "tails") +
  text(0, 0.2, paste(confianza * 100, "%", "\n", 
                     "alfa=", (1 - confianza), "\n",  "alfa / 2 = ", 
                     (1 - confianza) /  2, sep = ""),  col = "black") +
  text(-2.5, 0.05, paste("0.025"),  col = "red")+
  text(2.5, 0.05, paste("0.025"),  col = "red")

# Para pruebas de una cola
# Izquierda confianza 90% 95% 99%
confianza <- c(0.90, 0.95, 0.99)
alfa <- 1 - confianza
z.critico.izq <- qnorm(alfa) # izquierda
z.critico.izq


z.critico.der <- qnorm(alfa, lower.tail = FALSE) # izquierda
z.critico.der

z.critico<- abs(z.critico.der)
z.critico

# Izquierda
par(mfrow=c(1, 3))
visualize.norm(stat = z.critico.izq[1],  
               section = "lower") +
  text(0, 0.2, paste(confianza[1] * 100, "%", "\n", 
                   "alfa=", (1 - confianza[1]), sep = ""), 
                      col = "black") +
  text(-2.5, 0.05, paste(1 - confianza[1]),  col = "red")

visualize.norm(stat = z.critico.izq[2],  
               section = "lower")+
  text(0, 0.2, paste(confianza[2] * 100, "%", "\n", 
                     "alfa=", (1 - confianza[2]), sep = ""), 
       col = "black") +
  text(-2.5, 0.05, paste(1 - confianza[2]),  col = "red")

visualize.norm(stat = z.critico.izq[3],  
               section = "lower")+
  text(0, 0.2, paste(confianza[3] * 100, "%", "\n", 
                     "alfa=", (1 - confianza[3]), sep = ""), 
       col = "black") +
  text(-2.5, 0.05, paste(1 - confianza[3]),  col = "red")


# Derecha
par(mfrow=c(1, 3))
visualize.norm(stat = z.critico.der[1],  
               section = "upper") +
  text(0, 0.2, paste(confianza[1] * 100, "%", "\n", 
                     "alfa=", (1 - confianza[1]), sep = ""), 
       col = "black") +
  text(-2.5, 0.05, paste(1 - confianza[1]),  col = "red")

visualize.norm(stat = z.critico.der[2],  
               section = "upper")+
  text(0, 0.2, paste(confianza[2] * 100, "%", "\n", 
                     "alfa=", (1 - confianza[2]), sep = ""), 
       col = "black") +
  text(-2.5, 0.05, paste(1 - confianza[2]),  col = "red")

visualize.norm(stat = z.critico.der[3],  
               section = "upper")+
  text(0, 0.2, paste(confianza[3] * 100, "%", "\n", 
                     "alfa=", (1 - confianza[3]), sep = ""), 
       col = "black") +
  text(-2.5, 0.05, paste(1 - confianza[3]),  col = "red")


# Prueba de hipotesis para una cola izquierda 
# datos
# SI SE CONOCE LA DESV. STD. DE POBLACION
alturas <- c(167, 167, 168, 168, 168, 169, 171, 172, 173, 175, 175, 175, 177, 182, 195)
media.p <- 175
desv.p <- 4
desv.m <- sd(alturas)
media.m <- mean(alturas)
n <- length(alturas)
confianza = 0.95
alfa <- 1 - confianza


z <- f.devolver.z.prueba(media.m = media.m, desv.p = desv.p, media.p = media.p, n = n)
z

z.critico <- qnorm(p = alfa)
z.critico


conclusion <- "Acepta Ho"
if (z < z.critico )
  conclusion = "Se rechaza Ho"
conclusion

# Visualmente
library(visualize)

par(mfrow=c(1, 1))
visualize.norm(stat = c(z.critico),  
               section = "lower") +
  text(0, 0.2, paste(confianza * 100, "%", "\n", 
                     "alfa=", (1 - confianza), sep = ""),  col = "black") +
  abline(v = z, col = 'red', lwd = 1, lty= 4)

# NO SE CONOCE LA DESV. STD. DE POBLACION
alturas <- c(167, 167, 168, 168, 168, 169, 171, 172, 173, 175, 175, 175, 177, 182, 195)
media.p <- 175 # Probar con 168
desv.p <- 4
desv.m <- sd(alturas)
media.m <- mean(alturas)
n <- length(alturas)
confianza = 0.95
alfa <- 1 - confianza

t <- f.devolver.t.prueba(media.m = media.m, media.p = media.p, desv.m = desv.m, n = 15)
t

t.critico <- qt(alfa, n-1)
t.critico

conclusion <- "Acepta Ho"
if (t < t.critico )
  conclusion = "Se rechaza Ho"
conclusion


library(visualize)

par(mfrow=c(1, 1))
visualize.t(stat = c(t.critico),  
               section = "lower", df = n-1) +
  text(0, 0.2, paste(confianza * 100, "%", "\n", 
                     "alfa=", (1 - confianza), sep = ""),  col = "black") +
  abline(v = z, col = 'red', lwd = 1, lty= 4)
  