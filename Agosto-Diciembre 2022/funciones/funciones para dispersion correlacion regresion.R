# Librerías: 
library(dplyr)
library(mosaic)
library(ggplot2)  # Para gráficos
library(cowplot) #Imágenes en el mismo renglón
library("visualize")

options(scipen=999) # Notación normal

# Funciones para gráficos

# Construye diagrama de dispersión 
f_diag.dispersion <- function (datos) { 
  # datos <- data.frame(datos)
  nom.x = colnames(datos[1])
  nom.y = colnames(datos[2])
  x = datos[,1]
  y = datos[,2]
  
  media.x <- round(mean(x), 4)
  media.y <- round(mean(y), 4)
  
  ggplot() +
    geom_point(aes(x = x, y = y), col='red') +
    geom_vline(xintercept = media.x, col='blue') +
    geom_hline(yintercept = media.y, col='blue') +
    ggtitle(label = paste("Dispersión de ", nom.x, " y ", nom.y) , 
            subtitle = paste("Media ", nom.x, " =", media.x, 
                             " , ", "Media ", nom.y, "=", media.y))+
    xlab( nom.x)+
    ylab( nom.y)
  
}

f.prueba.significanncia.corr <- function(r, n) {
  t <- (r * sqrt(n-2))/ (sqrt(1 - r^2))
  t
}

f.diag.prueba.signif.corr <- function (t, t.signif,  n, confianza) {
  t <- abs(t)
  conf <- paste(as.character(confianza * 100), "%", sep = "")
  if (t.signif < -(t) | t.signif > t) {
    visualize.t(stat = c(-t, t), df = n-2, section = "tails") +
      text(0, 0.2, conf, col = "red") +
      text(0, 0.1, expression("t.signif fuera del intervalo; acepta H1"), col="red") +
      abline(v = t.signif, col = "red", lwd = 3, lty = 2)
  }  
  if(t.signif >= -(t) & t.signif <= t) {
    visualize.t(stat = c(-t, t), df = n-2, section = "tails") +
      text(0, 0.2, conf, col = "red") +
      text(0, 0.1, expression("t.signif dentro del intervalo; acepta H0"), col="red") +
      abline(v = t.signif, col = "red", lwd = 3, lty = 2)
    
  }  
  
}




