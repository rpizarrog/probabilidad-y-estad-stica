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


f_reg_lineal_simple <- function (datos) {
  
  tabla <- datos
  media_x <- mean(datos[, 1])
  media_y <- mean(datos[, 2])
  n <- nrow(datos)
  
  
  tabla <- cbind(tabla , media_x, media_y)
  tabla <- cbind(tabla, Xi_menos_media.x = tabla[,1] - media_x, 
                 Yi_menos_media.y = tabla[, 2] - media_y)
  tabla <- cbind(tabla, Xi_menos_media.x_cuad = tabla$Xi_menos_media.x^2, 
                 Yi_menos_media.y_cuad = tabla$Yi_menos_media.y^2)
  tabla <- cbind(tabla, Xi_menos_media.x_POR_Yi_menos_media.y = tabla$Xi_menos_media.x * tabla$Yi_menos_media.y)
  tabla <- rbind(tabla, round(apply(tabla, MARGIN = 2, sum), 4))
  
  row.names(tabla) <- c(1:(nrow(tabla) - 1), 'Sumatorias')
 # columnas <- colnames(datos)
#  columnas <- c(columnas, c("$\\bar{x}$", "$\\bar{y}$", "$x_i - \\bar{x}$", "$y_i - \\bar{y}$", "$(x_i - \\bar{x})^2$", "$(y_i - \\bar{y})^2$", "$(x_i - \\bar{x})^2\\cdot(y_i - \\bar{y})^2$"))
 # columnas
  
#  colnames(tabla) <- columnas
  
  desv.std_x <-sqrt(tabla[nrow(tabla), 7] / (n- 1))
  desv_std_y <-sqrt(tabla[nrow(tabla), 8] / (n- 1)) 
  covarianza <- tabla[nrow(tabla), 9] / (n- 1)
  r <- covarianza / (desv.std_x * desv_std_y)
  
  estadisticos <- data.frame(desv.std_x, desv_std_y, 
                         covarianza, r)
  colnames(estadisticos) <- c("desv.std.x", "desv.std.x", 
                              "Covarianza", "Correlación")
  
  regresion <- list(tabla = tabla, 
                    n = n, 
                    dispersion = f_diag.dispersion(datos[,c(1,2)]),
                    estadisticos = estadisticos)
  
  return (regresion)
}




