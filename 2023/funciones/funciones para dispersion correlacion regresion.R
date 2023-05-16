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
  
  media.x <- round(mean(x, na.rm = TRUE), 4)
  media.y <- round(mean(y, na.rm = TRUE), 4)
  
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

# 22-11-2022
# Genera gráfica dispersión tabla y estadísticos como 
# la medias de x e y, desviación std., covarianza, correlación
f_reg_lineal_simple <- function (datos) {
  
  tabla <- datos
  media_x <- round(mean(datos[, 1], na.rm = TRUE), 4)
  media_y <- round(mean(datos[, 2], na.rm = TRUE), 4)
  n <- nrow(datos)
  
  
  tabla <- cbind(tabla , media_x, media_y)
  tabla <- cbind(tabla, Xi_menos_media.x = tabla[,1] - media_x, 
                 Yi_menos_media.y = tabla[, 2] - media_y)
  tabla <- cbind(tabla, Xi_menos_media.x_cuad = tabla$Xi_menos_media.x^2, 
                 Yi_menos_media.y_cuad = tabla$Yi_menos_media.y^2)
  tabla <- cbind(tabla, Xi_menos_media.x_POR_Yi_menos_media.y = tabla$Xi_menos_media.x * tabla$Yi_menos_media.y)
  tabla <- rbind(tabla, round(apply(tabla, MARGIN = 2, sum), 4))
  
  row.names(tabla) <- c(1:(nrow(tabla) - 1), 'Sumatorias')
  columnas <- colnames(datos)
  columnas <- c(columnas, c("media.x", "media.y", 
                            "x_menos_media.x", "y_menos_media.y",
                            "x_menos_media.x_cuad", "y_menos_media.y_cuad",
                            "x_menos_media.x_cuad_POR_y_menos_media.y_cuad"))
 # columnas
  
  colnames(tabla) <- columnas
  
  desv.std_x <-sqrt(tabla[nrow(tabla), 7] / (n- 1))
  desv_std_y <-sqrt(tabla[nrow(tabla), 8] / (n- 1)) 
  covarianza <- tabla[nrow(tabla), 9] / (n- 1)
  r <- covarianza / (desv.std_x * desv_std_y)
  
  estadisticos <- data.frame(media_x = media_x, media_y = media_y,
                             desv.std_x, desv_std_y, 
                         covarianza, r)
  colnames(estadisticos) <- c("media_x", "media.y", "desv.std.x", "desv.std.y", 
                              "Covarianza", "Correlación")
  
  regresion <- list(tabla = tabla, 
                    n = n, 
                    dispersion = f_diag.dispersion(datos[,c(1,2)]),
                    estadisticos = estadisticos)
  
  return (regresion)
}

# 22-11-2022
# Genera la linea de tendencia lineal, recibe los datos y un modelo construído
f_linea_tendencia_reg_lineal <- function(datos, modelo) {
  ggplot(data = datos) + 
    geom_point(aes(x = datos[,1], y = datos[,2]), colour='blue') +
    geom_point(aes(x= mean(datos[,1]), y = mean(datos[,2])), col = 'green') +
    geom_line(aes( x = datos[,1], y = predict(modelo, datos)), color = "red") +
    xlab(colnames(datos[1])) + 
    ylab(colnames(datos[2])) + 
    ggtitle("Linea de tendencia sobre Conjunto de Datos")
}




