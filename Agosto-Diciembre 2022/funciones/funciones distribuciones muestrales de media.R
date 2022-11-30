# Disribuciones muestrales


f_inicializar <- function(N, rango, n, q)  {
  N <- N # Cantidad de datos de población
  rango <- rango # rango # Rango de la variable
  n <- n  # Cantidad de datos de cada muestra
  q <- q # 100 o diferente ... # Cantidad de muestras m1, m2, m3

  
  poblacion <- data.frame(variable=sample(x = rango, size =  N, replace = TRUE))
  
  
  dist_muestral <- f_dist_muestral_q(poblacion = poblacion, n = n, q = q)
  
  hist <- f_histograma_dos_df(poblacion, dist_muestral)
  
  lista <- list(poblacion = poblacion, 
                dist_muestral = dist_muestral,
                hist = hist)
  
  
  return(lista)
}


# Genera una distribución muestral
# devuelve una tabla distribución muestral
f_dist_muestral_q <- function(poblacion, n, q) {
  muestras = as.list(NULL)
  m.muestras = NULL
  for (i in 1:q) {
    muestras[[i]] <- sample(x = poblacion$variable, size = q, replace = TRUE)
    
    m.muestras[i] <- mean(muestras[[i]])
  }
  
  df <- data.frame(muestras)
  df <- t(df)
  
  colnames(df) <- paste0("valores",1:q)
  rownames(df) <- paste0("M",1:q)
  
  dist_muestral <- data.frame(df[,1:3], "..."="...", df[,(q-2):q], medias.muestrales = m.muestras)
  
  return (dist_muestral)
}


f_histograma_dos_df <- function(poblacion, dist_muestral) {
  # variable1 <- colnames(poblacion[ncol(poblacion)])
  # variable2 <- colnames(dist_muestral[ncol(dist_muestral)])
  nombre_datos_1 <- substitute(poblacion)
  nombre_datos_2 <- substitute(dist_muestral)
  media.p <- round(mean(poblacion[,ncol(poblacion)]), 2)
  desv.std.p <- round(sd(poblacion[,ncol(poblacion)]), 2)
  
  media.m <- round(mean(dist_muestral[,ncol(dist_muestral)]), 2)
  desv.std.m <- round(sd(dist_muestral[,ncol(dist_muestral)]), 2)
  
  err_muestral.me <- round(media.p - media.m, 2)
  
  # Histograma con densidad
  g1 <- ggplot(poblacion, aes(x = variable)) + 
    geom_histogram(aes(y = ..density..),
                   colour = 1, fill = "blue") +
    labs(title = "Población",
         subtitle = paste("ME=", media.p, "; ds=", desv.std.p,  "; Err Mu. ME=",0),
         caption = "Fuente propia") +  
    
    geom_vline(xintercept = media.p, col='red') +
    geom_density(lwd = 1.2,
                 linetype = 2,
                 colour = 2)
#  g1 <- g1 + theme(
#    plot.title = element_text(color = "black", size = 14, face = "bold"),
#    plot.subtitle = element_text(color = "black",size=14),
#    plot.caption = element_text(color = "black", face = "italic", size=6)
#  )
  g2 <- ggplot(dist_muestral, aes(x = medias.muestrales)) + 
    geom_histogram(aes(y = ..density..),
                   colour = 1, fill = "green") +
    geom_vline(xintercept = media.m, col='red') +
    labs(title = paste("Distribución Muestral", nrow(dist_muestral), "muestras"),
         subtitle = paste("ME=", media.m, "; ds=", desv.std.m,  "; Err Mu. ME=",err_muestral.me),
         caption = "Fuente propia") +
    geom_density(lwd = 1.2,
                 linetype = 2,
                 colour = 2)
  #g2 <- g2 + theme(
  #  plot.title = element_text(color = "black", size = 10, face = "bold"),
  #  plot.subtitle = element_text(color = "black",size=7),
  #  plot.caption = element_text(color = "black", face = "italic", size=6)
  # )
  
  lista <- list(g1 = g1, g2 = g2)
  return(lista)
} 
