# Librerías
library(ggplot2)
library(gtools)
library(mosaic)
library(dplyr)
library(cowplot)
library(plotly) # gráficos interactivos
library(visualize)

# Variables globales par función de distribución NORMAL en plotDist()
x11 <- NULL
x12 <- NULL 


options(scipen=999) # PAra notación normal y no científica


prueba_devolver_plot <- function(a, b) {
  
  a <- 1:5
  b <- 6:10
  datos <- data.frame(a=a, b=b)
  g <- ggplot(datos, aes(x=a, y=b)) + 
    geom_point()
  lista <- list(datos = datos, g = g)
  lista
}



# 14 - Oct 2022
# Funciones para variables discretas

# Algunas funciones para variables discretas
# Función que devuelve tabla de distribución VE, Varianza y Desv. Std, recibe solo los casos
f.discretas.ve.v.sd <- function(casos, distribucion) {
  #library(ggplot2)
  # Inicializando valores
  n <- sum(casos)
  x = 0:(length(casos) - 1)
  
  # Calculando columnas
  prob_x <- casos / n
  acumulado <- cumsum(prob_x)
  x.prob_x <- x * prob_x
  
  
  datos <- data.frame(x, prob_x)  
  # Valor esperado
  VE <- sum(x.prob_x)
  
  # Varianza y Desviación Std
  x_menos_VE <- x - VE
  x_menos_VE.CUAD <- x_menos_VE ^ 2
  x_menos_VE.CUAD.prob_x <- x_menos_VE.CUAD * prob_x
  varianza <- sum(x_menos_VE.CUAD.prob_x)
  desv.std <- sqrt(varianza)
  
  if (distribucion == 1)
    t_dist <- "Probabilidad Uniforme Discreta"
  if (distribucion == 2)
    t_dist <- "Probabilidad Bernoulli; n experimentos"
  if (distribucion == 3)
    t_dist <- "Probabilidad Binomial"
  if (distribucion == 4)
    t_dist <- "Probabilidad Poisson"
  if (distribucion == 5)
    t_dist <- "Probabilidad Hipergeométrica"
  
  g_barra <- ggplot(data = datos, aes(x = x, y=prob_x , fill=x)) +
    geom_bar(stat="identity") +
    geom_vline(xintercept = VE, color = 'red', linetype = "dashed", size = 1) +
    geom_vline(xintercept = VE - desv.std, color = 'blue', linetype = "dashed", size = 1) +
    geom_vline(xintercept = VE + desv.std, color = 'blue', linetype = "dashed", size = 1) +
    labs(title=t_dist, subtitle = paste("VE", round(VE, 2), "± Desv. Std", round(desv.std, 2)), x="Variable X", y="Probabilidad")
  
  
  tabla <- data.frame(x, casos, prob_x,
                      acumulado, x.prob_x,
                      VE,
                      x_menos_VE,
                      x_menos_VE.CUAD,
                      x_menos_VE.CUAD.prob_x
  )
  estadisticos <- list(tabla = tabla, x = x, N = n,
                       VE = VE, varianza = varianza, desv.std = desv.std, 
                       g_barra = g_barra)
  
  estadisticos
  
  
}


# Similar función a la anterior solo que recibe también los valores de la variable discreta
# Algunas veces los valores de la variable discreta comienzan por debajo de 0, o no inician en cero
f.discretas.ve.v.sd.val.disc <- function(discretas, casos, distribucion) {
  #library(ggplot2)
  # Inicializando valores
  n <- sum(casos)
  x = discretas
  
  # Calculando columnas
  prob_x <- casos / n
  acumulado <- cumsum(prob_x)
  x.prob_x <- x * prob_x
  

  datos <- data.frame(x, prob_x)  
  
  # Valor esperado
  VE <- sum(x.prob_x)
  
  # Varianza y Desviación Std
  x_menos_VE <- x - VE
  x_menos_VE.CUAD <- x_menos_VE ^ 2
  x_menos_VE.CUAD.prob_x <- x_menos_VE.CUAD * prob_x
  varianza <- sum(x_menos_VE.CUAD.prob_x)
  desv.std <- sqrt(varianza)
  
  if (distribucion == 1)
    t_dist <- "Probabilidad Uniforme Discreta"
  if (distribucion == 2)
    t_dist <- "Probabilidad Bernoulli; n experimentos"
  if (distribucion == 3)
    t_dist <- "Probabilidad Binomial"
  if (distribucion == 4)
    t_dist <- "Probabilidad Poisson"
  if (distribucion == 5)
    t_dist <- "Probabilidad Hipergeométrica"
  
  g_barra <- ggplot(data = datos, aes(x = x, y=prob_x , fill=x)) +
    geom_bar(stat="identity") +
    geom_vline(xintercept = VE, color = 'red', linetype = "dashed", size = 1) +
    geom_vline(xintercept = VE - desv.std, color = 'blue', linetype = "dashed", size = 1) +
    geom_vline(xintercept = VE + desv.std, color = 'blue', linetype = "dashed", size = 1) +
    labs(title=t_dist, subtitle = paste("VE", round(VE, 2), "± Desv. Std", round(desv.std, 2)), x="Variable X", y="Probabilidad")
  
  
  tabla <- data.frame(x, casos, prob_x,
                      acumulado, x.prob_x,
                      VE,
                      x_menos_VE,
                      x_menos_VE.CUAD,
                      x_menos_VE.CUAD.prob_x
  )
  estadisticos <- list(tabla = tabla, x = x, N = n,
                       VE = VE, varianza = varianza, desv.std = desv.std, 
                       g_barra = g_barra)
  
  estadisticos
  
  
}

f_Bernoulli_all <- function(x, p, n) {
  
  q = (1-p)
  tabla <- data.frame(x = x, p = c(p, 1-p))
  
  
  # Para un solo experimeto
  acumulado <- cumsum(tabla$p)
  
  tabla <- cbind(tabla, acumulado)
  VE = p
  varianza = p * q
  desv.std = sqrt(varianza)
  
  t_dist <- "Distribución de Bernoulli. Un solo experimento"
  
  g_barra <- ggplot(data = tabla, aes(x = x, y=p , fill=x)) +
    geom_bar(stat="identity") +
    geom_vline(xintercept = VE, color = 'red', linetype = "dashed", size = 1) +
    geom_vline(xintercept = VE - desv.std, color = 'blue', linetype = "dashed", size = 1) +
    geom_vline(xintercept = VE + desv.std, color = 'blue', linetype = "dashed", size = 1) +
    labs(title=t_dist, subtitle = paste("VE", round(VE, 2), "± Desv. Std", round(desv.std, 2)), x="Variable X", y="Probabilidad")
  
  # Simulación de varios experimentos Bernoulli
  prob <- NULL
  for (xi in 0:n) {
    Cn <- factorial(n) / (factorial(xi) * factorial(n-xi))
    prob[xi+1] = round(Cn * p^(xi) * q ^ (n-xi), 4)
  }
  
  casos <- round(prob * 1000) # Simular 1000 casos lanzamientos distribuidos
  
  # Reutilizar la función f.discretas.ve.v.sd.val.disc()
  # que construye una tabla de distribución discreta y un diagrama de barra
  resultado <- f.discretas.ve.v.sd.val.disc(discretas = 0:n, casos = casos, distribucion = 2)
  
  estadisticos <- list(tabla = tabla, x = x, n = n,
                       VE = VE, varianza = varianza, desv.std = desv.std, 
                       g_barra = g_barra,
                       resultado = resultado)
  
  estadisticos
}



# Funciones para distribuciones de probabilidad
# Funciones preparadas para DISTRIBUCION UNIFORME
plotunif <- function(x, min = 0, max = 1, lwd = 1, col = 1, ...) {
  
  # Rejilla de valores del eje X
  if (missing(x)) {
    x <- seq(min - 0.5, max + 0.5, 0.01)
  }
  
  if(max < min) {
    stop("'min' debe ser menor que 'max'")
  }
  
  plot(x, dunif(x, min = min, max = max),
       xlim = c(min - 0.25, max + 0.25), type = "l",
       lty = 0, ylab = "f(x)", ...) 
  segments(min, 1/(max - min), max, 1/(max - min), col = col, lwd = lwd)
  segments(min - 2, 0, min, 0, lwd = lwd, col = col)
  segments(max, 0, max + 2, 0, lwd = lwd, col = col)
  points(min, 1/(max - min), pch = 19, col = col)
  points(max, 1/(max - min), pch = 19, col = col)
  segments(min, 0, min, 1/(max - min), lty = 2, col = col, lwd = lwd)
  segments(max, 0, max, 1/(max - min), lty = 2, col = col, lwd = lwd)
  points(0, min, pch = 21, col = col, bg = "white")
  points(max, min, pch = 21, col = col, bg = "white")
}

unif_area <- function(min = 0, max = 1, lb, ub, col = 1, acolor = "lightgray", ...) {
  x <- seq(min - 0.25 * max, max + 0.25 * max, 0.001) 
  
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  if(max < min) {
    stop("'min' debe ser menor que 'max'")
  }
  
  x2 <- seq(lb, ub, length = 1000) 
  plot(x, dunif(x, min = min, max = max),
       xlim = c(min - 0.25 * max, max + 0.25 * max), type = "l",
       ylab = "f(x)", lty = 0, ...)   
  
  y <- dunif(x2, min = min, max = max)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor, lty = 0)
  segments(min, 1/(max - min), max, 1/(max - min), lwd = 2, col = col)
  segments(min - 2 * max, 0, min, 0, lwd = 2, col = col)
  segments(max, 0, max + 2 * max, 0, lwd = 2, col = col)
  points(min, 1/(max - min), pch = 19, col = col)
  points(max, 1/(max - min), pch = 19, col = col)
  segments(min, 0, min, 1/(max - min), lty = 2, col = col, lwd = 2)
  segments(max, 0, max, 1/(max - min), lty = 2, col = col, lwd = 2)
  points(0, min, pch = 21, col = col, bg = "white")
  points(max, min, pch = 21, col = col, bg = "white")
}

# función para distribuciones
# 27 Oct 2021 Actualizado

# library(gtools)


# Función que devuelve la probabilidad conforme y de acuerdo a la 
# la fórmula de distribución binomial
# Recibe tres parámetros: 
# los valores de x, e valor de n y la probabilidad de éxito
# Devuelve las probabilidades para cada valor de la variable aleatoria discreta
f.prob.binom <- function (x,n,exito) {
  fracaso <- 1 - exito
  prob <- (factorial(n) / 
             (factorial(x) * factorial(n-x)))  * ((exito^x) * (fracaso ^ (n-x)))
  prob
}

# 14 Oct 2022
# Función para tabla binomial. Devuelve tabla binomial
f.tabla_binom <- function(n, exito) {
  tabla <- data.frame(x = 0:n,
                      f.x = dbinom(x = 0:n, size = n, prob = exito),
                      F.x = pbinom(q = 0:n, size = n, prob = 0.80))
  tabla
}

# 14 Abr 2023
# Función para devuelve tabla binomial, VE Varianza y Desv. Std
# de una distribución binomial. 
f.binom.all <- function(n, exito){
  tabla <- data.frame(x = 0:n,
                      f.x = dbinom(x = 0:n, size = n, prob = exito),
                      F.x = pbinom(q = 0:n, size = n, prob = exito))
  tabla
  
  # Valor esperado
  VE <- n * exito # n * p
  
  # Varianza n * p * (n - 1)
  varianza <- n * exito * (1 - exito)
  desv.std <- sqrt(varianza)
  
  g.dens <- plotDist(dist = "binom", 
                     params = c(n, exito), 
                     xlim = c(-1, n+1), 
                     kind = "d",
                     xlab ="X's", 
                     ylab = "Probabilidad", 
                     main='Distribución Binomial')
  
  g.hist <- plotDist(dist = "binom", 
                     params = c(n, exito), 
                     xlim = c(-1, n+1), 
                     kind = "h", 
                     xlab ="X's", 
                     ylab = "Probabilidad", 
                     main='Distribución Binomial')
  
  g.acum <- plotDist(dist = "binom", 
                     params = c(n, exito), 
                     xlim = c(-1, n+1), 
                     kind = "c", 
                     xlab ="X's", 
                     ylab = "Prob Acumulada", 
                     main='Distribución Binomial',
                     sub = "")
  
  t_dist <- 'Distribución Binomial'
  g_barra <- ggplot(data = tabla, aes(x = x, y=f.x , fill=x)) +
    geom_bar(stat="identity") +
    geom_vline(xintercept = VE, color = 'red', linetype = "dashed", size = 1) +
    geom_vline(xintercept = VE - desv.std, color = 'blue', linetype = "dashed", size = 1) +
    geom_vline(xintercept = VE + desv.std, color = 'blue', linetype = "dashed", size = 1) +
    labs(title=t_dist, subtitle = paste("VE", round(VE, 2), "± Desv. Std", round(desv.std, 2)), x="Variable X", y="Probabilidad")
  
  g.text <- ggplot(data = tabla) +
    geom_col(aes(x = x, y = f.x), fill='blue') + 
    ggtitle(label = "Distribución binomial",subtitle = paste("ve=", VE, ";", 
                                                             "var=", round(varianza, 2), ";",
                                                             "sd=", round(desv.std, 2))
    )
  g.hist.plotly <- plot_ly(
    x = c(tabla$x),
    y = c(tabla$f.x),
    type = "bar") %>%
    layout(title = "Distribución binomial",
           xaxis = list(title = "x's"), 
           yaxis = list(title = "Función de Prob. f(X)")
    )
  
  
  g.acum.plotly <- plot_ly(
    x = c(tabla$x),
    y = c(tabla$F.x),
    type = "scatter" ,
    mode = "lines") %>%
    layout(title = "Distribución binomial",
           xaxis = list(title = "x's"), 
           yaxis = list(title = "Función Acumulada F(X)")
    )
  

  distribucion <- list(tabla = tabla, VE = VE, 
                       varianza = varianza, desv.std = desv.std, 
                       g.dens = g.dens, 
                       g.hist = g.hist,
                       g.acum = g.acum,
                       g.text = g.text,
                       g_barra = g_barra,
                       g_barra = g_barra,
                       g.hist.plotly = g.hist.plotly,
                       g.acum.plotly = g.acum.plotly,
                       g_all = f.hist.dens.discreta(tabla))
  
}

# 15 OCT 2022
# Devolver histograma, histograma con densidad y acumulado
f.hist.dens.discreta <- function(datos) {
  library(ggplot2)
  
  g1 <- ggplot(data = datos) +
    geom_col(aes(x = x, y = f.x), fill='blue') +
    ggtitle(label = "Histograma")
  
  casos <- NULL
  for(r in 1:nrow(datos)) {
    casos <- c(casos, c(rep(datos$x[r], round(datos[r,2] * 100))))
  }
  
  g2 <- ggplot() +
    geom_col(data = datos, aes(x = x, y = f.x)) +
    geom_density(aes(x = casos), color = 'red') +
    ggtitle(label = "Histograma y Densidad")
  
  g3 <- ggplot(data = datos) +
    geom_line(aes(x = x, y = F.x), color = 'red') +
    geom_point(aes(x = x, y = F.x), color = 'blue') +
    ggtitle(label = "Función acumulada")
  
  
  lista <- list(hist = g1, dens = g2, acum = g3)
  
  
  return(lista)
  
}


# Función de distribución hipergeométrica
# Recibe estos parámetros:
# N Total de elementos de la población
# n Elementos de la muestra o ensayos
# r número de elementos considerados como éxito
# x Valores que puede tener la variable aleatoria discreta
# Actualización 17 Oct 2022
f.prob.hiper <- function (x, poblacion, muestra, exitosos) {
  N <- poblacion
  n <- muestra
  r <- exitosos
  
  numerador <- (factorial(r) / (factorial(x) * (factorial(r-x)))) * (factorial(N-r) / (factorial(n-x) * (factorial((N-r)-(n-x)))))
  denominador<- factorial(N) / (factorial(n) * factorial(N-n))
  
  prob <- numerador / denominador
  prob
  
}

# 18 OCT 2022
# Función para devolver una lista con tabla de distribución valor esperado
# varianza desviación estándar y algunos gráficos 
# de una distribución hipergeométrica

f.hiper.all <- function(exitosos, muestra, poblacion){
  # n = número de ensayos exitosos
  # N Tamaño de la población de cada N
  # r o k Tamaño de la muestra extraída de N
  
  N <- poblacion 
  n <- muestra 
  r <- exitosos
  x <- 0:muestra
  
  VE <- n * (r/N)
  VE
  
  varianza <- VE * (1 - r/N) * ((N-n) / (N-1))
  varianza
  
  desv.std <- sqrt(varianza)
  
  
  tabla <- data.frame(x=x, f.x = round(dhyper(x = x, m = r, n = N - r, k = n),8), 
                      F.x = round(phyper(q = x, m = r, n = N - r, k = n), 8))
  tabla
  
  
  g.dens <- plotDist(dist = "hyper", 
                     kind = "density",
                     params = c(m = r, n = N - r, k=n), 
                     xlim = c(-1, n+1), 
                     xlab ="X's", 
                     ylab = "Probabilidad. f(x)", 
                     main='Distribución Hipergeométrica',
                     sub = paste("VE:",VE, "; ", "Var:",round(varianza,4), "; ", "ds:",round(desv.std, 4)))
  
  
  g.hist <- plotDist(dist = "hyper", 
                     params = c(m = r, n = N - r, k=n), 
                     xlim = c(-1, n+1), 
                     kind = "h", 
                     xlab ="X's", 
                     ylab = "Probabilidad. f(x)", 
                     main='Distribución Hipergeométrica',
                     sub = paste("VE:",VE, "; ", "Var:",round(varianza,4), "; ", "ds:",round(desv.std, 4)))
  
  g.acum <- plotDist(dist = "hyper", 
                     params = c(m = r, n = N - r, k=n), 
                     xlim = c(-1, n+1), 
                     kind = "c", 
                     xlab ="X's", 
                     ylab = "Probabilidad Acumulada. F(x)", 
                     main='Distribución Hipergeométrica',
                     sub = paste("VE:",VE, "; ", "Var:",round(varianza,4), "; ", "ds:",round(desv.std, 4)))
  
  g.text <- ggplot(data = tabla) +
    geom_col(aes(x = x, y = f.x), fill='blue') + 
    ggtitle(label = "Distribución Hipergeométrica",subtitle = paste("ve=", VE, ";", 
                                                                    "var=", round(varianza, 2), ";",
                                                                    "sd=", round(desv.std, 2))
    )
  
  t_dist <- 'Distribución Hipergeométrica'
  g_barra <- ggplot(data = tabla, aes(x = x, y=f.x , fill=x)) +
    geom_bar(stat="identity") +
    geom_vline(xintercept = VE, color = 'red', linetype = "dashed", size = 1) +
    geom_vline(xintercept = VE - desv.std, color = 'blue', linetype = "dashed", size = 1) +
    geom_vline(xintercept = VE + desv.std, color = 'blue', linetype = "dashed", size = 1) +
    labs(title=t_dist, subtitle = paste("VE", round(VE, 2), "± Desv. Std", round(desv.std, 2)), x="Variable X", y="Probabilidad")
  
  g.hist.plotly <- plot_ly(
    x = c(tabla$x),
    y = c(tabla$f.x),
    type = "bar") %>%
    layout(title = "Distribución Hipergeométrica",
           xaxis = list(title = "x's"), 
           yaxis = list(title = "Función de Prob. f(X)")
    )
  
  
  g.acum.plotly <- plot_ly(
    x = c(tabla$x),
    y = c(tabla$F.x),
    type = "scatter" ,
    mode = "lines") %>%
    layout(title = "Distribución Hipergeométrica",
           xaxis = list(title = "x's"), 
           yaxis = list(title = "Función Acumulada F(X)")
    )  
  
  
  distribucion <- list(tabla = tabla, VE = VE, 
                       varianza = varianza, desv.std = desv.std,
                       g.dens = g.dens, g.hist = g.hist, 
                       g_barra = g_barra, 
                       g.acum = g.acum, g.text = g.text,
                       g.hist.plotly = g.hist.plotly, g.acum.plotly = g.acum.plotly,
                       g_all = f.hist.dens.discreta(tabla))
  
  return(distribucion)
}



# Función que devuelve el valor esperado de una distribución hipergeométrica
f.va.hiper <- function (n, r, N) {
  VE <- n * (r/N)
  VE
}

# Función que devuelve la varianza de una distribución hipergeométrica
f.varianza.hiper <- function(VE, n, r, N)  {
  varianza <- VE * (1 - r/N) * ((N-n) / (N-1))
  varianza
}

# Función de distribución de Poisson conforme a la Fórmula
f.prob.poisson <- function (media, x) {
  e <- exp(1)
  prob <- media ^ x * e ^(-media) / factorial(x)
  prob
  
  
}


# 19 OCT 2022
# Función para devolver una lista con tabla de distribución valor esperado
# varianza desviación estándar y algunos gráficos 
# de una distribución Poisson

f.poisson.all <- function(media) {
  VE <- media
  
  varianza <- VE 
  
  desv.std <- sqrt(varianza)
  
  n = media * 2  # Es infinito.... pero e deja hasta un tope
  x <- 0:n
  
  tabla <- data.frame(x=x, f.x = round(dpois(x = x, lambda = media),8), 
                      F.x = round(ppois(q = x, lambda = media),8))
  tabla
  
  g.dens <- plotDist(dist = "pois", 
                     kind = "density",
                     params = c(media), 
                     xlim = c(-1, n+1), 
                     xlab ="X's", 
                     ylab = "Probabilidad. f(x)", 
                     main='Distribución Poisson',
                     sub = paste("VE:",VE, "; ", "Var:",round(varianza,4), "; ", "ds:",round(desv.std, 4)))
  
  
  g.hist <- plotDist(dist = "pois", 
                     params = c(media), 
                     xlim = c(-1, n+1), 
                     kind = "h", 
                     xlab ="X's", 
                     ylab = "Probabilidad. f(x)", 
                     main='Distribución Poisson',
                     sub = paste("VE:",VE, "; ", "Var:",round(varianza,4), "; ", "ds:",round(desv.std, 4)))
  
  g.acum <- plotDist(dist = "pois", 
                     params = c(media), 
                     xlim = c(-1, n+1), 
                     kind = "c", 
                     xlab ="X's", 
                     ylab = "Probabilidad Acumulada. F(x)", 
                     main='Distribución Poisson',
                     sub = paste("VE:",VE, "; ", "Var:",round(varianza,4), "; ", "ds:",round(desv.std, 4)))
  
  g.text <- ggplot(data = tabla) +
    geom_col(aes(x = x, y = f.x), fill='blue') + 
    ggtitle(label = "Distribución Poisson",
            subtitle = paste("ve=", VE, ";", "var=", round(varianza, 2), ";", "sd=", round(desv.std, 2))
    )
  
  t_dist <- 'Distribución Poisson'
  g_barra <- ggplot(data = tabla, aes(x = x, y=f.x , fill=x)) +
    geom_bar(stat="identity") +
    geom_vline(xintercept = VE, color = 'red', linetype = "dashed", size = 1) +
    geom_vline(xintercept = VE - desv.std, color = 'blue', linetype = "dashed", size = 1) +
    geom_vline(xintercept = VE + desv.std, color = 'blue', linetype = "dashed", size = 1) +
    labs(title=t_dist, subtitle = paste("VE", round(VE, 2), "± Desv. Std", round(desv.std, 2)), x="Variable X", y="Probabilidad")
  
  
  g.hist.plotly <- plot_ly(
    x = c(tabla$x),
    y = c(tabla$f.x),
    type = "bar") %>%
    layout(title = "Distribución Poisson",
           xaxis = list(title = "x's"), 
           yaxis = list(title = "Función de Prob. f(X)")
    )
  
  
  g.acum.plotly <- plot_ly(
    x = c(tabla$x),
    y = c(tabla$F.x),
    type = "scatter" ,
    mode = "lines") %>%
    layout(title = "Distribución Poisson",
           xaxis = list(title = "x's"), 
           yaxis = list(title = "Función Acumulada F(X)")
    ) 
  
  distribucion <- list(tabla = tabla, VE = VE, 
                       varianza = varianza, desv.std = desv.std,
                       g.dens = g.dens, g.hist = g.hist, g.acum = g.acum, g.text = g.text,
                       g_barra = g_barra,
                       g.hist.plotly = g.hist.plotly, g.acum.plotly = g.acum.plotly,
                       g_all = f.hist.dens.discreta(tabla))
  
  return(distribucion)
  
}

# Devuelve el valor de t para una distribución T Student
f.devolver.t <- function(media.muestra, media.pob, desv.muestra, n) {
  t <- (media.muestra - media.pob) / (desv.muestra / sqrt(n))
  t
}

f.t.student.std <- function(Z, V) {
  dens <- Z / (V / sqrt(V))
  dens
}

f.normal.dens <- function(desv, x, media) {
  numerador <- exp(1)^(-(x - media)^2 / (2 * desv^2))
  denominador <- desv * sqrt(2 * pi)
  prob <- numerador / denominador
  prob
  
  # Es lo mismo que dnorm(x = x, mean = media, sd = desv)
}



# 02 Noviembre 2022
# Función para devolver probabilidades de una distribución normal
# Recibe media, desviación 
# Recibe x1 y x2 que son los valores de la variable discreta x
#  y representan el intervalo par calcular la probabilidad

f.normal.all <- function(media, desv.std, x1, x2, tipo) {
  
  # Crea variables globales que se usa en group del plotDist()
  
  x11 <<- x1
  x12 <<- x2
  
  prob <- NULL
  prob.str <- NULL
  # Cola izquierda se utiliza x1
  if (tipo == 1) {
    prob <- round(pnorm(q = x1, mean = media, sd = desv.std), 6)
    prob.str <- "(Cola izquierda. Prob ="
    prob.str <- paste(prob.str, prob, ")")
  }
  # Cola derecha se utiliza x2
  if (tipo == 2) {
    prob <- round(pnorm(q = x2, mean = media, sd = desv.std, lower.tail = FALSE), 6)
    prob.str <- "(Cola derecha. Prob = "
    prob.str <- paste(prob.str, prob, ")")
  }
  if (tipo == 3) {
    prob.str <- "(Intervalo. Prob = "
    if (is.null(x1) | is.null(x2)) {
      prob.str <- "No se puede calcular, no se capturó intervalo x2 y x1"
      prob <- NULL
    } else {
      prob <- round((pnorm(q = x2, mean = media, sd = desv.std) - pnorm(q = x1, mean = media, sd = desv.std)), 6)
      prob.str <- paste(prob.str, prob, ")")
    }
    
  }
  #prob
  
  
  
  # Se construye un conjunto de datos aleatorios
  #  generados con características de distribución normal
  set.seed(2022)
  x <- sort(rnorm(n = 1000, mean = media, sd = desv.std))
  datos <- data.frame(x=x, 
                      f.x = dnorm(x = x, mean = media, sd = desv.std))
  
  titulo <- "Distribución normal"
  subtitulo <- paste("Media = ", media, "; Desviación Std.=", desv.std, 
                     "; valores de x de ", round(min(datos$x), 2), " hasta ",
                     round(max(datos$x), 2))
  
  # histograma con ggplot() geom_hist()
  g_hist <- ggplot(data = datos) +
    geom_histogram(aes(x = x), fill="lightblue", color='gray') +
    geom_vline(xintercept = media, color='red', linetype = "dashed", size = 1) +
    geom_vline(xintercept = media - desv.std, color = 'blue', linetype = "dashed", size = 1) +
    geom_vline(xintercept = media + desv.std, color = 'blue', linetype = "dashed", size = 1) +
    ggtitle(label = "Histograma", 
            subtitle = subtitulo)
  # g_hist
  
  #  ggplot()
  g.gauss.gg <- ggplot(data = datos, aes(x, f.x) ) +
    geom_point(colour = "red") +
    geom_line(colour = 'blue') +
    geom_vline(xintercept = media, col='red') +
    ggtitle(label = titulo, 
            subtitle = subtitulo)
  
  
  # plotDist()
  if (tipo == 1) {
    g.plotDist <- plotDist(dist = "norm", mean = media, sd = desv.std, 
                           groups = x <= x11, type = "h", 
                           xlab ="x's", 
                           ylab = "Densidad f(x)", 
                           main=titulo,
                           sub = prob.str)
    
    
  }
  
  if (tipo == 2) {
    g.plotDist <- plotDist(dist = "norm", mean = media, sd = desv.std, 
                           groups = x >= x12, type = "h", 
                           xlab ="x's", 
                           ylab = "Densidad f(x)", 
                           main = titulo,
                           sub = prob.str)
    
    
  }
  
  
  if (tipo == 3) {
    g.plotDist <- plotDist(dist = "norm", mean = media, sd = desv.std, 
                           groups = x >= x11 & x <= x12, type = "h", 
                           xlab ="x's", 
                           ylab = "Densidad f(x)", 
                           main=titulo,
                           sub = prob.str)
    
    
  }
  # Plot_ly()
  g.gauss.plotly <- plot_ly( 
    x = datos$x, y = datos$f.x,
    type = "scatter" ,
    mode = "lines") %>%
    layout(title = titulo, 
           xaxis = list(title = "x's"), 
           yaxis = list(title = "Densidad f(x)"))
  
  
  
  
  distribucion <- list(prob = prob, 
                       prob.str = prob.str,
                       g_hist = g_hist,
                       g.gauss.gg = g.gauss.gg,
                       g.plotDist = g.plotDist,
                       g.gauss.plotly = g.gauss.plotly)
  
  
  
  
  return(distribucion)
  
}


# 26 Abril 2022
# Distribución exponencial
# Recibe media e intervalo, devuelve una lista con varios estadísticos y gráficos
f_exponencial_all <- function(media, intervalo, tipo = 1) {
  
  # tipo == 1 izquierda
  # tipo == 2 derecha
  # tipo == 3 intervalo
    
    lambda = 1/media
    
    a <- intervalo[1]
    b <- intervalo[2]
    
    # x's
    x = seq(from = 0, to=(media*3), by=0.001)
    # Densidad
    f.x <- round(dexp(x = x, rate = lambda), 6)
    f.x
    
    # Acumulada
    F.x <- round(pexp(q = x, rate =  lambda), 6)
    F.x
    
    datos <- data.frame(x = x, f.x = f.x, F.x = F.x)

    VE <- round(1 / lambda, 4)
    varianza = round(1 / (lambda ^2), 4)
    desv.std = round(sqrt(varianza), 4)
    

  # Cola izquierda
  if (tipo == 1) {
      prob <- round(pexp(q = b, rate = lambda), 6)
      datos <- data.frame(x = x, f.x = f.x, F.x = F.x)
      datos <- cbind(datos, p = ifelse(datos$x <= b , 'p', 'q'))
      datos$p <- as.factor(datos$p)
      g_curva <- ggplot(data = datos, aes(x = x, y = f.x, fill = p))  +
        geom_area(alpha = 0.5, position = "identity") +
        scale_fill_manual(values = c("red", "dodgerblue4")) +
        labs(title=paste("Probabilidad F(x ≤", b, ")=",prob ), 
             subtitle = paste("VE", VE, "Desv.Std=", desv.std)) +
        xlab("x's") +
        ylab("f(x)")

  }
    

    # Cola derecha
    if (tipo == 2) {
      prob <- round(pexp(q = b, rate = lambda, lower.tail = FALSE), 6)
      datos <- data.frame(x = x, f.x = f.x, F.x = F.x)
      datos <- cbind(datos, p = ifelse(datos$x >= b , 'q', 'p'))
      datos$p <- as.factor(datos$p)
      g_curva <- ggplot(data = datos, aes(x = x, y = f.x, fill = p))  +
        geom_area(alpha = 0.5, position = "identity") +
        scale_fill_manual(values = c("dodgerblue4", "red")) +
        labs(title=paste("Probabilidad F(x ≥", b, ")=",prob ), 
             subtitle = paste("VE", VE, "Desv.Std=", desv.std)) +
        xlab("x's") +
        ylab("f(x)")
      
    }      
    
  # Intervalo
  if (tipo == 3) {
    prob <- round(pexp(q = b, rate = lambda) - pexp(q = a, rate = lambda), 6)
    datos <- data.frame(x = x, f.x = f.x, F.x = F.x)
    datos <- cbind(datos, p = ifelse(datos$x >= a & datos$x <= b , 'p', 'q'))
    datos$p <- as.factor(datos$p)
    
    g_curva<- ggplot(data = datos, aes(x = x, y = f.x, fill = p))  +
        geom_area(alpha = 0.5, position = "identity") +
        scale_fill_manual(values = c("red", "dodgerblue4")) +
        labs(title=paste("Densidad F(",a , "≤ x ≤", b, ")=",prob ), 
             subtitle = paste("VE", VE, "Desv.Std=", desv.std)) +
        xlab("x's") +
        ylab("f(x)")
      
    }
    
    
    distribucion = list(VE = VE, varianza = varianza, desv.std = desv.std, 
                        prob = prob,
                        g_curva = g_curva)
    return(distribucion)  
}
  


# Función para devolver el valor de z
f.devolver.z <- function(x, media, desv) {
  z <- (x - media) / desv
  z
}

f.devolver.z.prueba <- function(media.m, desv.p, media.p, n) {
  z <- (media.m - media.p) / (desv.p / sqrt(n))
  z
}


f.devolver.t.prueba <- function(media.m, desv.m, media.p, n) {
  t <- (media.m - media.p) / (desv.m / sqrt(n))
  t
}



# Función para devolver Z para Intervalo de Confianza
f.z.int.conf <- function (confianza) {
  alfa = 1 - confianza
  #alfa
  
  v.critico <- 1 - (alfa / 2)
  #v.critico
  
  z <- qnorm(v.critico)
  z
}


# Función para devolver el intervalo de confianza Z
# a cuatro posiciones decimales
f.intervalo.confianza <- function (media, desv, confianza, n) {
  li <- media - f.z.int.conf(confianza) * desv / sqrt(n) 
  ls <- media + f.z.int.conf(confianza) * desv / sqrt(n) 
  
  round(c(li, ls),4)
}
# Función para devolver el intervalo de confianza Z
# a cuatro posiciones decimales
f.intervalo.confianza.z <- function (media, desv, confianza, n) {
  li <- media - f.z.int.conf(confianza) * desv / sqrt(n) 
  ls <- media + f.z.int.conf(confianza) * desv / sqrt(n) 
  
  round(c(li, ls),4)
}



# Función para devolver t para Intervalo de Confianza
# Recibe confianza en valor relativo, n y tipo que puede ser "izq", "der", "ambos"
f.t.int.conf <- function (confianza, n, cola) {
  gl <- n-1
  if (cola == "izq" ) {
    alfa = 1 - confianza
    t <- qt(p = alfa, df = gl)
  }
  if (cola == "der" ) {
    alfa = 1 - confianza
    t <- qt(p = alfa, df = gl, lower.tail = FALSE)
  }
  
  if (cola == "ambas" ) {
    alfa = (1 - confianza) / 2
    t <- qt(p = alfa, df = gl)
    t = abs(t)
  }

  return (t)
}

# Función para devolver el intervalo de confianza t
# a cuatro posiciones decimales
# recibe media, desv, confianza, n y tipo [1 = "izq", 2 = "der", 3 = "ambos"]
f.intervalo.confianza.t <- function (media, desv, confianza, n, tipo) {
  if (tipo == 1) { # cola izquierda
    li <- media - (f.t.int.conf(confianza, n, cola = "izq") * desv / sqrt(n)) 
    ls <- Inf
  }
  if (tipo == 2) { # cola derecha
    li <- -Inf
    ls <- media + f.t.int.conf(confianza, n, cola = "der") * desv / sqrt(n)
  }
  if (tipo == 3) { # ambas colas
    li <- media - f.t.int.conf(confianza, n, cola = "ambas") * desv / sqrt(n)
    ls <- media + f.t.int.conf(confianza, n, cola = "ambas") * desv / sqrt(n)
  }
  
  
  round(c(li, ls),4)
}

# Función para graficar intervalo de confianza
# No funciona
f.graf.intervalo.confianza <- function (datos) {
  datos <- data.frame(valores = datos)
  g <- ggplot(data = datos) +
    geom_point(aes(x = f.devolver.z(x = valores, media = mean(valores), desv = sd(valores)), 
                   y = pnorm(f.devolver.z(x = valores, media = mean(valores), desv = sd(valores)))))
  g
}



# Devuelve la probabilidad de una variable discreta
# Recibe una tabla de distribución
# recibe el valor de la variable discreta
# Recibe el tipo: 
#   = 0; 
#   < 1; 
#   > 2; 
#   <= 3; 
#   >= 4 
f.prob.discr <- function(datos, discreta, tipo) {
  if (tipo == 0) { # == 
    salida <- filter(datos, x == discreta) %>%
      select (f.prob)    
  }
  if (tipo == 1) { # <
    salida <- filter(datos, x == discreta - 1) %>%
      select (f.acum) 
  }
  if (tipo == 2) { # >
    salida <- filter(datos, x == discreta) %>%
      select (f.acum)
    salida <- 1 - salida
  }
  if (tipo == 3) { # <=
    salida <- filter(datos, x == discreta) %>%
      select (f.acum)
  }
  if (tipo == 4) { # >=
    salida <- filter(datos, x == discreta - 1) %>%
      select (f.acum)
    salida <- 1 - salida
  }
  salida
}