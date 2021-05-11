fhola <- function () {
  print("Hola mundo desde la función")
}

fsumar <- function(a, b) {
  suma <- a + b
  suma
} 

foperaciones <- function(x,y,tipo=1) {
  if (tipo == 1) {
    res <- x + y
  }
  if (tipo == 2) {
    res <- x - y
  }
  if (tipo == 3) {
    res <- x * y
  }
  if (tipo == 4) {
    res <- x / y
  }
  if (tipo == 5) {
    res <- x ^ y
  }
  res
  
}



fordenar <- function(datos, columna) {
  # Esta función recibe un dataframe y el número de la columna
  #y ordena todo el df
  df.orden <- datos[order(datos[columna]),]
  df.orden
}
ffrecuencias <- function(vector) {
  frec <- table(vector)
  columna <- names(frec)
  frecuencia <- as.numeric(frec)
  frecuencia
}


f.lanzar.moneda <- function(S.espacio.muestral = c("H", "T")) {
  punto.muestral <- sample(S.espacio.muestral, 1) 
  punto.muestral
}

f.lanzar.dado <- function(S.espacio.muestral=c(1,2,3,4,5,6)) {
  punto.muestral <- as.character(sample(S.espacio.muestral, 1))
  punto.muestral
}

# La función f.moneda.dado regresa un punto muestral 
# en caso de que el primer resultado sea 'H',
# se lanza nuevamente la moneda, 
# Por el contrario, en caso de que el primer resultado 
# sea una 'T' se lanza el dado 
# En ambos casos se determina su resultado

f.moneda.dado <- function(S.espacio.muestral) {
  punto.muestral.1 <- f.lanzar.moneda()
  if (punto.muestral.1 == "H") {
    punto.muestral <- sample(S.espacio.muestral[1:2], 1)
  } else {
    punto.muestral <- sample(S.espacio.muestral[3:length(S.espacio.muestral)], 1)
  }
  punto.muestral 
}

f.contar.dados <- function(S.espacio.muestral,inicial, final) {
  sumas<- NULL
  for (i in 1:length(S.espacio.muestral)) {
        sumas[i] <- as.numeric(substr(S.espacio.muestral[i],1,1)) + as.numeric(substr(S.espacio.muestral[i],2,2)) 
  }
  sumas <- sumas[order(sumas)]
  print(sumas)
  length(which(sumas >= inicial & sumas <=final))
  
}

f.sumar.dados <- function(S.espacio.muestral,inicial, final) {
  sumas<- NULL
  for (i in 1:length(S.espacio.muestral)) {
    sumas[i] <- as.numeric(substr(S.espacio.muestral[i],1,1)) + as.numeric(substr(S.espacio.muestral[i],2,2)) 
  }
  sumas <- sumas[order(sumas)]
  print(sumas)
  which(sumas >= inicial & sumas <=final)
  
}


f.fichas.domino <- function() {
  fichas_cero <- c("00", "01", "02", "03", "04", "05", "06")
  fichas_uno <- c("11","12","13","14","15","16")
  fichas_dos <- c("22","23","24","25","26")
  fichas_tres <- c("33", "34", "35", "36")
  fichas_cuatro <- c("44","45","46")
  fichas_cinco <- c("55","56")
  fichas_seis <- c("66")
  S.domino <- c(fichas_cero, fichas_uno, fichas_dos, fichas_tres, 
                                 fichas_cuatro, fichas_cinco, fichas_seis)
  S.domino
}

f.distribucion.fichas.domino <- function(S.espacio.muestral,inicial, final) {
  sumas<- NULL
  for (i in 1:length(S.espacio.muestral)) {
    sumas[i] <- as.numeric(substr(S.espacio.muestral[i],1,1)) + as.numeric(substr(S.espacio.muestral[i],2,2)) 
  }
  sumas <- sumas[order(sumas)]
  # print(sumas)
  tabla <- transform(table(sumas))
  tabla <- cbind(tabla, prob = tabla$Freq / length(S.espacio.muestral))
  tabla <- cbind(tabla, acum = cumsum(tabla$prob))
  tabla
}

plotunif <- function(x, min = 0, max = 1, lwd = 1, col = 1, ...) {

    # Rejilla de valores del eje X
    if (missing(x)) {
        x <- seq(min - 0.5, max + 0.5, 0.01)
    }

    if(max < min) {
        stop("'min' must be lower than 'max'")
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






