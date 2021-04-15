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




