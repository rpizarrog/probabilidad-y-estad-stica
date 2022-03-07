# Repartir siete fichas de dominó
f.fichas.domino <- function() {
  domino <- c("0","1","2","3","4","5","6")
  fichas <- combinations(7, 2, domino, repeats.allowed = TRUE)
  
  fichas <- paste(fichas[,1], fichas[,2])
  fichas
  
  fichas.combinadas <- data.frame(combinations(28, 7, fichas))
  names(fichas.combinadas) <- c("F1", "F2", "F3", "F4", "F5", "F6", "F7")
  
  # fichas.combinadas  
}

f.repartir.fichas.domino <- function(fichas) {
  repartir <- sample(x = fichas, size = 7, replace = FALSE)
  repartir
}