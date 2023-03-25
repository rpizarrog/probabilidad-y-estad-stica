# función de densidad para una variable aleatoria continua
f_prueba <- function(x) {
  ifelse(0 <= x & x <= 2, 1 - x/2, 0) }


# f_dens <- function (x) 
#   ifelse (x >=0 & x <= 2, 1 - x / 2, 0)
 

f_dens_1.entre.360 <- function(x) {
  ifelse(x >= 0 & x <= 360, 1/360, 0)
}



f_valor_esperado <- function(f_densidad, minimo, maximo) {
  resultado <- integrate(function(x) x * f_densidad(x), 
                        lower = minimo, 
                        upper = maximo)
  return(resultado)
}

# f_valor_esperado(f_densidad = f_dens, minimo = minimo, maximo = maximo)


f_varianza <- function(f_densidad, VE, minimo, maximo) {
  resultado <- integrate(f = function(x) (x - VE)^2 * f_densidad(x), 
                         lower = minimo, 
                         upper = maximo)
  
  resultado
}
  
f_graf_dens <- function (f_dens, x, intervalo) {
  library(ggplot2)
  
  y <- f_dens(x)
  a <- intervalo[1]
  b <- intervalo[2]
  
  curva <- ggplot()  +
    geom_point(aes(x = x, y = y), color='red') +
    geom_line(aes(x = x, y = y), color='blue') +
    geom_area(aes(x = a, y = b), fill='pink') +
      labs(title="Densidad f(x)", ) +
      xlab("x's") +
      ylab("f(x)")
  
  
  return(curva)  
}


# Para crear unos datos de prueba

f_crear_datos_graf <- function(x, f_densidad, minmax, intervalo) {
  y <- f_densidad(x)
  
  datos <- data.frame(x, y)
  datos
  
  # Intervalo F(x)
  min_x <- minmax[1]
  max_x <- minmax[2]
  datos <- cbind(datos, f = ifelse(datos$x >= min_x & datos$x <= max_x, 'f(x)', '0'))
  datos$f <- as.factor(datos$f)
  # datos
  
  
  # Intervalo de probabilidad [a, b]
  a <- intervalo[1]
  b <- intervalo[2]
  datos <- cbind(datos, p = ifelse(datos$x >= a & datos$x <= b & datos$f == 'f(x)', 'P(x)', as.character(datos$f)))
  datos$p <- as.factor(datos$p)
  
  return(datos)
  
} 



f_graf_dens_ggplot <- function (f_dens, datos) {
  g <- ggplot(datos, aes(x = x, y = y, fill = p)) + 
    geom_area(alpha = 0.5, position = "identity") +
    scale_fill_manual(values = c("white", "pink", "blue")) +
    ggtitle("F(X) = 1; P(a <= x <= b)") +
    xlab("x") +
    ylab("f(x)")
  
  return (g)
}


es_densidad<- function(la_funcion ) {

  # No funciona aún
  densidad <- "Es función de densidad"
  
  # Condición 1
  if (la_funcion < 0)
    densidad <- "No es función de densidad"
  
  # Condición 2
  integral <- integrate(function(x) .func(x, ...), 
                        lower = -Inf, 
                        upper = Inf)
  if(integral$value != 1)
    densidad <- "No es función de densidad"
  
  densidad
}

  
  

