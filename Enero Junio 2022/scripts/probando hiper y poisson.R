#  Distribución hiérgeométrica
N <- 40 # Tamaño de la población
n <- 8  # La cantidad de elementis que se extraen de la población
k <- 10 # La cantidad elementos con características malas o semejantes
x <-  3 #0, 1, 2, 3, 4, 5, 6, 7, 8, .... n

N ; n; k; x

# Comprobar el resultado conforme a la fórmula
# Combinar 10 con 3
combinations(n = 10, r = 3)
p.factor <- factorial(10) / (factorial(3) * factorial(10-3))
p.factor

# Combinar 40 con 10
combinations(30, 5)
s.factor <- factorial(30) / (factorial(5) * factorial(30-5))
s.factor


# Combinar 40 con 10
nrow(combinations(40, 8))
t.factor <- factorial(40) / (factorial(8) * factorial(40-8))
t.factor

# Calcular  p(3)
numerador <-  p.factor * s.factor
denominador <- t.factor

dens.hyper <- numerador / denominador
dens.hyper



# Calculando densidad con dhyper
dhyper(x = 0:8, m = 10, n = N - 10, k = 8)


f.prob.hiper(x = 0:8, N = 40, n = 8, r = 10)



# Poisson

x.0 <- (2^0 * exp(1) ^ -2) / factorial(0)
x.0

x.1 <- (2^1 * exp(1) ^ -2) / factorial(1)
x.1

x.2 <- (2^2 * exp(1) ^ -2) / factorial(2)
x.2

