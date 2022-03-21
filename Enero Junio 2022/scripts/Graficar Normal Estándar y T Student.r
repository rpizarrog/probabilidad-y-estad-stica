# Graficar Normal Estándar y T Student
media = 50; desv = 2
media.p = 50.5

f.devolver.t <- function(media.muestra, media.pob, desv.muestra, n) {
  t <- (media.muestra - media.pob) / (desv.muestra / sqrt(n))
  t
}

f.devolver.z <- function(x, media, desv) {
  z <- (x - media) / desv
  z
}

# source()
x <- rnorm(n = 1000, mean = media, sd = desv)
z <- f.devolver.z(x = x, media = media, desv = desv)
t <- rt(n = 1000, df = 5)


prob.x <- dnorm(x = x, mean = media, sd = desv)
prob.z <- dnorm(z)
prob.t <- dt(x = t, df = 5)

# Visualizando las distribuciones
g <- ggplot() +
        geom_point(aes (x = z, y = prob.z), size = 0.5, colour = "green") +
    geom_line(aes (x = z, y = prob.z), size = 0.5, colour = "green") +
    geom_point(aes (x = t, y = prob.t), size = 0.5, colour = "brown") +
    geom_line(aes (x = t, y = prob.t), size = 0.5, colour = "brown")



