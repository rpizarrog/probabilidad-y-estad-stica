e=exp(1)
e
pi
media=170
desv=5
x=170

# Calcular la función de densidad f(x) de una distribucion normal cuando x tiene valor especifico
numerador <- e^(-(x - media)^2 / (2 * desv^2))
denominador <- desv * sqrt(2 * pi)
prob <- numerador / denominador
prob

# Calcular la probabilidad acumulada F(x) con la funcion de dnorm
dnorm(x = 150:200,mean = media,sd = desv)

# Crear un data.frame con los valores de x y su densidad
datos=data.frame(x=150:200,f.x=dnorm(x = 150:200,mean = media,sd = desv))
datos

# Graficar la densidad con gplot
library(ggplot2)
ggplot(data = datos,aes(x = x,y=f.x))+
  geom_point(color="blue")+
  geom_line(color="red")

# Calcular probabilidades acumuladas con pnorm
# Probabilidad acumulada con personas que miden 160
acum=pnorm(q = 160,mean = media,sd = desv)
acum

# Agregar el valor acumulado a data_frame
datos=data.frame(x=150:200,f.x=dnorm(x = 150:200,mean = media,sd = desv),
                 F.x=pnorm(q=150:200,mean=media,sd=desv))
datos

# Calcular la probabilidad mayor o gial a 160, cola por la derecha
pnorm(q=160,mean=media,sd=desv,lower.tail = FALSE)
1-pnorm(q=160,mean=media,sd=desv)

# Generar numeros aleatorios de una distribucion normal
estaturas=rnorm(n = 100,mean = media,sd=desv)
estaturas
mean(estaturas)

# Obtener el valor x a partir de un  acumulada
qnorm(p = .5,mean = media,sd=desv)

# Calcular la probabilidad en el intervao 21-27 de una distribución normal de media igual a 30 y desviación igual a 6

pnorm(30,mean = 25,sd = 6)-pnorm(20,mean = 25,sd = 6)
