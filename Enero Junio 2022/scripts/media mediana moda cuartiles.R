datos <- c(21, 22, 23, 26, 27, 28, 25, 26, 30,
           30, 29, 21, 22, 23, 20, 23, 27, 29,
           29, 29, 30, 25, 26, 27, 30, 22, 25,
           20, 25, 30, 22, 21, 20, 29, 28, 27,
           27, 30, 21, 30, 30, 30)

datos

sort(datos)

n <- length(datos)
n

media <- mean(datos)
mediana <- median(datos)
moda <- table(datos)

media; mediana; moda

# Cuartiles

cuartiles <- quantile(x = datos, c(0.25, 0.50, 0.75))
cuartiles


# Datos de clase de las 10:00

datos <- c(20, 23, 35, 42, 50, 38, 53, 
           27, 19, 31, 33, 24, 30, 45, 
           18, 26, 19, 18, 20, 21, 37, 
           43, 52, 26, 34, 56, 19, 39, 
           55, 42, 39, 21, 28)
media <- mean(datos)

mediana <- medmediaian(datos)

moda <- table(datos)

media
mediana
moda

cuartiles <- quantile(x = datos, 
                      probs = c(0.25, 0.50, 0.75))
cuartiles


# Datos del caso 3
datos <- c(40,60,50,45,65,70,95,90,45,60,43,56, 65, 80, 45, 70, 45, 75, 45, 54, 35, 46, 47, 50, 50, 60, 50, 50, 65, 50, 50, 22, 54, 68, 70, 46, 54, 55, 50, 55, 40, 68, 76, 56, 55, 45, 50, 43, 46, 47, 70, 24, 34, 54,43, 34, 45, 45, 45, 45)

datos

mean(datos)








