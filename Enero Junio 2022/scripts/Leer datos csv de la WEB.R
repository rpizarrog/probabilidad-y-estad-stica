# Leer archivo csv de WEB o local
# Si son muchos datos se recomienda cargarlos de manera local

library(readr)

# datos en dirección URL WEB
localidades <- read.csv("https://raw.githubusercontent.com/rpizarrog/probabilidad-y-estad-stica/master/datos/locdurangomx.csv")

datos.laboral <- read.csv("https://raw.githubusercontent.com/rpizarrog/probabilidad-y-estad-stica/master/datos/EncuestaClimaCulturaOrganizacional_2010-2017.csv")

datos.fifa <- read.csv("https://raw.githubusercontent.com/rpizarrog/probabilidad-y-estad-stica/master/datos/datos.FIFA.limpios.csv")

# Datos de manera local
datos.covid <- read.csv("C:/Users/Usuario/Documents/Mis clases ITD/SEMESTRE Feb-Jun 2022/Trabajos en R/datos/220210COVID19MEXICO.csv")
