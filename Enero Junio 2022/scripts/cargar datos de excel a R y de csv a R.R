# Instalar paquetes o librerías
# install.packages("xlsx", "readxl")

# Cargar datos de EXCEL
# library() significa cargar un paquete previamente instaldo
library(readxl)
ruta.excel <- "C:/Users/Usuario/Documents/Mis clases ITD/SEMESTRE Feb-Jun 2022/Trabajos en R/datos/personas_10-02-2022.xlsx"
ruta.csv <- "C:/Users/Usuario/Documents/Mis clases ITD/SEMESTRE Feb-Jun 2022/Trabajos en R/datos/personas_10-02-2022.csv"
ruta.excel.alumnos <- "C:/Users/Usuario/Documents/Mis clases ITD/SEMESTRE Feb-Jun 2022/Trabajos en R/datos/LISTA ALUMNOS 2Y.xlsx"
ruta.csv.alumnos <- "C:/Users/Usuario/Documents/Mis clases ITD/SEMESTRE Feb-Jun 2022/Trabajos en R/datos/LISTA ALUMNOS 2Y.csv"

datos1 <- read_xlsx(ruta.excel) # Importar o cargar un archivo excel
alumnos <- read_xlsx(path = ruta.excel.alumnos, skip = 1)

# Cargar datos de CSV
# install.packages("readr")
library(readr)

datos2 <- read.csv(ruta.csv)
