# Instalar paquetes o librerías
# install.packages("xlsx", "readxl")


# Cargar datos de EXCEL
# library() significa cargar un paquete previamente instaldo
library(readxl)
ruta.excel <- "C:/Users/Usuario/Documents/Mis clases ITD/SEMESTRE Feb-Jun 2022/Trabajos en R/datos/personas_10-02-2022.xlsx"
ruta.csv <- "C:/Users/Usuario/Documents/Mis clases ITD/SEMESTRE Feb-Jun 2022/Trabajos en R/datos/personas_10-02-2022.csv"

datos1 <- read_xlsx(ruta.excel)

# Cargar datos de CSV
# install.packages("readr")
library(readr)

datos2 <- read.csv(ruta.csv)
