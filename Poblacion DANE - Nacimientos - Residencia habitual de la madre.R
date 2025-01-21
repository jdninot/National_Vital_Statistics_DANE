########################################################################
# Code Objective			    : Poblacion DANE- Nacimientos
# Programmer 					    : David Niño
########################################################################

# Borrar entorno --------------------
remove(list = ls())

# Library --------------------
library(dplyr)
library(readxl)
library(openxlsx)
library(reshape2)
library(stringr)
library(data.table)

########################################################################
################### Población DANE: Nacimientos ########################
########################################################################

# Lectura de archivos --------------------
Poblacion_nacimientos_DANE_2018 <- read.csv("C:\\Users\\\\EEVV - 2018\\nac2018 20231025.csv")
Poblacion_nacimientos_DANE_2019 <- read.csv("C:\\Users\\\\EEVV - 2019\\nac2019 20231025.csv")
Poblacion_nacimientos_DANE_2020 <- read.csv("C:\\Users\\\\EEVV - 2020\\nac2020 20231025.csv")
Poblacion_nacimientos_DANE_2021 <- read.csv("C:\\Users\\\\EEVV - 2021\\nac2021 20231025.csv")

# Combinacion de años --------------------
Poblacion_nacimientos_DANE <- rbind(Poblacion_nacimientos_DANE_2018[, c("CODPTORE", "ANO", "SEXO", "IDPERTET")], 
                                    Poblacion_nacimientos_DANE_2019[, c("CODPTORE", "ANO", "SEXO", "IDPERTET")],
                                    Poblacion_nacimientos_DANE_2020[, c("CODPTORE", "ANO", "SEXO", "IDPERTET")],
                                    Poblacion_nacimientos_DANE_2021[, c("CODPTORE", "ANO", "SEXO", "IDPERTET")])

# Filtrado por dept y agregado por dept, año y sexo --------------------
Poblacion_nacimientos_DANE_1 <- Poblacion_nacimientos_DANE %>%
  # Se agrupa los datos por las columnas CODPTORE, ANO y SEXO
  group_by(CODPTORE, ANO, SEXO) %>%
  # Se filtra las filas por departamento de estudio
  filter(CODPTORE %in% c(11, 19, 91, 95)) %>%
  # Se resume los datos, calculando el total (conteo) de filas en cada grupo y almacenándolo en una nueva columna llamada "Total"
  summarise(Total = n(), .groups = "drop")
Poblacion_nacimientos_DANE_1 <- as.data.frame(Poblacion_nacimientos_DANE_1)

# Aplicar dcast de acuerdo al sexo --------------------
Poblacion_nacimientos_DANE_2 <- reshape2::dcast(Poblacion_nacimientos_DANE_1, CODPTORE + ANO ~ SEXO, fun.aggregate = sum)

# Agregado por dept, año y sexo: Calculo para totales generales Nacional --------------------
Poblacion_nacimientos_DANE_3 <- Poblacion_nacimientos_DANE %>% 
  # Se agrupa los datos por las columnas CODPTORE, ANO y SEXO
  group_by(ANO, SEXO) %>%
  # Se resume los datos, calculando el total (conteo) de filas en cada grupo
  summarise(Total = n(), .groups = "drop")
# Se convierte en un dataframe
Poblacion_nacimientos_DANE_3 <- as.data.frame(Poblacion_nacimientos_DANE_3)
# se aplica dcast por sexo para totales generales
Poblacion_nacimientos_DANE_4 <- reshape2::dcast(Poblacion_nacimientos_DANE_3, ANO ~ SEXO, fun.aggregate = sum)
# Se crea la categoria nacional
Poblacion_nacimientos_DANE_4$CODPTORE <- rep("Nacional",dim(Poblacion_nacimientos_DANE_4)[1])

# Total: Se combina los datos por departamentos con el total general nacional  -------------------
Poblacion_nacimientos_DANE_5 <- rbind(Poblacion_nacimientos_DANE_2, Poblacion_nacimientos_DANE_4)
Poblacion_nacimientos_DANE_5$Total <- apply(Poblacion_nacimientos_DANE_5[, -c(1:2)], 1, sum)

