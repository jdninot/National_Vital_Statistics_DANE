########################################################################
# Code Objective			    : Poblacion etnica DANE- Nacimientos
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
Poblacion_nacimientos_DANE_2018 <- read.csv("C:\\Users\\\\Datos\\DANE\\Estadísticas Vitales - EEVV\\EEVV - 2018\\nac2018 20231025.csv")
Poblacion_nacimientos_DANE_2019 <- read.csv("C:\\Users\\\\Datos\\DANE\\Estadísticas Vitales - EEVV\\EEVV - 2019\\nac2019 20231025.csv")
Poblacion_nacimientos_DANE_2020 <- read.csv("C:\\Users\\\\Datos\\DANE\\Estadísticas Vitales - EEVV\\EEVV - 2020\\nac2020 20231025.csv")
Poblacion_nacimientos_DANE_2021 <- read.csv("C:\\Users\\\\Datos\\DANE\\Estadísticas Vitales - EEVV\\EEVV - 2021\\nac2021 20231025.csv")

# Combinacion de años --------------------
Poblacion_nacimientos_DANE <- rbind(Poblacion_nacimientos_DANE_2018[, c("COD_DPTO", "ANO", "SEXO", "IDPERTET")], 
                                    Poblacion_nacimientos_DANE_2019[, c("COD_DPTO", "ANO", "SEXO", "IDPERTET")],
                                    Poblacion_nacimientos_DANE_2020[, c("COD_DPTO", "ANO", "SEXO", "IDPERTET")],
                                    Poblacion_nacimientos_DANE_2021[, c("COD_DPTO", "ANO", "SEXO", "IDPERTET")])

# Filtrado por dept y agregado por dept, año y sexo --------------------
Poblacion_nacimientos_DANE_1 <- Poblacion_nacimientos_DANE %>%
  # Se agrupa los datos por las columnas COD_DPTO, ANO y SEXO
  group_by(IDPERTET, COD_DPTO, ANO, SEXO) %>%
  # Se filtra las filas por departamento de estudio y grupo etnico
  filter(IDPERTET %in% c(1, 2, 3, 4, 5), COD_DPTO %in% c(11, 19, 91, 95)) %>%
  # Se resume los datos, calculando el total (conteo) de filas en cada grupo y almacenándolo en una nueva columna llamada "Total"
  summarise(Total = n(), .groups = "drop")
Poblacion_nacimientos_DANE_1 <- as.data.frame(Poblacion_nacimientos_DANE_1)

# Aplicar dcast de acuerdo al sexo --------------------
Poblacion_nacimientos_DANE_2 <- reshape2::dcast(Poblacion_nacimientos_DANE_1, IDPERTET + COD_DPTO + ANO ~ SEXO, fun.aggregate = sum)

# Agregado por dept, año y sexo: Calculo para totales generales Nacional --------------------
Poblacion_nacimientos_DANE_3 <- Poblacion_nacimientos_DANE %>% 
  # Se agrupa los datos por las columnas COD_DPTO, ANO y SEXO
  group_by(IDPERTET, ANO, SEXO) %>%
  filter(IDPERTET %in% c(1, 2, 3, 4, 5)) %>%
  # Se resume los datos, calculando el total (conteo) de filas en cada grupo
  summarise(Total = n(), .groups = "drop")
# Se convierte en un dataframe
Poblacion_nacimientos_DANE_3 <- as.data.frame(Poblacion_nacimientos_DANE_3)
# se aplica dcast por sexo para totales generales
Poblacion_nacimientos_DANE_4 <- reshape2::dcast(Poblacion_nacimientos_DANE_3, IDPERTET + ANO ~ SEXO, fun.aggregate = sum)
# Se crea la categoria nacional
Poblacion_nacimientos_DANE_4$COD_DPTO <- rep("Nacional",dim(Poblacion_nacimientos_DANE_4)[1])

# Total: Se combina los datos por departamentos con el total general nacional  -------------------
Poblacion_nacimientos_DANE_5 <- rbind(Poblacion_nacimientos_DANE_2, Poblacion_nacimientos_DANE_4)
Poblacion_nacimientos_DANE_5$Total <- apply(Poblacion_nacimientos_DANE_5[, -c(1:3)], 1, sum)

# Creacion de nombre de la variable IDPERTET
Poblacion_nacimientos_DANE_5$Pertenencia_etnica <- ifelse(Poblacion_nacimientos_DANE_5$IDPERTET == 1, "Indígena",
                                                          ifelse(Poblacion_nacimientos_DANE_5$IDPERTET == 2, "Rom (Gitano)",
                                                                 ifelse(Poblacion_nacimientos_DANE_5$IDPERTET == 3, "Raizal del archipiélago de San Andrés y Providencia",
                                                                        ifelse(Poblacion_nacimientos_DANE_5$IDPERTET == 4, "Palenquero de San Basilio",
                                                                               ifelse(Poblacion_nacimientos_DANE_5$IDPERTET == 5, "Negro(a), mulato(a), afrocolombiano(a) o afrodescendiente", Poblacion_nacimientos_DANE_5$IDPERTET)))))
# Ordenar la base
colnames(Poblacion_nacimientos_DANE_5)
Poblacion_nacimientos_DANE_5 <- Poblacion_nacimientos_DANE_5[order(Poblacion_nacimientos_DANE_5$Departamento), c(8, 2, 9, 1, 3:7)]
View(Poblacion_nacimientos_DANE_5)

# Guardar datos
# Poblacion_nacimientos_DANE_5 %>% filter(ANO == "2018")
write.xlsx(Poblacion_nacimientos_DANE_5, "Poblacion_nacimientos_etnia_DANE 20231219.xlsx")

