########################################################################
# Code Objective			    : Poblacion DANE - Defunciones
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
################### Población DANE: Defunciones ########################
########################################################################

# Lectura de archivos --------------------
Poblacion_defunciones_DANE_2018 <- read.csv("C:\\Users\\EEVV - 2018\\nofetal2018 20231025.csv")
Poblacion_defunciones_DANE_2019 <- read.csv("C:\\Users\\EEVV - 2019\\nofetal2019 20231025.csv")
Poblacion_defunciones_DANE_2020 <- read.csv("C:\\Users\\EEVV - 2020\\nofetal2020 20231025.csv")
Poblacion_defunciones_DANE_2021 <- read.csv("C:\\Users\\EEVV - 2021\\nofetal2021 20231025.csv")

# Combinacion de años --------------------
Poblacion_defunciones_DANE <- rbind(Poblacion_defunciones_DANE_2018[, c("COD_DPTO", "ANO", "SEXO", "GRU_ED1")], 
                                    Poblacion_defunciones_DANE_2019[, c("COD_DPTO", "ANO", "SEXO", "GRU_ED1")],
                                    Poblacion_defunciones_DANE_2020[, c("COD_DPTO", "ANO", "SEXO", "GRU_ED1")],
                                    Poblacion_defunciones_DANE_2021[, c("COD_DPTO", "ANO", "SEXO", "GRU_ED1")])

########################################################################
############## Población DANE: Defunciones total nacional ##############
########################################################################

# Totales nacionales --------------------
Poblacion_defunciones_DANE_n <- Poblacion_defunciones_DANE %>%
  # Se agrupa los datos por las columnas COD_DPTO, ANO, SEXO
  group_by(ANO, SEXO) %>%
  # Se resume los datos, calculando el total (conteo) de filas en cada grupo y almacenándolo en una nueva columna llamada "Total"
  summarise(Total = n(), .groups = "drop")

# Aplicar dcast de acuerdo al sexo --------------------
# Resultado final: Base con Dept, Año, Grupo etario, sexo
Poblacion_defunciones_DANE_n_2 <- reshape2::dcast(Poblacion_defunciones_DANE_n, ANO ~ SEXO, fun.aggregate = sum)
Poblacion_defunciones_DANE_n_2$COD_DPTO <- rep("Nacional", dim(Poblacion_defunciones_DANE_n_2)[1])

# Totales generales --------------------
Poblacion_defunciones_DANE_g <- Poblacion_defunciones_DANE %>%
  # Se agrupa los datos por las columnas COD_DPTO, ANO, SEXO
  group_by(COD_DPTO, ANO, SEXO) %>%
  # Se resume los datos, calculando el total (conteo) de filas en cada grupo y almacenándolo en una nueva columna llamada "Total"
  summarise(Total = n(), .groups = "drop")

# Aplicar dcast de acuerdo al sexo --------------------
# Resultado final: Base con Dept, Año, Grupo etario, sexo
Poblacion_defunciones_DANE_g_2 <- reshape2::dcast(Poblacion_defunciones_DANE_g, COD_DPTO + ANO ~ SEXO, fun.aggregate = sum)

# Se filtra por departamento de estudio y se crea su respectiva variable nombre de departamento  --------------------
# Se crea el filtro
Poblacion_defunciones_DANE_g_2 <- Poblacion_defunciones_DANE_g_2 %>%
  filter(COD_DPTO %in% c(11, 19, 91, 95))

# Union de las bases nacional con totales generales --------------------
Poblacion_defunciones_DANE_overall <- rbind(Poblacion_defunciones_DANE_g_2, Poblacion_defunciones_DANE_n_2)

# nombre de la variable
Poblacion_defunciones_DANE_overall$Departamento <- ifelse(Poblacion_defunciones_DANE_overall$COD_DPTO == 11, "Bogotá, D.C.",
                                                          ifelse(Poblacion_defunciones_DANE_overall$COD_DPTO == 19, "Cauca",
                                                                 ifelse(Poblacion_defunciones_DANE_overall$COD_DPTO == 91, "Amazonas",
                                                                        ifelse(Poblacion_defunciones_DANE_overall$COD_DPTO == 95, "Guaviare", Poblacion_defunciones_DANE_overall$COD_DPTO))))

Poblacion_defunciones_DANE_overall <- Poblacion_defunciones_DANE_overall[order(Poblacion_defunciones_DANE_overall$Departamento), c(6, 1, 2:5)]
Poblacion_defunciones_DANE_overall$Total <- rowSums(Poblacion_defunciones_DANE_overall[,4:6])

# Guardar datos
write.xlsx(Poblacion_defunciones_DANE_overall, "Poblacion_defunciones_DANE 20231219.xlsx")

########################################################################
############## Población DANE: Defunciones > 15 años ###################
########################################################################

# Clasificación por grupo etario --------------------
lim <- c(0, 10, 11, 12, 13, 15, 16, 17, 19, 20, 28, Inf)
labels <- c("De 0 a 14 años",
            "De 15 a 19 años", 
            "De 20 a 24 años", 
            "De 25 a 29 años", 
            "De 30 a 39 años",
            "De 40 a 44 años", 
            "De 45 a 49 años", 
            "De 50 a 59 años", 
            "De 60 a 64 años", 
            "Mayores de 65 años",
            "Edad desconocida")
Poblacion_defunciones_DANE$Grupo_Etario <- cut(Poblacion_defunciones_DANE$GRU_ED1, breaks = lim, labels = labels, include.lowest = TRUE)
Poblacion_defunciones_DANE <- Poblacion_defunciones_DANE %>% filter(!Grupo_Etario %in% c("De 0 a 14 años", "Edad desconocida"))

# Filtrado por dept y agregado por dept, año y sexo --------------------
Poblacion_defunciones_DANE_1 <- Poblacion_defunciones_DANE %>%
  # Se agrupa los datos por las columnas COD_DPTO, ANO, SEXO, EDAD
  group_by(COD_DPTO, ANO, SEXO, Grupo_Etario) %>%
  # Se filtra las filas por departamento de estudio
  filter(COD_DPTO %in% c(11, 19, 91, 95)) %>%
  # Se resume los datos, calculando el total (conteo) de filas en cada grupo y almacenándolo en una nueva columna llamada "Total"
  summarise(Total = n(), .groups = "drop")
Poblacion_defunciones_DANE_1 <- as.data.frame(Poblacion_defunciones_DANE_1)

# Aplicar dcast de acuerdo al sexo --------------------
# Resultado final: Base con Dept, Año, Grupo etario, sexo
Poblacion_defunciones_DANE_2 <- reshape2::dcast(Poblacion_defunciones_DANE_1, COD_DPTO + ANO + Grupo_Etario ~ SEXO, fun.aggregate = sum)

# Agregado por dept, año, sexo y edad: Calculo para totales generales --------------------
Poblacion_defunciones_DANE_3 <- Poblacion_defunciones_DANE %>% 
  # Se agrupa los datos por las columnas COD_DPTO, ANO y SEXO
  group_by(COD_DPTO, ANO, SEXO) %>%
  # Se filtra las filas por departamento de estudio
  filter(COD_DPTO %in% c(11, 19, 91, 95)) %>%
  # Se resume los datos, calculando el total (conteo) de filas en cada grupo
  summarise(Total = n(), .groups = "drop")
# Se convierte en un dataframe
Poblacion_defunciones_DANE_3 <- as.data.frame(Poblacion_defunciones_DANE_3)
# se aplica dcast por sexo para totales generales
Poblacion_defunciones_DANE_4 <- reshape2::dcast(Poblacion_defunciones_DANE_3, COD_DPTO + ANO ~ SEXO, fun.aggregate = sum)
# Se crea la categoria total general
# Resultado final: Base con los totales generales de cada año y departamento
Poblacion_defunciones_DANE_4$Grupo_Etario <- rep("Total general", dim(Poblacion_defunciones_DANE_4)[1])

# Filtrado por Naciaonal y agregado por año y sexo --------------------
Poblacion_defunciones_DANE_5 <- Poblacion_defunciones_DANE %>%
  # Se agrupa los datos por las columnas ANO, SEXO, EDAD
  group_by(ANO, SEXO, Grupo_Etario) %>%
  # Se resume los datos, calculando el total (conteo) de filas en cada grupo y almacenándolo en una nueva columna llamada "Total"
  summarise(Total = n(), .groups = "drop")
Poblacion_defunciones_DANE_5 <- as.data.frame(Poblacion_defunciones_DANE_5)

# Aplicar dcast de acuerdo al sexo --------------------
# Resultado final: Base con los datos nacionales por año y sexo
Poblacion_defunciones_DANE_6 <- reshape2::dcast(Poblacion_defunciones_DANE_5, ANO + Grupo_Etario ~ SEXO, fun.aggregate = sum)
Poblacion_defunciones_DANE_6$COD_DPTO <- rep("Nacional", dim(Poblacion_defunciones_DANE_6)[1])

# Agregado por dept, año, sexo y edad: Calculo para totales nacionales --------------------
Poblacion_defunciones_DANE_7 <- Poblacion_defunciones_DANE %>% 
  # Se agrupa los datos por las columnas COD_DPTO, ANO y SEXO
  group_by(ANO, SEXO) %>%
  # Se resume los datos, calculando el total (conteo) de filas en cada grupo
  summarise(Total = n(), .groups = "drop")
# Se convierte en un dataframe
Poblacion_defunciones_DANE_7 <- as.data.frame(Poblacion_defunciones_DANE_7)
# se aplica dcast por sexo para totales generales
Poblacion_defunciones_DANE_8 <- reshape2::dcast(Poblacion_defunciones_DANE_7, ANO ~ SEXO, fun.aggregate = sum)
# Se crea la categoria nacional y total general
# Resultado: base con la informacion de los totales generales nacional
Poblacion_defunciones_DANE_8$COD_DPTO <- rep("Nacional", dim(Poblacion_defunciones_DANE_8)[1])
Poblacion_defunciones_DANE_8$Grupo_Etario <- rep("Total general", dim(Poblacion_defunciones_DANE_8)[1])

# Total: Se combina los datos por departamentos con el total general  -------------------
Poblacion_defunciones_DANE_9 <- rbind(Poblacion_defunciones_DANE_2, Poblacion_defunciones_DANE_4, Poblacion_defunciones_DANE_6, Poblacion_defunciones_DANE_8)
Poblacion_defunciones_DANE_9$Total <- apply(Poblacion_defunciones_DANE_9[, -c(1:3)], 1, sum)