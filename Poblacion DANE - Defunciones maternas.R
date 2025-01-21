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

# Modificar la categorizacion de probable manera de muerte
# 1 = Natural por 0, 2 = Violenta por NA, 3 = En estudio por NA
# 0 = Natural, 1 = Homicidio, 2 = Accidente, 4 = Suicidio, 5 = No se pudo deter, 6 = Desconocido, 7 = Intervencion legal, 8 = Guerra
Poblacion_defunciones_DANE_2018$P_PMAN_IRIS = ifelse(Poblacion_defunciones_DANE_2018$PMAN_MUER == 1, 0, NA)

# Crear una unica variables entre embarazada cuando falleció y embarazada en las últimas 6 semanas
Poblacion_defunciones_DANE_2018$EMB <- ifelse((!is.na(Poblacion_defunciones_DANE_2018$EMB_SEM) & Poblacion_defunciones_DANE_2018$EMB_SEM == 1) |
                                                (!is.na(Poblacion_defunciones_DANE_2018$EMB_FAL) & Poblacion_defunciones_DANE_2018$EMB_FAL == 1), 1, NA)
Poblacion_defunciones_DANE_2019$EMB <- ifelse((!is.na(Poblacion_defunciones_DANE_2019$EMB_SEM) & Poblacion_defunciones_DANE_2019$EMB_SEM == 1) |
                                                (!is.na(Poblacion_defunciones_DANE_2019$EMB_FAL) & Poblacion_defunciones_DANE_2019$EMB_FAL == 1), 1, NA)
Poblacion_defunciones_DANE_2020$EMB <- ifelse((!is.na(Poblacion_defunciones_DANE_2020$EMB_SEM) & Poblacion_defunciones_DANE_2020$EMB_SEM == 1) |
                                                (!is.na(Poblacion_defunciones_DANE_2020$EMB_FAL) & Poblacion_defunciones_DANE_2020$EMB_FAL == 1), 1, NA)
Poblacion_defunciones_DANE_2021$EMB <- ifelse((!is.na(Poblacion_defunciones_DANE_2021$EMB_SEM) & Poblacion_defunciones_DANE_2021$EMB_SEM == 1) |
                                                (!is.na(Poblacion_defunciones_DANE_2021$EMB_FAL) & Poblacion_defunciones_DANE_2021$EMB_FAL == 1), 1, NA)

# Combinacion de años --------------------
Poblacion_defunciones_DANE <- rbind(Poblacion_defunciones_DANE_2018[, c("CAUSA_667", "EMB", "CODPTORE", "ANO", "SEXO", "GRU_ED1")], 
                                    Poblacion_defunciones_DANE_2019[, c("CAUSA_667", "EMB", "CODPTORE", "ANO", "SEXO", "GRU_ED1")],
                                    Poblacion_defunciones_DANE_2020[, c("CAUSA_667", "EMB", "CODPTORE", "ANO", "SEXO", "GRU_ED1")],
                                    Poblacion_defunciones_DANE_2021[, c("CAUSA_667", "EMB", "CODPTORE", "ANO", "SEXO", "GRU_ED1")])

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

# Filtrado por dept y agregado por dept, año y sexo --------------------
Poblacion_defunciones_DANE_1 <- Poblacion_defunciones_DANE %>%
  # Se agrupa los datos por las columnas CODPTORE, ANO, SEXO, EDAD
  group_by(CAUSA_667, EMB, CODPTORE, ANO, SEXO, Grupo_Etario) %>%
  # Se filtra las filas por departamento de estudio
  filter(CODPTORE %in% c(11, 19, 91, 95)) %>%
  # Se filtra por muerte natural y mujeres embarazadas en las ultimas 6 semanas
  filter(CAUSA_667 == 612, EMB == 1) %>%
  # Se resume los datos, calculando el total (conteo) de filas en cada grupo y almacenándolo en una nueva columna llamada "Total"
  summarise(Total = n(), .groups = "drop")
Poblacion_defunciones_DANE_1 <- as.data.frame(Poblacion_defunciones_DANE_1)

# Aplicar dcast de acuerdo al sexo --------------------
# Resultado final: Base con Dept, Año, Grupo etario, sexo
Poblacion_defunciones_DANE_2 <- reshape2::dcast(Poblacion_defunciones_DANE_1, CAUSA_667 + EMB + CODPTORE + ANO + Grupo_Etario ~ SEXO, fun.aggregate = sum)

# Agregado por dept, año, sexo y edad: Calculo para totales generales --------------------
Poblacion_defunciones_DANE_3 <- Poblacion_defunciones_DANE %>% 
  # Se agrupa los datos por las columnas CODPTORE, ANO y SEXO
  group_by(CAUSA_667, EMB, CODPTORE, ANO, SEXO) %>%
  # Se filtra las filas por departamento de estudio
  filter(CODPTORE %in% c(11, 19, 91, 95)) %>%
  # Se filtra por muerte natural y mujeres embarazadas en las ultimas 6 semanas
  filter(CAUSA_667 == 612, EMB == 1) %>%
  # Se resume los datos, calculando el total (conteo) de filas en cada grupo
  summarise(Total = n(), .groups = "drop")
# Se convierte en un dataframe
Poblacion_defunciones_DANE_3 <- as.data.frame(Poblacion_defunciones_DANE_3)

# se aplica dcast por sexo para totales generales  --------------------
Poblacion_defunciones_DANE_4 <- reshape2::dcast(Poblacion_defunciones_DANE_3, CAUSA_667 + EMB + CODPTORE + ANO ~ SEXO, fun.aggregate = sum)
# Se crea la categoria total general
# Resultado final: Base con los totales generales de cada año y departamento
Poblacion_defunciones_DANE_4$Grupo_Etario <- rep("Total general", dim(Poblacion_defunciones_DANE_4)[1])

# Filtrado por Naciaonal y agregado por año y sexo --------------------
Poblacion_defunciones_DANE_5 <- Poblacion_defunciones_DANE %>%
  # Se agrupa los datos por las columnas ANO, SEXO, EDAD
  group_by(CAUSA_667, EMB, ANO, SEXO, Grupo_Etario) %>%
  # Se filtra por muerte natural y mujeres embarazadas en las ultimas 6 semanas
  filter(CAUSA_667 == 612, EMB == 1) %>%
  # Se resume los datos, calculando el total (conteo) de filas en cada grupo y almacenándolo en una nueva columna llamada "Total"
  summarise(Total = n(), .groups = "drop")
Poblacion_defunciones_DANE_5 <- as.data.frame(Poblacion_defunciones_DANE_5)

# Aplicar dcast de acuerdo al sexo --------------------
# Resultado final: Base con los datos nacionales por año y sexo
Poblacion_defunciones_DANE_6 <- reshape2::dcast(Poblacion_defunciones_DANE_5, CAUSA_667 + EMB + ANO + Grupo_Etario ~ SEXO, fun.aggregate = sum)
Poblacion_defunciones_DANE_6$CODPTORE <- rep("Nacional", dim(Poblacion_defunciones_DANE_6)[1])

# Agregado por dept, año, sexo y edad: Calculo para totales nacionales --------------------
Poblacion_defunciones_DANE_7 <- Poblacion_defunciones_DANE %>% 
  # Se agrupa los datos por las columnas CODPTORE, ANO y SEXO
  group_by(CAUSA_667, EMB, ANO, SEXO) %>%
  # Se filtra por muerte natural y mujeres embarazadas en las ultimas 6 semanas
  filter(CAUSA_667 == 612, EMB == 1) %>%
  # Se resume los datos, calculando el total (conteo) de filas en cada grupo
  summarise(Total = n(), .groups = "drop")
# Se convierte en un dataframe
Poblacion_defunciones_DANE_7 <- as.data.frame(Poblacion_defunciones_DANE_7)

# se aplica dcast por sexo para totales generales --------------------
Poblacion_defunciones_DANE_8 <- reshape2::dcast(Poblacion_defunciones_DANE_7, CAUSA_667 + EMB + ANO ~ SEXO, fun.aggregate = sum)
# Se crea la categoria nacional y total general
# Resultado: base con la informacion de los totales generales nacional
Poblacion_defunciones_DANE_8$CODPTORE <- rep("Nacional", dim(Poblacion_defunciones_DANE_8)[1])
Poblacion_defunciones_DANE_8$Grupo_Etario <- rep("Total general", dim(Poblacion_defunciones_DANE_8)[1])

# Total: Se combina los datos por departamentos con el total general  -------------------
Poblacion_defunciones_DANE_9 <- rbind(Poblacion_defunciones_DANE_2, Poblacion_defunciones_DANE_4, Poblacion_defunciones_DANE_6, Poblacion_defunciones_DANE_8)

# Creacion de nombre de la variable departamento
Poblacion_defunciones_DANE_9$Departamento <- ifelse(Poblacion_defunciones_DANE_9$CODPTORE == 11, "Bogotá, D.C.",
                                                    ifelse(Poblacion_defunciones_DANE_9$CODPTORE == 19, "Cauca",
                                                           ifelse(Poblacion_defunciones_DANE_9$CODPTORE == 91, "Amazonas",
                                                                  ifelse(Poblacion_defunciones_DANE_9$CODPTORE == 95, "Guaviare", Poblacion_defunciones_DANE_9$CODPTORE))))
colnames(Poblacion_defunciones_DANE_9)
View(Poblacion_defunciones_DANE_9)

# Ordenar y guardar datos
Poblacion_defunciones_DANE_9 <- Poblacion_defunciones_DANE_9[order(Poblacion_defunciones_DANE_9$Departamento, Poblacion_defunciones_DANE_9$ANO, Poblacion_defunciones_DANE_9$Grupo_Etario), c(7, 3, 1, 2, 4:6)]
colnames(Poblacion_defunciones_DANE_9) <- c("Departamento", "CODPTORE", "CAUSA_667", "EMB", "Año", "Grupo Etario", "Muertes maternas")
View(Poblacion_defunciones_DANE_9)
write.xlsx(Poblacion_defunciones_DANE_9, "Poblacion_defunciones_maternas_DANE 20231219.xlsx")
