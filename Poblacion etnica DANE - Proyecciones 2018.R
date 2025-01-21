
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
################### Población DANE: indigenas ##########################
########################################################################

# Lectura de archivos --------------------
Poblacion_indigenas_DANE <- read_excel("C:\\Users\\\\Datos\\DANE\\Demografía y población\\Autoreconocimiento étnico\\Autoreconocimiento_étnico_CNPV_2018.xlsx", sheet = "Indígena")
Poblacion_indigenas_DANE <- as.data.frame(Poblacion_indigenas_DANE)
head(Poblacion_indigenas_DANE)

# Eliminar las filas de los totales generales --------------------
Poblacion_indigenas_DANE_2 <- Poblacion_indigenas_DANE %>% filter(Edad != "Total")
head(Poblacion_indigenas_DANE_2, 200)

# Clasificación por grupo etario --------------------

# Convertir la variable Edad en variable numérica
Poblacion_indigenas_DANE_2$Edad <- as.numeric(Poblacion_indigenas_DANE_2$Edad)
head(Poblacion_indigenas_DANE_2, 200)

# Crear los labels y limites
lim <- c(0, 17, 19, 24, 29, 39, 44, 49, 59, 64, Inf)
labels <- c("De 0 a 17 años",
            "De 18 a 19 años", 
            "De 20 a 24 años", 
            "De 25 a 29 años", 
            "De 30 a 39 años",
            "De 40 a 44 años", 
            "De 45 a 49 años", 
            "De 50 a 59 años", 
            "De 60 a 64 años", 
            "Mayores de 65 años")
Poblacion_indigenas_DANE_2$Grupo_Etario <- cut(Poblacion_indigenas_DANE_2$Edad, breaks = lim, labels = labels, include.lowest = TRUE)
head(Poblacion_indigenas_DANE_2, 200)

# Agregacion de datos en funcion del grupo etario --------------------
Poblacion_indigenas_DANE_3 <- aggregate(cbind(Hombres, Mujeres, Total) ~ Departamento + Grupo_Etario, Poblacion_indigenas_DANE_2, sum)
head(Poblacion_indigenas_DANE_3, 12)

# Filtrado mayores de 18 años --------------------
Poblacion_indigenas_DANE_4 <- Poblacion_indigenas_DANE_3 %>% filter(Grupo_Etario != "De 0 a 17 años")
head(Poblacion_indigenas_DANE_4, 11)

# Totales generales para la nueva poblacion --------------------
# Subtotales renombrados como totales generales para la poblacion mayor de 18 años por departamento
Poblacion_indigenas_DANE_5 <- aggregate(cbind(Hombres, Mujeres, Total) ~ Departamento, Poblacion_indigenas_DANE_4, sum)
head(Poblacion_indigenas_DANE_5, 11)

# Anexar la variable Grupo etario a la base de totales generales --------------------
Poblacion_indigenas_DANE_5$Grupo_Etario <- rep("Total general", dim(Poblacion_indigenas_DANE_5)[1])

# Organizar columnas --------------------
Poblacion_indigenas_DANE_5 <- Poblacion_indigenas_DANE_5[, c(1, 5, 2:4)]
head(Poblacion_indigenas_DANE_5, 10)

# Unir los datos de totales generales a la base filtrado para mayores de 18 años --------------------
Poblacion_indigenas_DANE_6 <- rbind(Poblacion_indigenas_DANE_4, Poblacion_indigenas_DANE_5)
head(Poblacion_indigenas_DANE_6)

# Ordenar la base por departamento --------------------
Poblacion_indigenas_DANE_6 <- Poblacion_indigenas_DANE_6[order(Poblacion_indigenas_DANE_6$Departamento), ]
head(Poblacion_indigenas_DANE_6, 20)

# Save
writexl::write_xlsx(Poblacion_indigenas_DANE_6, "Indicadores_indigenas_DANE 20231106.xlsx")

########################################################################
###################### Población DANE: room ############################
########################################################################

# Lectura de archivos --------------------
Poblacion_room_DANE <- read_excel("C:\\Users\\\\Datos\\DANE\\Demografía y población\\Autoreconocimiento étnico\\Autoreconocimiento_étnico_CNPV_2018.xlsx", sheet = "Gitano(a) o Rrom")
Poblacion_room_DANE <- as.data.frame(Poblacion_room_DANE)
head(Poblacion_room_DANE)

# Eliminar las filas de los totales generales --------------------
Poblacion_room_DANE_2 <- Poblacion_room_DANE %>% filter(Edad != "Total")
head(Poblacion_room_DANE_2, 200)

# Clasificación por grupo etario --------------------

# Convertir la variable Edad en variable numérica
Poblacion_room_DANE_2$Edad <- as.numeric(Poblacion_room_DANE_2$Edad)
any(is.na(Poblacion_room_DANE_2$Edad))
head(Poblacion_room_DANE_2, 200)

# Crear los labels y limites
lim <- c(0, 17, 19, 24, 29, 39, 44, 49, 59, 64, Inf)
labels <- c("De 0 a 17 años",
            "De 18 a 19 años", 
            "De 20 a 24 años", 
            "De 25 a 29 años", 
            "De 30 a 39 años",
            "De 40 a 44 años", 
            "De 45 a 49 años", 
            "De 50 a 59 años", 
            "De 60 a 64 años", 
            "Mayores de 65 años")
Poblacion_room_DANE_2$Grupo_Etario <- cut(Poblacion_room_DANE_2$Edad, breaks = lim, labels = labels, include.lowest = TRUE)
head(Poblacion_room_DANE_2, 200)

# Agregacion de datos en funcion del grupo etario --------------------
Poblacion_room_DANE_3 <- aggregate(cbind(Hombres, Mujeres, Total) ~ Departamento + Grupo_Etario, Poblacion_room_DANE_2, sum)
head(Poblacion_room_DANE_3, 12)

# Filtrado mayores de 18 años --------------------
Poblacion_room_DANE_4 <- Poblacion_room_DANE_3 %>% filter(Grupo_Etario != "De 0 a 17 años")
head(Poblacion_room_DANE_4, 11)

# Totales generales para la nueva poblacion --------------------
# Subtotales renombrados como totales generales para la poblacion mayor de 18 años por departamento
Poblacion_room_DANE_5 <- aggregate(cbind(Hombres, Mujeres, Total) ~ Departamento, Poblacion_room_DANE_4, sum)
head(Poblacion_room_DANE_5, 11)

# Anexar la variable Grupo etario a la base de totales generales --------------------
Poblacion_room_DANE_5$Grupo_Etario <- rep("Total general", dim(Poblacion_room_DANE_5)[1])

# Organizar columnas --------------------
Poblacion_room_DANE_5 <- Poblacion_room_DANE_5[, c(1, 5, 2:4)]
head(Poblacion_room_DANE_5, 10)

# Unir los datos de totales generales a la base filtrado para mayores de 18 años --------------------
Poblacion_room_DANE_6 <- rbind(Poblacion_room_DANE_4, Poblacion_room_DANE_5)
head(Poblacion_room_DANE_6)

# Ordenar la base por departamento --------------------
Poblacion_room_DANE_6 <- Poblacion_room_DANE_6[order(Poblacion_room_DANE_6$Departamento), ]
head(Poblacion_room_DANE_6, 20)

# Save
writexl::write_xlsx(Poblacion_room_DANE_6, "Indicadores_room_DANE 20231106.xlsx")

########################################################################
###################### Población DANE: raizal ##########################
########################################################################

# Lectura de archivos --------------------
Poblacion_raizal_DANE <- read_excel("C:\\Users\\\\Datos\\DANE\\Demografía y población\\Autoreconocimiento étnico\\Autoreconocimiento_étnico_CNPV_2018.xlsx", sheet = "Raizal del Archipielago de San")
Poblacion_raizal_DANE <- as.data.frame(Poblacion_raizal_DANE)
head(Poblacion_raizal_DANE)

# Eliminar las filas de los totales generales --------------------
Poblacion_raizal_DANE_2 <- Poblacion_raizal_DANE %>% filter(Edad != "Total")
head(Poblacion_raizal_DANE_2, 200)

# Clasificación por grupo etario --------------------

# Convertir la variable Edad en variable numérica
Poblacion_raizal_DANE_2$Edad <- as.numeric(Poblacion_raizal_DANE_2$Edad)
any(is.na(Poblacion_raizal_DANE_2$Edad))
head(Poblacion_raizal_DANE_2, 200)

# Crear los labels y limites
lim <- c(0, 17, 19, 24, 29, 39, 44, 49, 59, 64, Inf)
labels <- c("De 0 a 17 años",
            "De 18 a 19 años", 
            "De 20 a 24 años", 
            "De 25 a 29 años", 
            "De 30 a 39 años",
            "De 40 a 44 años", 
            "De 45 a 49 años", 
            "De 50 a 59 años", 
            "De 60 a 64 años", 
            "Mayores de 65 años")
Poblacion_raizal_DANE_2$Grupo_Etario <- cut(Poblacion_raizal_DANE_2$Edad, breaks = lim, labels = labels, include.lowest = TRUE)
head(Poblacion_raizal_DANE_2, 200)

# Agregacion de datos en funcion del grupo etario --------------------
Poblacion_raizal_DANE_3 <- aggregate(cbind(Hombres, Mujeres, Total) ~ Departamento + Grupo_Etario, Poblacion_raizal_DANE_2, sum)
head(Poblacion_raizal_DANE_3, 12)

# Filtrado mayores de 18 años --------------------
Poblacion_raizal_DANE_4 <- Poblacion_raizal_DANE_3 %>% filter(Grupo_Etario != "De 0 a 17 años")
head(Poblacion_raizal_DANE_4, 11)

# Totales generales para la nueva poblacion --------------------
# Subtotales renombrados como totales generales para la poblacion mayor de 18 años por departamento
Poblacion_raizal_DANE_5 <- aggregate(cbind(Hombres, Mujeres, Total) ~ Departamento, Poblacion_raizal_DANE_4, sum)
head(Poblacion_raizal_DANE_5, 11)

# Anexar la variable Grupo etario a la base de totales generales --------------------
Poblacion_raizal_DANE_5$Grupo_Etario <- rep("Total general", dim(Poblacion_raizal_DANE_5)[1])

# Organizar columnas --------------------
Poblacion_raizal_DANE_5 <- Poblacion_raizal_DANE_5[, c(1, 5, 2:4)]
head(Poblacion_raizal_DANE_5, 10)

# Unir los datos de totales generales a la base filtrado para mayores de 18 años --------------------
Poblacion_raizal_DANE_6 <- rbind(Poblacion_raizal_DANE_4, Poblacion_raizal_DANE_5)
head(Poblacion_raizal_DANE_6)

# Ordenar la base por departamento --------------------
Poblacion_raizal_DANE_6 <- Poblacion_raizal_DANE_6[order(Poblacion_raizal_DANE_6$Departamento), ]
head(Poblacion_raizal_DANE_6, 20)

# Save
writexl::write_xlsx(Poblacion_raizal_DANE_6, "Indicadores_raizal_DANE 20231106.xlsx")

########################################################################
###################### Población DANE: palenquera ######################
########################################################################

# Lectura de archivos --------------------
Poblacion_palenquera_DANE <- read_excel("C:\\Users\\\\Datos\\DANE\\Demografía y población\\Autoreconocimiento étnico\\Autoreconocimiento_étnico_CNPV_2018.xlsx", sheet = "Palenquero(a) de San Basilio")
Poblacion_palenquera_DANE <- as.data.frame(Poblacion_palenquera_DANE)
head(Poblacion_palenquera_DANE)

# Eliminar las filas de los totales generales --------------------
Poblacion_palenquera_DANE_2 <- Poblacion_palenquera_DANE %>% filter(Edad != "Total")
head(Poblacion_palenquera_DANE_2, 200)

# Clasificación por grupo etario --------------------

# Convertir la variable Edad en variable numérica
Poblacion_palenquera_DANE_2$Edad <- as.numeric(Poblacion_palenquera_DANE_2$Edad)
any(is.na(Poblacion_palenquera_DANE_2$Edad))
head(Poblacion_palenquera_DANE_2, 200)

# Crear los labels y limites
lim <- c(0, 17, 19, 24, 29, 39, 44, 49, 59, 64, Inf)
labels <- c("De 0 a 17 años",
            "De 18 a 19 años", 
            "De 20 a 24 años", 
            "De 25 a 29 años", 
            "De 30 a 39 años",
            "De 40 a 44 años", 
            "De 45 a 49 años", 
            "De 50 a 59 años", 
            "De 60 a 64 años", 
            "Mayores de 65 años")
Poblacion_palenquera_DANE_2$Grupo_Etario <- cut(Poblacion_palenquera_DANE_2$Edad, breaks = lim, labels = labels, include.lowest = TRUE)
head(Poblacion_palenquera_DANE_2, 200)

# Agregacion de datos en funcion del grupo etario --------------------
Poblacion_palenquera_DANE_3 <- aggregate(cbind(Hombres, Mujeres, Total) ~ Departamento + Grupo_Etario, Poblacion_palenquera_DANE_2, sum)
head(Poblacion_palenquera_DANE_3, 12)

# Filtrado mayores de 18 años --------------------
Poblacion_palenquera_DANE_4 <- Poblacion_palenquera_DANE_3 %>% filter(Grupo_Etario != "De 0 a 17 años")
head(Poblacion_palenquera_DANE_4, 11)

# Totales generales para la nueva poblacion --------------------
# Subtotales renombrados como totales generales para la poblacion mayor de 18 años por departamento
Poblacion_palenquera_DANE_5 <- aggregate(cbind(Hombres, Mujeres, Total) ~ Departamento, Poblacion_palenquera_DANE_4, sum)
head(Poblacion_palenquera_DANE_5, 11)

# Anexar la variable Grupo etario a la base de totales generales --------------------
Poblacion_palenquera_DANE_5$Grupo_Etario <- rep("Total general", dim(Poblacion_palenquera_DANE_5)[1])

# Organizar columnas --------------------
Poblacion_palenquera_DANE_5 <- Poblacion_palenquera_DANE_5[, c(1, 5, 2:4)]
head(Poblacion_palenquera_DANE_5, 10)

# Unir los datos de totales generales a la base filtrado para mayores de 18 años --------------------
Poblacion_palenquera_DANE_6 <- rbind(Poblacion_palenquera_DANE_4, Poblacion_palenquera_DANE_5)
head(Poblacion_palenquera_DANE_6)

# Ordenar la base por departamento --------------------
Poblacion_palenquera_DANE_6 <- Poblacion_palenquera_DANE_6[order(Poblacion_palenquera_DANE_6$Departamento), ]
head(Poblacion_palenquera_DANE_6, 20)

# Save
writexl::write_xlsx(Poblacion_palenquera_DANE_6, "Indicadores_palenquera_DANE 20231106.xlsx")

########################################################################
###################### Población DANE: afro ############################
########################################################################

# Lectura de archivos --------------------
Poblacion_afro_DANE <- read_excel("C:\\Users\\\\Datos\\DANE\\Demografía y población\\Autoreconocimiento étnico\\Autoreconocimiento_étnico_CNPV_2018.xlsx", sheet = "Negro(a), Mulato(a), Afrodesce")
Poblacion_afro_DANE <- as.data.frame(Poblacion_afro_DANE)
head(Poblacion_afro_DANE)

# Eliminar las filas de los totales generales --------------------
Poblacion_afro_DANE_2 <- Poblacion_afro_DANE %>% filter(Edad != "Total")
head(Poblacion_afro_DANE_2, 200)

# Clasificación por grupo etario --------------------

# Convertir la variable Edad en variable numérica
Poblacion_afro_DANE_2$Edad <- as.numeric(Poblacion_afro_DANE_2$Edad)
any(is.na(Poblacion_afro_DANE_2$Edad))
head(Poblacion_afro_DANE_2, 200)

# Crear los labels y limites
lim <- c(0, 17, 19, 24, 29, 39, 44, 49, 59, 64, Inf)
labels <- c("De 0 a 17 años",
            "De 18 a 19 años", 
            "De 20 a 24 años", 
            "De 25 a 29 años", 
            "De 30 a 39 años",
            "De 40 a 44 años", 
            "De 45 a 49 años", 
            "De 50 a 59 años", 
            "De 60 a 64 años", 
            "Mayores de 65 años")
Poblacion_afro_DANE_2$Grupo_Etario <- cut(Poblacion_afro_DANE_2$Edad, breaks = lim, labels = labels, include.lowest = TRUE)
head(Poblacion_afro_DANE_2, 200)

# Agregacion de datos en funcion del grupo etario --------------------
Poblacion_afro_DANE_3 <- aggregate(cbind(Hombres, Mujeres, Total) ~ Departamento + Grupo_Etario, Poblacion_afro_DANE_2, sum)
head(Poblacion_afro_DANE_3, 12)

# Filtrado mayores de 18 años --------------------
Poblacion_afro_DANE_4 <- Poblacion_afro_DANE_3 %>% filter(Grupo_Etario != "De 0 a 17 años")
head(Poblacion_afro_DANE_4, 11)

# Totales generales para la nueva poblacion --------------------
# Subtotales renombrados como totales generales para la poblacion mayor de 18 años por departamento
Poblacion_afro_DANE_5 <- aggregate(cbind(Hombres, Mujeres, Total) ~ Departamento, Poblacion_afro_DANE_4, sum)
head(Poblacion_afro_DANE_5, 11)

# Anexar la variable Grupo etario a la base de totales generales --------------------
Poblacion_afro_DANE_5$Grupo_Etario <- rep("Total general", dim(Poblacion_afro_DANE_5)[1])

# Organizar columnas --------------------
Poblacion_afro_DANE_5 <- Poblacion_afro_DANE_5[, c(1, 5, 2:4)]
head(Poblacion_afro_DANE_5, 10)

# Unir los datos de totales generales a la base filtrado para mayores de 18 años --------------------
Poblacion_afro_DANE_6 <- rbind(Poblacion_afro_DANE_4, Poblacion_afro_DANE_5)
head(Poblacion_afro_DANE_6)

# Ordenar la base por departamento --------------------
Poblacion_afro_DANE_6 <- Poblacion_afro_DANE_6[order(Poblacion_afro_DANE_6$Departamento), ]
head(Poblacion_afro_DANE_6, 20)

# Save
writexl::write_xlsx(Poblacion_afro_DANE_6, "Indicadores_afro_DANE 20231106.xlsx")

########################################################################
################### Población DANE: Totales ############################
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

# Lectura de archivos --------------------

# Indigenas
Poblacion_indigenas_DANE <- read_excel("C:\\Users\\\\Datos\\DANE\\Demografía y población\\Autoreconocimiento étnico\\Autoreconocimiento_étnico_CNPV_2018.xlsx", sheet = "Indígena")
Poblacion_indigenas_DANE <- as.data.frame(Poblacion_indigenas_DANE)
Poblacion_indigenas_DANE$Pertenencia_etnica <- rep("Indígena", dim(Poblacion_indigenas_DANE)[1])
# Room
Poblacion_room_DANE <- read_excel("C:\\Users\\\\Datos\\DANE\\Demografía y población\\Autoreconocimiento étnico\\Autoreconocimiento_étnico_CNPV_2018.xlsx", sheet = "Gitano(a) o Rrom")
Poblacion_room_DANE <- as.data.frame(Poblacion_room_DANE)
Poblacion_room_DANE$Pertenencia_etnica <- rep("Rom (Gitano)", dim(Poblacion_room_DANE)[1])
# Raizal
Poblacion_raizal_DANE <- read_excel("C:\\Users\\\\Datos\\DANE\\Demografía y población\\Autoreconocimiento étnico\\Autoreconocimiento_étnico_CNPV_2018.xlsx", sheet = "Raizal del Archipielago de San")
Poblacion_raizal_DANE <- as.data.frame(Poblacion_raizal_DANE)
Poblacion_raizal_DANE$Pertenencia_etnica <- rep("Raizal del archipiélago de San Andrés y Providencia", dim(Poblacion_raizal_DANE)[1])
# Palequera
Poblacion_palenquera_DANE <- read_excel("C:\\Users\\\\Datos\\DANE\\Demografía y población\\Autoreconocimiento étnico\\Autoreconocimiento_étnico_CNPV_2018.xlsx", sheet = "Palenquero(a) de San Basilio")
Poblacion_palenquera_DANE <- as.data.frame(Poblacion_palenquera_DANE)
Poblacion_palenquera_DANE$Pertenencia_etnica <- rep("Palenquero de San Basilio", dim(Poblacion_palenquera_DANE)[1])
# Afro
Poblacion_afro_DANE <- read_excel("C:\\Users\\\\Datos\\DANE\\Demografía y población\\Autoreconocimiento étnico\\Autoreconocimiento_étnico_CNPV_2018.xlsx", sheet = "Negro(a), Mulato(a), Afrodesce")
Poblacion_afro_DANE <- as.data.frame(Poblacion_afro_DANE)
Poblacion_afro_DANE$Pertenencia_etnica <- rep("Negro(a), mulato(a), afrocolombiano(a) o afrodescendiente", dim(Poblacion_afro_DANE)[1])

# Combinacion del grupo etnico --------------------
Poblacion_grupoetnico_DANE <- rbind(Poblacion_indigenas_DANE, Poblacion_room_DANE, Poblacion_raizal_DANE, Poblacion_palenquera_DANE, Poblacion_afro_DANE)

# Filtrado por totales generales para cada grupo etnico --------------------
Poblacion_grupoetnico_DANE_1 <- Poblacion_grupoetnico_DANE %>% filter(Edad == "Total")
Poblacion_grupoetnico_DANE_2 <- Poblacion_grupoetnico_DANE_1[, -1]
Poblacion_grupoetnico_DANE_2 <- Poblacion_grupoetnico_DANE_2[, c(5, 4, 1, 2, 3)]
Poblacion_grupoetnico_DANE_2 <- Poblacion_grupoetnico_DANE_2[order(Poblacion_grupoetnico_DANE_2$Departamento), ]

# Save
writexl::write_xlsx(Poblacion_grupoetnico_DANE_2, "Indicadores_etnicos_DANE 20231106.xlsx")
