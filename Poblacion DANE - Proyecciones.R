########################################################################
# Code Objective			    : Melt - Poblacion DANE
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
##################### Población 2018 - 2023 ############################
########################################################################

# """ Esta sección del código depura los datos de la población de Colombia 
# para los años 2018-2019. La depuración implica la transformación de la base 
# de datos a un formato largo, la agrupación de los datos en grupos etarios 
# previamente establecidos y la aplicación de un filtro para seleccionar 
# únicamente la población mayor de 18 años, así como el calculo del total general."""

# Lectura de archivos --------------------
Poblacion_DANE <- read_excel("C:/Users/ASUS TUF/DANE/Demografía y población//Proyecciónes de poblacion//ProyeccionesPD - Formato ancho 20231103.xlsx", sheet = "2018-2023")
Poblacion_DANE <- as.data.frame(Poblacion_DANE)
head(Poblacion_DANE)

# Convertir los datos en formato largo --------------------

# Utilizar la funcion melt transformar la base en formato largo 
Poblacion_DANE_melt <- reshape2::melt(Poblacion_DANE, id.vars = "Sexo_Edad")
head(Poblacion_DANE_melt)

# Dividir la variable Sexo_Edad entre Sexo y Edad 
Poblacion_DANE_melt_2 <- cbind(Poblacion_DANE_melt, str_split(Poblacion_DANE_melt$Sexo_Edad, "_", simplify = TRUE))
colnames(Poblacion_DANE_melt_2)[-c(1:3)] <- c("Sexo", "Edad")
head(Poblacion_DANE_melt_2)                            

# Dividir la variable información encabezado entre "DP", "DPNOM", "Año", "Área Geográfica"
Poblacion_DANE_melt_3 <- cbind(Poblacion_DANE_melt_2, str_split(Poblacion_DANE_melt_2$variable, "_", simplify = TRUE))
colnames(Poblacion_DANE_melt_3)[-c(1:5)] <- c("DP", "DPNOM", "Año", "Area_Geografica")
head(Poblacion_DANE_melt_3)

# Seleccionar las variables de interés
Poblacion_DANE_melt_4 <- Poblacion_DANE_melt_3[, c("Sexo", "Edad", "DP", "DPNOM", "Año", "Area_Geografica", "value")]

# Convertir la variable Edad en variable numérica
Poblacion_DANE_melt_4$Edad <- as.numeric(Poblacion_DANE_melt_4$Edad)
head(Poblacion_DANE_melt_4)

# Agregacion datos en funcion de las variables Hombres y Mujeres --------------------
Poblacion_DANE_melt_5 <- reshape2::dcast(Poblacion_DANE_melt_4, DPNOM + DP + Año + Area_Geografica + Edad ~ Sexo, fun.aggregate = sum)
head(Poblacion_DANE_melt_5)

# Clasificación por grupo etario --------------------
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
Poblacion_DANE_melt_5$Grupo_Etario <- cut(Poblacion_DANE_melt_5$Edad, breaks = lim, labels = labels, include.lowest = TRUE)
head(Poblacion_DANE_melt_5)

# Cambiar valor -1 por total general
Poblacion_DANE_melt_5$Edad <- ifelse(Poblacion_DANE_melt_5$Edad == -1, "Total general", Poblacion_DANE_melt_5$Edad)
Poblacion_DANE_melt_5$Grupo_Etario <- ifelse(Poblacion_DANE_melt_5$Edad == "Total general", "Total general", as.character(Poblacion_DANE_melt_5$Grupo_Etario))

# Cambiar de posicion el grupo etario
Poblacion_DANE_melt_5 <- Poblacion_DANE_melt_5[c(1:5, 9, 6:8)]
head(Poblacion_DANE_melt_5)

# Agregacion de datos en funcion del grupo etario --------------------
Poblacion_DANE_melt_6 <- aggregate(cbind(Hombres, Mujeres, Total) ~ DPNOM + DP + Año + Area_Geografica + Grupo_Etario, Poblacion_DANE_melt_5, sum)

# Ordenar la base por departamento
Poblacion_DANE_melt_6 <- Poblacion_DANE_melt_6[order(Poblacion_DANE_melt_6$DPNOM, Poblacion_DANE_melt_6$Año, Poblacion_DANE_melt_6$Area_Geografica), ]
head(Poblacion_DANE_melt_6)

# Guardar totales generales
write.xlsx(Poblacion_DANE_melt_6 %>% filter(Area_Geografica == "Total", Grupo_Etario == "Total general"),
           "Totales generales DANE 20231220.xlsx")

########################################################################
################### Totales generales >= 18 ############################
########################################################################

# Filtrado mayores de 18 años --------------------
Poblacion_DANE_melt_7 <- Poblacion_DANE_melt_6 %>% filter(Grupo_Etario != "De 0 a 17 años", Grupo_Etario != "Total general")
head(Poblacion_DANE_melt_7, 11)

# Totales generales para la nueva poblacion --------------------
# Subtotales renombrados como totales generales para la poblacion mayor de 18 años por año, deparmento, etc...
Poblacion_DANE_melt_8 <- aggregate(cbind(Hombres, Mujeres, Total) ~ DPNOM + DP + Año + Area_Geografica, Poblacion_DANE_melt_7, sum)

# Anexar la variable Grupo etario a la base de totales generales --------------------
Poblacion_DANE_melt_8$Grupo_Etario <- rep("Total general", dim(Poblacion_DANE_melt_8)[1])

# Organizar columnas --------------------
Poblacion_DANE_melt_8 <- Poblacion_DANE_melt_8[, c(1:4, 8, 5:7)]
head(Poblacion_DANE_melt_8, 10)

# Unir los datos de totales generales a la base filtrado para mayores de 18 años --------------------
Poblacion_DANE_melt_9 <- rbind(Poblacion_DANE_melt_7, Poblacion_DANE_melt_8)

# Ordenar la base por departamento --------------------
Poblacion_DANE_melt_9 <- Poblacion_DANE_melt_9[order(Poblacion_DANE_melt_9$DPNOM, Poblacion_DANE_melt_9$Año, Poblacion_DANE_melt_9$Area_Geografica, Poblacion_DANE_melt_9$Grupo_Etario), ]
head(Poblacion_DANE_melt_9, 10)
View(Poblacion_DANE_melt_9)

rm(labels, lim, Poblacion_DANE, Poblacion_DANE_melt, Poblacion_DANE_melt_2, 
   Poblacion_DANE_melt_3, Poblacion_DANE_melt_4, Poblacion_DANE_melt_5,
   Poblacion_DANE_melt_6, Poblacion_DANE_melt_7, Poblacion_DANE_melt_8)

# Guardar enterno --------------------
save.image("C:\\Users\\ASUS TUF\\DANE\\Demografía y población\\Proyecciónes de poblacion\\ProyeccionesPD - Formato largo 20231103.RData")

# Guardar archivo --------------------
# writexl::write_xlsx(Poblacion_DANE_melt_9, "Indicadores_DANE 20231106.xlsx")

# # Guardar archivo
# wb <- loadWorkbook("Indicadores_DANE 20231105.xlsx")
# addWorksheet(wb, sheetName = "Población total")
# writeData(wb, sheet = "Población total", x = Poblacion_DANE_melt_5)
# saveWorkbook(wb, file = "Indicadores_DANE 20231106.xlsx")