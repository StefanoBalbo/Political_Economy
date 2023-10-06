############################### ECO POL 2023 ##################################
############################### DATOS MESAS ###################################
rm(list=ls())

directorio <- "/Users/IDECOR/Documents/Code/Political_Economy/datos_mesas/"
setwd(directorio)
getwd()

datos <- read.csv("votacion.csv")
names(datos)
head(datos)

agrupaciones <- read.csv("agrupacion.csv")
head(agrupaciones)

listas <- read.csv("lista.csv")
head(listas)

mesas <- read.csv("mesa.csv")
head(mesas)

cargos <- read.csv("cargo.csv")
cargos

tdv <- read.csv("tipovoto.csv")
tdv

elecciontipo <- read.csv("eleccion.csv")
elecciontipo

circuito <-  read.csv("circuito.csv")
head(circuito)

provincias <- read.csv("distrito.csv")
table(provincias$nombre); names(provincias)

seccion <- read.csv("seccion.csv")
head(seccion)

seccion_prov <- read.csv("seccionprovincial.csv")
head(seccion_prov)

###############################################################################

# Sumamos nombres distritos
names(datos)
names(provincias)

library(dplyr)
datos_provincias <- left_join(datos, provincias, by = "distrito_id")
names(datos_provincias)

datos_provincias <- datos_provincias[, -c(12, 13)]
datos_provincias <- rename(datos_provincias, distrito_nombre = nombre)
head(datos_provincias); rm(datos); rm(provincias)


# Sumamos nombres cargos
cargos
table(datos_provincias$cargo_id)

datos_provincias$cargo_nombre <- case_when(datos_provincias$cargo_id == 1 ~ "Presidente",
                                    datos_provincias$cargo_id == 2 ~ "Sen_Nac",
                                    datos_provincias$cargo_id == 3 ~ "Dip_Nac",
                                    datos_provincias$cargo_id == 4 ~ "Gobernador",
                                    datos_provincias$cargo_id == 5 ~ "Sen_Prov",
                                    datos_provincias$cargo_id == 6 ~ "Dip_Prov",
                                    datos_provincias$cargo_id == 7 ~ "Intendente",
                                    datos_provincias$cargo_id == 8 ~ "Mercosur_Nac",
                                    datos_provincias$cargo_id == 9 ~ "Mercosur_Prov",
                                    datos_provincias$cargo_id == 10 ~ "Concejales")

table(datos_provincias$cargo_nombre)
head(datos_provincias)
rm(cargos)


# Elección tipo y año

head(elecciontipo)
elecciontipo <- elecciontipo[, -c(2, 7)]; names(elecciontipo)

datos <- left_join(datos_provincias, elecciontipo, by = "eleccion_id")
head(datos)
names(datos); rm(datos_provincias, elecciontipo)


# Elección tipo y año

names(datos)
head(tdv)

datos2 <- left_join(datos, tdv, by = "tipovoto_id")
head(datos2); tdv

datos2 <- rename(datos2, tdv = nombre); names(datos2)

datos2 <- datos2[, -c(18)]

head(datos2)

datos <- datos2; rm(datos2, tdv); names(datos)

############################################################################
rm(circuito, listas, seccion, seccion_prov)

head(datos)
table(datos$cargo_nombre)
table(datos$eleccion_tipo)
table(datos$tdv)
table(datos$distrito_nombre)

############################################################################

names(agrupaciones)
table(agrupaciones$agrupacion_id)
table(agrupaciones$nombre)




