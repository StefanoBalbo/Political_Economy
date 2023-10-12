############################### ECO POL 2023 ##################################
rm(list=ls())

library(dplyr)
library(data.table)

############################### DATOS MESAS ###################################

#directorio <- "/Users/IDECOR/Documents/Code/Political_Economy/datos_mesas/"
directorio <- "/Users/stefa/Documents/Code/Political_Economy/datos_mesas/"
setwd(directorio)
getwd()

{
  print("Importamos los datos")
datos <- fread(paste0(directorio, "votacion.csv"), sep = ",", dec = ".")
agrupaciones <- fread(paste0(directorio, "agrupacion.csv"), sep = ",", dec = ".")
listas <- fread(paste0(directorio, "lista.csv"), sep = ",", dec = ".")
mesas <-  fread(paste0(directorio, "mesa.csv"), sep = ",", dec = ".")
cargos <-  fread(paste0(directorio, "cargo.csv"), sep = ",", dec = ".")
tdv <-  fread(paste0(directorio, "tipovoto.csv"), sep = ",", dec = ".")
elecciontipo <-  fread(paste0(directorio, "eleccion.csv"), sep = ",", dec = ".")
circuito <- fread(paste0(directorio, "circuito.csv"), sep = ",", dec = ".")
provincias <- fread(paste0(directorio, "distrito.csv"), sep = ",", dec = ".")
seccion <- fread(paste0(directorio, "seccion.csv"), sep = ",", dec = ".")
seccion_prov <-fread(paste0(directorio, "seccionprovincial.csv"), sep = ",", dec = ".")
}

head(datos)
head(agrupaciones)
head(listas)
head(mesas)
cargos
tdv
elecciontipo
head(circuito)
table(provincias$nombre); names(provincias)
head(seccion)
head(seccion_prov)

###############################################################################

# Sumamos nombres distritos
names(datos)
names(provincias)

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
table(datos$eleccion_tipo)

datos$eleccion_tipo <- ifelse(datos$eleccion_tipo == "GENERAL", "GENERALES", datos$eleccion_tipo)

table(datos$eleccion_tipo)

table(datos$tdv)
table(datos$distrito_nombre)

getwd()
saveRDS(datos, "datos_sinagrup", compress = FALSE)
#write.csv(datos, "datos.csv")


# FALTA DEFINIR AGRUPACIONES
names(agrupaciones) ################################################ !
################################################


############################# FILTRAR DATOS ###########################

names(datos)
head(datos)
table(datos$cargo_nombre)

{
  print("Filtramos elecciones por cargo")
presidencial <- datos %>%
  filter(cargo_nombre == "Presidente")

gobernador <- datos %>%
  filter(cargo_nombre == "Gobernador")
}

names(datos)
table(datos$eleccion_tipo)

{
  print("Filtramos elecciones por tipo")
PASO <- datos %>%
  filter(eleccion_tipo == "PASO")

GENERALES <- datos %>%
  filter(eleccion_tipo == "GENERALES")

BALLOTAGE <- datos %>%
  filter(eleccion_tipo == "BALLOTAGE")
}

table(datos$anio)
{
  print("Filtramos elecciones por año")

elecciones2011 <- datos %>%
  filter(anio == "2011")

elecciones2013 <- datos %>%
  filter(anio == "2013")

elecciones2015 <- datos %>%
  filter(anio == "2015")

elecciones2017 <- datos %>%
  filter(anio == "2017")

elecciones2019 <- datos %>%
  filter(anio == "2019")

elecciones2021 <- datos %>%
  filter(anio == "2021")

elecciones2023 <- datos %>%
  filter(anio == "2023")
}


######################## FILTRO MIXTO ########################

PASO2019 <- datos %>%
  filter(eleccion_tipo == "PASO") %>%
  filter(anio == "2019") %>%
  filter(cargo_nombre == "Presidente")
head(PASO2019)
