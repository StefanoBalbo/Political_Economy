############################### ECO POL 2023 ##################################
rm(list=ls())

library(dplyr)
library(data.table)
library(tidyverse)
library(rvest)
library(janitor)
library(rio)
library(here)

############################### DATOS MESAS ###################################

directorio <- "/Users/IDECOR/Documents/Code/Political_Economy/eco pol 2023/datos_mesas/"
#directorio <- "/Users/stefa/Documents/Code/Political_Economy/eco pol 2023/datos_mesas/"
setwd(directorio); getwd()

{
  print("Importamos los datos")
  datos <- import("votacion.csv", setclass =" data.table", encoding="UTF-8")
  provincias <- import("distrito.csv",setclass="data.table",encoding="UTF-8")
  cargos <- import("cargo.csv",setclass="data.table",encoding="UTF-8")
  tdv <- import("tipovoto.csv",setclass="data.table",encoding="UTF-8")
  elecciontipo <- import("eleccion.csv",setclass="data.table",encoding="UTF-8")
  agrupaciones <- import("agrupacion.csv",setclass="data.table",encoding="UTF-8")
  seccion<-import("seccion.csv",setclass="data.table",encoding="UTF-8")
#listas <- fread(paste0(directorio, "lista.csv"), sep = ",", dec = ".")
#mesas <-  fread(paste0(directorio, "mesa.csv"), sep = ",", dec = ".")
#circuito <- fread(paste0(directorio, "circuito.csv"), sep = ",", dec = ".")
#seccion_prov <-fread(paste0(directorio, "seccionprovincial.csv"), sep = ",", dec = ".")
}

head(datos)
head(agrupaciones)
#head(listas)
#head(mesas)
cargos
tdv
elecciontipo
#head(circuito)
table(provincias$nombre); names(provincias)
head(seccion)
#head(seccion_prov)


############################### PREPARACIÓN ###################################

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

# Tipo de voto
names(datos)
head(tdv)

datos2 <- left_join(datos, tdv, by = "tipovoto_id")
head(datos2); tdv

datos2 <- rename(datos2, tdv = nombre); names(datos2)

datos2 <- datos2[, -c(18)]

head(datos2)

datos <- datos2; rm(datos2, tdv); names(datos)

############################################################################
#rm(circuito, listas, seccion, seccion_prov, mesas)

head(datos)
table(datos$eleccion_tipo)

datos$eleccion_tipo <- ifelse(datos$eleccion_tipo == "GENERAL", "GENERALES", datos$eleccion_tipo); table(datos$eleccion_tipo)

table(datos$tdv)
table(datos$distrito_nombre)

getwd()
saveRDS(datos, "datos.rda", compress = FALSE)
#write.csv(datos, "datos.csv")

#rm(list=ls())

############################# ELECCIONES PRESIDENCIALES #############################

names(agrupaciones)
agrupaciones <- agrupaciones[, -4]
names(datos)

head(agrupaciones)
head(datos)

table(datos$cargo_nombre)

{
  print("Filtramos elecciones presidenciales")
  datos_presidencial <- datos %>%
    filter(cargo_nombre == "Presidente")
}

head(datos_presidencial)

{
# Unificación nombres y recategorización
agrupaciones$nombre <- gsub("ALIANZA COMPROMISO FEDERAL", "Alianza Compromiso Federal", agrupaciones$nombre, fixed=TRUE)
agrupaciones$nombre <- gsub("ALIANZA FRENTE DE IZQUIERDA Y DE LOS TRABAJADORES", "Alianza Frente de Izquierda y de los Trabajadores", agrupaciones$nombre, fixed=TRUE)
agrupaciones$nombre <- gsub("ALIANZA FRENTE AMPLIO PROGRESISTA", "Alianza Frente Amplio Progresista", agrupaciones$nombre, fixed=TRUE)
agrupaciones$nombre <- gsub("ALIANZA FRENTE PARA LA VICTORIA", "Alianza Frente para la Victoria", agrupaciones$nombre, fixed=TRUE)
agrupaciones$nombre <- gsub("ALIANZA FRENTE POPULAR", "Alianza Frente Popular", agrupaciones$nombre, fixed=TRUE)
agrupaciones$nombre <- gsub("ALIANZA UNION PARA EL DESARROLLO SOCIAL - UDESO", "Alianza Unión para el Desarrollo Social", agrupaciones$nombre, fixed=TRUE)
agrupaciones$nombre <- gsub("COALICION CIVICA - AFIRMACION PARA UNA REPUBLICA IGUALITARIA (ARI)", "COALICION CIVICA ARI", agrupaciones$nombre, fixed=TRUE)
agrupaciones$nombre <- gsub("COALICIÓN CÍVICA - AFIRMACIÓN PARA UNA REPÚBLICA IGUALITARIA (ARI)", "COALICION CIVICA ARI", agrupaciones$nombre, fixed=TRUE)
agrupaciones$nombre <- gsub("Coalición Cívica - Afirmación para una República Igualitaria ARI", "COALICION CIVICA ARI", agrupaciones$nombre, fixed=TRUE)
agrupaciones$nombre <- gsub("COALICION CIVICA - ARI", "COALICION CIVICA ARI", agrupaciones$nombre, fixed=TRUE)
agrupaciones$nombre <- gsub("FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD", "Alianza Frente de Izquierda y de los Trabajadores", agrupaciones$nombre, fixed=TRUE)
agrupaciones$nombre <- gsub("FRENTE DE IZQUIERDA Y DE TRABAJADORES UNIDAD", "Alianza Frente de Izquierda y de los Trabajadores", agrupaciones$nombre, fixed=TRUE)
agrupaciones$nombre <- gsub("FRENTE PARA LA VICTORIA", "Alianza Frente para la Victoria", agrupaciones$nombre, fixed=TRUE)
agrupaciones$nombre <- gsub("MOVIMIENTO IZQUIERDA JUVENTUD DIGNIDAD", "MOVIMIENTO IZQUIERDA JUVENTUD Y DIGNIDAD", agrupaciones$nombre, fixed=TRUE)
agrupaciones$nombre <- gsub("MOVIMIENTO LIBRES DEL SUR", "Movimiento Libres del Sur", agrupaciones$nombre, fixed=TRUE)
agrupaciones$nombre <- gsub("MOVIMIENTO IZQUIERDA JUVENTUD DIGNIDAD", "MOVIMIENTO IZQUIERDA JUVENTUD Y DIGNIDAD", agrupaciones$nombre, fixed=TRUE)
agrupaciones$nombre <- gsub("MOVIMIENTO IZQUIERDA JUVENTUD DIGNIDAD", "MOVIMIENTO IZQUIERDA JUVENTUD Y DIGNIDAD", agrupaciones$nombre, fixed=TRUE)
}

head(agrupaciones)
head(datos_presidencial)

datos_agrupacion <- left_join(datos_presidencial, agrupaciones, by = c("agrupacion_id", "eleccion_id"))
head(datos_agrupacion)

datos_agrupacion <- rename(datos_agrupacion, agrupacion_nombre = nombre ); names(datos_agrupacion)

rm(agrupaciones)

{
  print("Recategorizamos las agrupaciones en 7 coaliciones")
datos_agrupacion$agrupacion_nombre <- ifelse(datos_agrupacion$agrupacion_nombre == "JUNTOS POR EL CAMBIO" | datos_agrupacion$agrupacion_nombre == "ALIANZA CAMBIEMOS" | datos_agrupacion$agrupacion_nombre == "CAMBIEMOS" | datos_agrupacion$agrupacion_nombre == "MOVIMIENTO DE ACCION VECINAL", "Cambiemos/Macrismo",
  ifelse(datos_agrupacion$agrupacion_nombre == "Alianza Frente para la Victoria" | datos_agrupacion$agrupacion_nombre == "ALIANZA FRENTE PARA LA VICTORIA" | datos_agrupacion$agrupacion_nombre == "FRENTE DE TODOS" | datos_agrupacion$agrupacion_nombre == "FRENTE PARA LA VICTORIA" | datos_agrupacion$agrupacion_nombre == "UNION POR LA PATRIA", "Kirchnerismo",
  ifelse(datos_agrupacion$agrupacion_nombre == "FRENTE LIBER.AR" | datos_agrupacion$agrupacion_nombre == "LA LIBERTAD AVANZA" | datos_agrupacion$agrupacion_nombre == "UNION DEL CENTRO DEMOCRATICO" | datos_agrupacion$agrupacion_nombre == "UNITE POR LA LIBERTAD Y LA DIGNIDAD", "Liberalismo",         
  ifelse(datos_agrupacion$agrupacion_nombre == "Alianza Compromiso Federal" | datos_agrupacion$agrupacion_nombre == "ALIANZA COMPROMISO FEDERAL" | datos_agrupacion$agrupacion_nombre == "Alianza Frente Popular" | datos_agrupacion$agrupacion_nombre == "ALIANZA FRENTE POPULAR" | datos_agrupacion$agrupacion_nombre == "ALIANZA UNIDOS POR UNA NUEVA ALTERNATIVA (UNA)" | datos_agrupacion$agrupacion_nombre == "Frente Entrerriano Federal" | datos_agrupacion$agrupacion_nombre == "HACEMOS POR NUESTRO PAIS" | datos_agrupacion$agrupacion_nombre == "PRINCIPIOS Y VALORES" | datos_agrupacion$agrupacion_nombre == "CONSENSO FEDERAL" | datos_agrupacion$agrupacion_nombre == "PARTIDO MOVIMIENTO DE ACCION  VECINAL", "Peronismo Federal/Tercera Via",              
  ifelse(datos_agrupacion$agrupacion_nombre == "Alianza Frente Amplio Progresista" | datos_agrupacion$agrupacion_nombre == "ALIANZA FRENTE AMPLIO PROGRESISTA" | datos_agrupacion$agrupacion_nombre == "ALIANZA PROGRESISTAS" | datos_agrupacion$agrupacion_nombre == "ALIANZA PROYECTO SUR" | datos_agrupacion$agrupacion_nombre == "Alianza Unión Para el Desarrollo Social" | datos_agrupacion$agrupacion_nombre == "ALIANZA UNION PARA EL DESARROLLO SOCIAL - UDESO" | datos_agrupacion$agrupacion_nombre == "COALICION CIVICA - AFIRMACION PARA UNA REPUBLICA IGUALITARIA (ARI)" | datos_agrupacion$agrupacion_nombre == "Coalición Cívica - Afirmación para una República Igualitaria ARI" | datos_agrupacion$agrupacion_nombre == "COALICION CIVICA - ARI" | datos_agrupacion$agrupacion_nombre == "COALICION CIVICA ARI" | datos_agrupacion$agrupacion_nombre == "Movimiento Libres del Sur" | datos_agrupacion$agrupacion_nombre == "MOVIMIENTO LIBRES DEL SUR" | datos_agrupacion$agrupacion_nombre == "PARTIDO SOCIAL DE CENTRO" | datos_agrupacion$agrupacion_nombre == "Alianza Unión para el Desarrollo Social" | datos_agrupacion$agrupacion_nombre == "COALICIÓN CÍVICA - AFIRMACIÓN PARA UNA REPÚBLICA IGUALITARIA (ARI)", "Progresismo/Socialdemocracia",              
  ifelse(datos_agrupacion$agrupacion_nombre == "Alianza Frente de Izquierda y de los Trabajadores" | datos_agrupacion$agrupacion_nombre == "ALIANZA FRENTE DE IZQUIERDA Y DE LOS TRABAJADORES" | datos_agrupacion$agrupacion_nombre == "FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD" | datos_agrupacion$agrupacion_nombre == "FRENTE DE IZQUIERDA Y DE TRABAJADORES UNIDAD" | datos_agrupacion$agrupacion_nombre == "MOVIMIENTO AL SOCIALISMO" | datos_agrupacion$agrupacion_nombre == "MST - NUEVA IZQUIERDA" | datos_agrupacion$agrupacion_nombre == "POLITICA OBRERA" | datos_agrupacion$agrupacion_nombre ==  "PROYECTO JOVEN", "Izquierda", "Conservadurismo/Nacionalismo"))))))
# In Else: "DEL CAMPO POPULAR", "FRENTE NOS", "FRENTE PATRIOTA","FRENTE PATRIOTA FEDERAL","MOVIMIENTO IZQUIERDA JUVENTUD DIGNIDAD","PARTIDO AUTONOMISTA","PARTIDO POPULAR","MOVIMIENTO IZQUIERDA JUVENTUD Y DIGNIDAD" <- "Conservadurismo/Nacionalismo"
}

table(datos_agrupacion$agrupacion_nombre)
head(datos_agrupacion)

datos_presidencial <- datos_agrupacion; rm(datos_agrupacion)

head(datos_presidencial)
table(datos_presidencial$eleccion_tipo)
table(datos_presidencial$anio)

# FILTRADO POR AÑOS ELECTORALES
{
  print("ELECCIONES 2011")
PASO2011 <- datos_presidencial %>%
  filter(eleccion_tipo == "PASO") %>%
  filter(anio == "2011")

GEN2011 <- datos_presidencial %>%
  filter(eleccion_tipo == "GENERALES") %>%
  filter(anio == "2011")

#BALL2011 <- datos_presidencial %>%
#  filter(eleccion_tipo == "BALLOTAGE") %>%
#  filter(anio == "2011")
}
################################
{
  print("ELECCIONES 2015")
PASO2015 <- datos_presidencial %>%
  filter(eleccion_tipo == "PASO") %>%
  filter(anio == "2015")

GEN2015 <- datos_presidencial %>%
  filter(eleccion_tipo == "GENERALES") %>%
  filter(anio == "2015")

BALL2015 <- datos_presidencial %>%
  filter(eleccion_tipo == "BALLOTAGE") %>%
  filter(anio == "2015")
}
################################
{
  print("ELECCIONES 2019")
PASO2019 <- datos_presidencial %>%
  filter(eleccion_tipo == "PASO") %>%
  filter(anio == "2019")

GEN2019 <- datos_presidencial %>%
  filter(eleccion_tipo == "GENERALES") %>%
  filter(anio == "2019")

#BALL2019 <- datos_presidencial %>%
#  filter(eleccion_tipo == "BALLOTAGE") %>%
#  filter(anio == "2019")
}
################################
{
  print("ELECCIONES 2023")
PASO2023 <- datos_presidencial %>%
  filter(eleccion_tipo == "PASO") %>%
  filter(anio == "2023")

GEN2023 <- datos_presidencial %>%
  filter(eleccion_tipo == "GENERALES") %>%
  filter(anio == "2023")

BALL2023 <- datos_presidencial %>%
  filter(eleccion_tipo == "BALLOTAGE") %>%
  filter(anio == "2023")
}
################################