############################### ECO POL 2023 ##################################
rm(list=ls())

library(installr)
library(R.utils)
library(dplyr)
library(data.table)
library(tidyverse)
library(rvest)
library(janitor)
library(rio)
library(here)
library(tmap)
library(sf)
library(glue)
library(scales)
library(htmltools)
library(leaflet)

############################### DATOS MESAS ###################################

directorio <- "C:\\Users\\IDECOR\\Documents\\Code\\Political_Economy\\eco pol 2023\\datos_mesas"
#directorio <- "C:\\Users\\stefa\\Documents\\Code\\Political_Economy\\eco pol 2023\\datos_mesas"

setwd(directorio); getwd()

############################################################################

url <- "https://socialstats.la/archivos/argentina/elecciones/elecciones.csv.7z"
file <- "C:\\Users\\IDECOR\\Documents\\Code\\Political_Economy\\eco pol 2023\\datos_mesas\\elecciones.csv.7z"
download.file(url, file)

winrar_path <- "C:\\Program Files\\WinRAR\\WinRAR.exe"

command <- paste(shQuote(winrar_path), "x", shQuote(file), shQuote(directorio))

system(command, wait = TRUE)

list.files(directorio)

############################################################################

{
  print("Importamos los datos")
  datos <- import("votacion.csv", setclass =" data.table", encoding="UTF-8")
  provincias <- import("distrito.csv",setclass="data.table",encoding="UTF-8")
  cargos <- import("cargo.csv",setclass="data.table",encoding="UTF-8")
  tdv <- import("tipovoto.csv",setclass="data.table",encoding="UTF-8")
  elecciontipo <- import("eleccion.csv",setclass="data.table",encoding="UTF-8")
  agrupaciones <- import("agrupacion.csv",setclass="data.table",encoding="UTF-8")
#seccion<-import("seccion.csv",setclass="data.table",encoding="UTF-8")
#listas <- fread(paste0(directorio, "lista.csv"), sep = ",", dec = ".")
#mesas <-  fread(paste0(directorio, "mesa.csv"), sep = ",", dec = ".")
#circuito <- fread(paste0(directorio, "circuito.csv"), sep = ",", dec = ".")
#seccion_prov <-fread(paste0(directorio, "seccionprovincial.csv"), sep = ",", dec = ".")
}

head(datos)
head(agrupaciones)
cargos
tdv
elecciontipo
table(provincias$nombre); names(provincias)


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
table(datos$eleccion_tipo)
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

head(datos)
table(datos$eleccion_tipo)

datos$eleccion_tipo <- ifelse(datos$eleccion_tipo == "GENERAL", "GENERALES", datos$eleccion_tipo); table(datos$eleccion_tipo)

table(datos$tdv)
table(datos$distrito_nombre)

#getwd()
#saveRDS(datos, "datos.rda", compress = FALSE)
#saveRDS(agrupaciones, "agrupaciones.rda", compress = FALSE)
#rm(list=ls())


############################# ELECCIONES PRESIDENCIALES #############################

#load("datos.rda")
#load("agrupaciones.rda")

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

rm(datos)
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
table(datos_presidencial$agrupacion_nombre)

#saveRDS(datos_presidencial, "datos_presidenciales.rda", compress = FALSE)

#load("datos_presidenciales.rda")


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

rm(datos_presidencial)

######################################### MAPAS #########################################

##################### POR PROVINCIAS
provs <- st_read("~/Code/Political_Economy/eco pol 2023/datos_mesas/IGN/provincia.shp"); names(provs)
provincias <- import("distrito.csv", setclass = "data.table", encoding = "UTF-8"); names(provs)

table(provs$nam)
table(provincias$nombre)

provs$nam <- case_when(as.character(provs$nam)== "Tierra del Fuego, Antártida e Islas del Atlántico Sur" ~ "Tierra del Fuego",
                       as.character(provs$nam) == "Ciudad Autónoma de Buenos Aires" ~ "Ciudad Autónoma Bs.As.",
                       TRUE ~ provs$nam
)

provs <- rename(provs, nombre = nam); names(provs)
names(provincias)

capa_provincial <- left_join(provs, provincias, by = "nombre")
names(capa_provincial); rm(provincias, provs)

head(capa_provincial)

head(GEN2023)
head(PASO2023)

table(capa_provincial$nombre)

table(GEN2023$distrito_nombre)
table(PASO2023$distrito_nombre)

capa_provincial <- rename(capa_provincial, distrito_nombre = nombre); names(capa_provincial)
names(GEN2023)
names(PASO2023)

head(capa_provincial)
capa_provincial <- capa_provincial[, -c(1, 2, 3, 4, 6, 7 ,8 , 9, 10, 11)]; head(capa_provincial)

st_write(capa_provincial, "provincias.gpkg") # Guardamos geometrías provinciales

class(GEN2023); class(PASO2023)
setDT(GEN2023)
setDT(PASO2023)
class(GEN2023); class(PASO2023)

fwrite(GEN2023, "GEN2023.csv") # Guardamos elecciones presidenciales 2023
fwrite(PASO2023, "PASO2023.csv")

names(GEN2023)
names(PASO2023)
names(capa_provincial)

############# ############# ############# ############# ############# #############

# PENDIENTES DE REVISAR:

# Left_join geometrías y elecciones # Mapas

map_data_GEN <- capa_provincial[GEN2023, on = .(distrito_nombre), nomatch = 0]

map_data_PASO <- capa_provincial[PASO2023, on = .(distrito_nombre), nomatch = 0]

# The next line of code merges the data with the geography:
#map_data_GEN <- merge(capa_provincial, GEN2023, by = "distrito_nombre")
#map_data_PASO <- merge(capa_provincial, PASO2023, by = "distrito_nombre")

#tmap_mode('view')
#tmap_options(check.and.fix = TRUE)
#mapas <-  tm_basemap(c(Satelite = "Esri.WorldImagery", Politico = "OpenStreetMap.DE")) +
#tm_shape(capa_provincial) +
#  tm_fill("white", alpha = 0) +
#  tm_borders("black", lwd = 1.5) +
#tm_shape(voto) +
#tm_polygons("", style = "quantile", palette ="RdYlGn") +
#mapas

##################### POR DEPARTAMENTO
#dptos <- st_read("~/Code/Political_Economy/eco pol 2023/datos_mesas/IGN/departamento.shp"); names(dptos)
#seccion <- import("seccion.csv", setclass = "data.table", encoding = "UTF-8"); names(seccion)

#table(dptos$nam)
#table(seccion$nombre)

#dptos <- rename(dptos, nombre = nam); names(dptos)
#names(seccion)

#capa_departamental <- left_join(dptos, seccion, by = "nombre")
#names(capa_departamental)

#na <- is.na(capa_departamental)
#sum(na) # Genera NA por =/= nombres

##########################################

#tmap_mode('view')
#tmap_options(check.and.fix = TRUE)
#mapas <-  tm_basemap(c(Satelite = "Esri.WorldImagery", Politico = "OpenStreetMap.DE")) +
#  tm_shape(Dptos_Imp) +
#  tm_polygons("", style = "quantile", palette ="RdYlGn")+
#  tm_shape(dptos) +
#  tm_fill("white", alpha = 0)+
#  tm_borders("black", lwd = 1.5)
#mapas

