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
library(stringr)
gc()

############################################################################

directorio <- "C:\\Users\\IDECOR\\Documents\\Code\\Political_Economy\\eco pol 2023\\datos_mesas"
#directorio <- "C:\\Users\\stefa\\Documents\\Code\\Political_Economy\\eco pol 2023\\datos_mesas"
setwd(directorio); getwd()

################################## SCRAP ###################################

url <- "https://socialstats.la/archivos/argentina/elecciones/elecciones.csv.7z"
file <- "C:\\Users\\IDECOR\\Documents\\Code\\Political_Economy\\eco pol 2023\\datos_mesas\\elecciones.csv.7z"
download.file(url, file)

winrar_path <- "C:\\Program Files\\WinRAR\\WinRAR.exe"
command <- paste(shQuote(winrar_path), "x", shQuote(file), shQuote(directorio))
system(command, wait = TRUE)
list.files(directorio)

rm(list=ls())

############################################################################
############################### DATOS MESAS ################################
{
  print("Importamos los datos")
  datos <- import("votacion.csv", setclass =" data.table", encoding="UTF-8")
  provincias <- import("distrito.csv",setclass="data.table",encoding="UTF-8")
  cargos <- import("cargo.csv",setclass="data.table",encoding="UTF-8")
  tdv <- import("tipovoto.csv",setclass="data.table",encoding="UTF-8")
  elecciontipo <- import("eleccion.csv",setclass="data.table",encoding="UTF-8")
  agrupaciones <- import("agrupacion.csv",setclass="data.table",encoding="UTF-8")
}


############################ PREPARACIÓN BASE ###############################

# # # Nombres distritos # # # 
names(datos)
names(provincias)

gc() # Not to overload
datos_provincias <- left_join(datos, provincias, by = "distrito_id")
names(datos_provincias)
head(datos_provincias)

datos_provincias <- datos_provincias[, -c(12, 13)]
datos_provincias <- rename(datos_provincias, distrito_nombre = nombre)
head(datos_provincias); rm(datos); rm(provincias)

# # # Nombres cargos # # # 
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

# # # Elección tipo y año # # # 
head(elecciontipo)
elecciontipo <- elecciontipo[, -c(2, 7)]; names(elecciontipo)

gc() # Not to overload
datos <- left_join(datos_provincias, elecciontipo, by = "eleccion_id")
head(datos)
table(datos$eleccion_tipo)
names(datos); rm(datos_provincias, elecciontipo)

# # # Tipo de voto # # # 
names(datos)
head(tdv)

gc() # Not to overload
datos2 <- left_join(datos, tdv, by = "tipovoto_id")
head(datos2); tdv

datos2 <- rename(datos2, tdv = nombre); names(datos2)
datos2 <- datos2[, -c(18)]

head(datos2)
datos <- datos2; rm(datos2, tdv); names(datos)

# # # Fix GENERAL =/= GENERALES  # # # 
head(datos)
table(datos$eleccion_tipo)

gc() # Not to overload
datos$eleccion_tipo <- ifelse(datos$eleccion_tipo == "GENERAL", 
                              "GENERALES", datos$eleccion_tipo); table(datos$eleccion_tipo)

table(datos$tdv)
table(datos$distrito_nombre)

# # # Agrupaciones  # # #
names(agrupaciones)
agrupaciones <- agrupaciones[, -4]
names(datos)

head(agrupaciones)
head(datos)
table(datos$cargo_nombre)


#############################################################################################
############################# ELECCIONES PRESIDENCIALES #####################################
#############################################################################################
{
  print("Filtramos elecciones presidenciales")
  datos_presidencial <- datos %>%
    filter(cargo_nombre == "Presidente")
}
rm(datos); head(datos_presidencial)

# # # Unificación nombres y recategorización # # #
{
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

gc() # Not to overload
datos_agrupacion <- left_join(datos_presidencial, agrupaciones, by = c("agrupacion_id", "eleccion_id"))
head(datos_agrupacion)
datos_agrupacion <- rename(datos_agrupacion, agrupacion_nombre = nombre )
names(datos_agrupacion); rm(agrupaciones)

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

################ ################ ################ ################ 
{
  print("ELECCIONES AÑO 2011")
PASO2011 <- datos_presidencial %>%
  filter(eleccion_tipo == "PASO") %>%
  filter(anio == "2011")

GEN2011 <- datos_presidencial %>%
  filter(eleccion_tipo == "GENERALES") %>%
  filter(anio == "2011")
}
################ ################ ################ ################ 
{
  print("ELECCIONES AÑO 2015")
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
################ ################ ################ ################ 
{
  print("ELECCIONES AÑO 2019")
PASO2019 <- datos_presidencial %>%
  filter(eleccion_tipo == "PASO") %>%
  filter(anio == "2019")

GEN2019 <- datos_presidencial %>%
  filter(eleccion_tipo == "GENERALES") %>%
  filter(anio == "2019")
}
################ ################ ################ ################ 
{
  print("ELECCIONES AÑO 2023")
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
################ ################ ################ ################ 

rm(datos_presidencial)

class(GEN2023); class(PASO2023); class(BALL2023); class(GEN2011); class(PASO2011); class(GEN2015); class(PASO2015); class(BALL2015); class(GEN2019); class(PASO2019)

setDT(GEN2023); setDT(PASO2023); setDT(BALL2023); setDT(GEN2011); setDT(PASO2011); setDT(GEN2015); setDT(PASO2015); setDT(BALL2015); setDT(GEN2019); setDT(PASO2019)

class(GEN2023); class(PASO2023); class(BALL2023); class(GEN2011); class(PASO2011); class(GEN2015); class(PASO2015); class(BALL2015); class(GEN2019); class(PASO2019)

# # # Guardamos presidenciales filtradas # # # 

fwrite(GEN2011, "GEN2011.csv")
fwrite(PASO2011, "PASO2011.csv")

fwrite(GEN2015, "GEN2015.csv")
fwrite(PASO2015, "PASO2015.csv")
fwrite(BALL2015, "BALL2015.csv")

fwrite(GEN2019, "GEN2019.csv")
fwrite(PASO2019, "PASO2019.csv")

fwrite(GEN2023, "GEN2023.csv")
fwrite(PASO2023, "PASO2023.csv")
fwrite(BALL2023, "BALL2023.csv")


###########################################################################################################################
######################################### MAPAS ##########################################################################

################ ################ MAPA PROVINCIAL 2023 ################ ################ 

# # # Geometrías provinciales # # # 
provs <- st_read("~/Code/Political_Economy/eco pol 2023/datos_mesas/IGN/provincia.shp"); names(provs)
provincias <- rio::import("distrito.csv", setclass = "data.table", encoding = "UTF-8"); names(provs)
table(provs$nam)
table(provincias$nombre)
class(provs$nam)
class(provincias$nombre)

provs$nam <- case_when(as.character(provs$nam)== "Tierra del Fuego, Antártida e Islas del Atlántico Sur" ~ "Tierra del Fuego",
                       as.character(provs$nam) == "Ciudad Autónoma de Buenos Aires" ~ "Ciudad Autónoma Bs.As.",
                       TRUE ~ provs$nam)

provs <- rename(provs, nombre = nam); names(provs)
names(provincias)

gc() # Not to overload
capa_provincial <- left_join(provs, provincias, by = "nombre")
names(capa_provincial); rm(provincias, provs)
head(capa_provincial)
table(capa_provincial$nombre)
capa_provincial <- rename(capa_provincial, distrito_nombre = nombre); names(capa_provincial)
capa_provincial <- capa_provincial[, -c(1, 2, 3, 4, 6, 7 ,8 , 9, 10, 11)]; head(capa_provincial)
class(capa_provincial)

# Guardamos geometrías provinciales
st_write(capa_provincial, "provincias.gpkg")
rm(list=ls()) ######## ######## ######## ######## ######## ######## ######## 

# Mapeamos
{
 capa_provincial <- st_read("~/Code/Political_Economy/eco pol 2023/datos_mesas/provincias.gpkg")
  GEN2023 <- import("GEN2023.csv", setclass =" data.table", encoding="UTF-8")
  PASO2023 <- import("PASO2023.csv", setclass =" data.table", encoding="UTF-8")
  #BALL2023 <- import("BALL2023.csv", setclass =" data.table", encoding="UTF-8")
}

class(GEN2023)
GEN2023 = data.frame(GEN2023)
PASO2023 = data.frame(PASO2023)
# BALL2023 = data.frame(BALL2023)

votos = GEN2023 %>% 
  group_by(distrito_nombre, agrupacion_nombre) %>% 
  summarise(votos = sum(votos))
votos

totales = GEN2023 %>% 
  group_by(distrito_nombre) %>% 
  summarise(totales = sum(votos))
totales


votos = left_join(votos, totales)
votos; rm(totales)
votos$prop = votos$votos / votos$totales
votos

votos$prop <- (votos$prop)*100
votos
class(votos$prop)

gc() # Not to overload
mapa_votos = left_join(votos, capa_provincial, by = "distrito_nombre")
mapa_votos

class(mapa_votos)
mapa_votos = st_as_sf(mapa_votos, sf_column_name = "geom")
# mapa = mapview::mapview(mapa_votos, zcol = "votos")

mapa_votos$agrupacion_nombre = gsub("/", " ", mapa_votos$agrupacion_nombre)

mapview_facet <- function(x,f, z) {
  
  criteria=split(x,x[[f]])
  nms = paste(deparse(substitute(x)), names(criteria), sep = "-")
  for (i in 1:length(criteria)) {
    map=mapview::mapview(criteria[[i]], layer.name = nms[i], z = z)
    assign(paste0("map_",i), map)
  }
  set=list(map_1)
  for (i in 2:length(criteria)) {set=append(set, get(paste0("map_",i)))}
  leafsync::latticeView(set)
}

mapview_facet(x = mapa_votos, f = "agrupacion_nombre", z = "prop")

library(tmap)
library(tmaptools)
#tmap_mode("view")
tmap_mode("plot")
#tmap_mode(mode = c("plot", "view"))
tmap_options(check.and.fix = TRUE)
mapa_votos = st_transform(mapa_votos, 22174)

gc() # Not to overload
ver <-   tm_shape(mapa_votos) +
  tm_scale_bar(position = c("left", "bottom")) + tm_compass(position = c("right", "top"), size = 1) +
  tm_polygons(size = 0.12, style = "jenks", n = 8, col = "prop", alpha = 0.7, border.lwd = 0.3, 
              title = "Porcentaje de Votos") +
  tm_layout(legend.position = c("right", "center"),
            legend.outside = TRUE,
            title = 'Elecciones Generales 2023', 
            title.position = c('center', 'top')) +
  tm_facets(by = "agrupacion_nombre", ncol = 3, nrow = 2)
ver
tmap_save(ver, "mapa_generales_2023_provs.png")

rm(list=ls())


################ ################ MAPA DEPARTAMENTAL 2023 ################ ################
{
  # Base mesas
distrito <- import("distrito.csv", setclass = "data.table", encoding = "UTF-8")
seccion <- import("seccion.csv", setclass = "data.table", encoding = "UTF-8")
  # Seba
deptnames <- import("Departamentos/deptnames.csv", setclass = "data.table", encoding = "UTF-8")
deptGEO <- import("Departamentos/deptnamesGEO.csv", setclass = "data.table", encoding = "UTF-8")
}

seccion$nombre <- toupper(seccion$nombre)
seccion <- rename(seccion, DEPARTA = nombre); head(seccion)

distrito$nombre <- toupper(distrito$nombre)
distrito <- rename(distrito, PROVINCIA = nombre); head(distrito)

seccionxdistrito <- merge(seccion, distrito, by = "distrito_id")
seccionxdistrito <- seccionxdistrito[, -c(3)]; head(seccionxdistrito); rm(seccion, distrito)

colnames(deptnames) <- c("DEPARTAMENTO", "NOMBRE"); head(deptnames)
colnames(deptGEO) <- c("index", "NOMBRE"); head(deptGEO)

nrow(deptnames); nrow(deptGEO)

# Quitamos ruido en nombres
Encoding(deptnames$NOMBRE) <- "UTF-8"
Encoding(deptGEO$NOMBRE) <- "UTF-8"
head(deptGEO)
head(deptnames)

deptnames$NOMBRE <- iconv(deptnames$NOMBRE, "latin1", "UTF-8")
deptGEO$NOMBRE <- iconv(deptGEO$NOMBRE, "latin1", "UTF-8")
head(deptGEO)
head(deptnames)

deptnames$NOMBRE <- gsub("¾", "Ñ", deptnames$NOMBRE)
deptGEO$NOMBRE <- gsub("¾", "Ñ", deptGEO$NOMBRE)
head(deptGEO)
head(deptnames)

deptnames$NOMBRE <- str_replace(deptnames$NOMBRE, "^[0-9]+\\s", "")

deptGEO$NOMBRE <- str_replace_all(deptGEO$NOMBRE, '"', '')
deptnames$NOMBRE <- str_replace_all(deptnames$NOMBRE, '"', '')
head(deptGEO)
head(deptnames)

deptnames$NOMBRE <- toupper(deptnames$NOMBRE)
deptGEO$NOMBRE <- toupper(deptGEO$NOMBRE)
deptnames$NOMBRE <- trimws(deptnames$NOMBRE)
deptGEO$NOMBRE <- trimws(deptGEO$NOMBRE)
head(deptGEO)
head(deptnames)

deptnames$NOMBRE <- gsub("[^[:print:]]", "", deptnames$NOMBRE)
deptGEO$NOMBRE <- gsub("[^[:print:]]", "", deptGEO$NOMBRE)
head(deptGEO)
head(deptnames)

class(deptnames); class(deptGEO)
class(deptnames$NOMBRE); class(deptGEO$NOMBRE)

deptnames$index <- as.integer(str_extract(deptnames$DEPARTAMENTO, "\\d+"))
head(deptnames)
head(deptGEO)

identical(deptnames$NOMBRE, deptGEO$NOMBRE)
identical(deptnames$index, deptGEO$index)
class(deptnames$index); class(deptGEO$index)

gc() # Not to overload
dept <- left_join(deptnames, deptGEO, by = "NOMBRE"); head(dept)

fwrite(dept, "Departamentos/namesdept.csv")
rm(deptGEO); rm(deptnames)

# check nombres #
#mismatched_names <- setdiff(unique(deptnames$NOMBRE), unique(deptGEO$NOMBRE)); mismatched_names
#mismatched_deptnames <- deptnames[deptnames$NOMBRE %in% mismatched_names,]
#mismatched_deptGEO <- deptGEO[deptGEO$NOMBRE %in% mismatched_names,]
#nrow(mismatched_deptGEO); nrow(#mismatched_deptnames) # Si nrow() = 0 son idénticos
#rm(mismatched_deptGEO, mismatched_deptnames)
# # # # # # # # 

# check missing #
#na <- is.na(dept); sum(na) 
#rm(na)
#library(fuzzyjoin)
#library(stringdist)
#fuzzy_result <- stringdist_left_join(deptnames, deptGEO, by = c("NOMBRE" = "NOMBRE"), method = "jw", max_dist = 0.1)
#head(fuzzy_result)
#na <- is.na(fuzzy_result$index); sum(na) 
#na <- is.na(fuzzy_result$NOMBRE.y); sum(na) 
#rm(na, fuzzy_result)
# # # # # # # # 

# # # Geometrías departamentales # # #
dptos <- st_read("~/Code/Political_Economy/eco pol 2023/datos_mesas/Departamentos/departamentos.shp"); names(dptos)
dptos <- dptos[dptos$OBJECTID!="488"&dptos$OBJECTID!="489",] # Elimina Islas Malvinas y Antartida
gc() # Not to overload
plot(dptos)

head(dptos)
head(dept)
head(seccionxdistrito)

identical(dept$NOMBRE, seccionxdistrito$DEPARTA)
identical(dept$NOMBRE, dptos$DEPARTA)
identical(dptos$DEPARTA, seccionxdistrito$DEPARTA)

seccionxdistrito <- rename(seccionxdistrito, seccion_nombre = DEPARTA)
dept <- rename(dept, seccion_nombre = NOMBRE)
dptos <- rename(dptos, seccion_nombre = DEPARTA)

gc() # Not to overload
merge <- left_join(seccionxdistrito, dept, by = "seccion_nombre")
departamentos <- left_join(dptos, merge, by = "seccion_nombre")

head(departamentos)
departamentos <- departamentos[, -c(1, 5, 6, 7, 9, 10, 11, 12)]
departamentos <- rename(departamentos, distrito_nombre = PROVINCIA.x); head(departamentos)

# Guardamos geometrías departamentales
st_write(departamentos, "departamentos.gpkg", append = FALSE)
rm(list=ls()) ######## ######## ######## ######## ######## ######## ######## 

# Mapeamos
{
  capa_departamental <- st_read("~/Code/Political_Economy/eco pol 2023/datos_mesas/departamentos.gpkg")
  GEN2023 <- import("GEN2023.csv", setclass =" data.table", encoding="UTF-8")
  PASO2023 <- import("PASO2023.csv", setclass =" data.table", encoding="UTF-8")
  #BALL2023 <- import("BALL2023.csv", setclass =" data.table", encoding="UTF-8")
}

class(GEN2023)
GEN2023 = data.frame(GEN2023)
PASO2023 = data.frame(PASO2023)
# BALL2023 = data.frame(BALL2023)

head(capa_departamental)

dptos = capa_departamental %>% 
  group_by(seccion_nombre, geom) %>% 
  summarise(distrito = distrito_nombre,
            distrito_id = distrito_id,
            seccion_id = seccion_id)
dptos; rm(capa_departamental)

votos = GEN2023 %>% 
  group_by(seccion_id, agrupacion_nombre) %>% 
  summarise(votos = sum(votos))
votos

totales = GEN2023 %>% 
  group_by(seccion_id) %>% 
  summarise(totales = sum(votos))
totales


votos = left_join(votos, totales)
votos; rm(totales)
votos$prop = votos$votos / votos$totales
votos

votos$prop <- (votos$prop)*100
votos
class(votos$prop)

gc() # Not to overload
mapa_votos = left_join(votos, dptos, by = "seccion_id", multiple = "all")
mapa_votos
class(mapa_votos)

library(tmap)
library(tmaptools)
#tmap_mode("view")
tmap_mode("plot")
#tmap_mode(mode = c("plot", "view"))
tmap_options(check.and.fix = TRUE)
mapa_votos = st_as_sf(mapa_votos)
mapa_votos = st_transform(mapa_votos, 22174)

ver <-   tm_shape(mapa_votos) +
  tm_scale_bar(position = c("left", "bottom")) + tm_compass(position = c("right", "top"), size = 1) +
  tm_polygons(size = 0.12, style = "jenks", n = 8, col = "prop", alpha = 0.7, border.lwd = 0.3, 
              title = "Porcentaje de Votos") +
  tm_layout(legend.position = c("right", "center"),
            legend.outside = TRUE,
            title = 'Elecciones Generales 2023', 
            title.position = c('center', 'top')) +
  tm_facets(by = "agrupacion_nombre", ncol = 3, nrow = 2)
ver
tmap_save(ver, "mapa_generales_2023_dptos.png")

rm(list=ls())

######## ######## ######## ######## ######## ######## ######## 
######## ######## ######## ######## ######## ######## ######## 
######## ######## ######## ######## ######## ######## ######## 

##################### INFERENCIA ECOLÓGICA #####################
# Ver Gary King

library(ecoreg)
library(eco) # ver: there is no package called ‘eco’
library(ei)


