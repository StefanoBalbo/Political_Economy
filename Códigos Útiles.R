st_write(datos, "datos.gpkg", delete_dsn = T, delete_layer = T)
save(datos, file = "datos.Rda")
write.xlsx(datos, "datos.xlsx")
write.csv(datos, "datos.csv")

# Para cambiar varias variables de class
library(dplyr)
library(magrittr)
datos2 <- st_drop_geometry(datos2)
datos2 [,c("valor" , "superficie" , "frente") ] %<>% mutate_if(is.factor,as.character)

# Geoservicios IDECOR
st_layers("WFS:http://idecor-ws.mapascordoba.gob.ar/geoserver/idecor/wms?request=GetCapabilities")
parcelas <- st_read("WFS:http://idecor-ws.mapascordoba.gob.ar/geoserver/idecor/wms?request=GetCapabilities","idecor:parcelas_cba")
names(parcelas)
parcelas = parcelas[,c("Superficie_Mejoras","Superficie_Tierra_Urbana","vut_vigente","Cantidad_Cuentas")]
parcelas = st_cast(parcelas, "GEOMETRYCOLLECTION") %>% st_collection_extract("POLYGON")
summary(parcelas)

nbi <- st_read ("WFS:http://geoservicios.indec.gov.ar/geoserver/ows?service=wfs&version=1.3.0&request=GetCapabilities","geocenso2010:nbi_radio") # IDERA --> Censo 2010 --> nbi
names(nbi)

nbi = st_cast(nbi, "GEOMETRYCOLLECTION") %>% 
  st_collection_extract("POLYGON")
nbi <- st_drop_geometry(nbi)
names(nbi)

#Para cambiar nombre de una variable
names(datos)[names(datos)=="nombre_viejo"] <- "nuevo_nombre"

#Para evitar poner tanto datps$vble
attach(datos)
table(p_sitjurid, p_forma)
summary (m2_coef)
detach(datos)

#Para ver si hay NA
contains_any_na = sapply(datos, function(x) any(is.na(x)))
names(datos)[contains_any_na]

#Guardar entrenamiento
saveRDS(train,"train_rf_VGB_443obs.rds")

#Reemplazar NA con medias
repalceNAsWithMean <- function(x) {replace(x, is.na(x), mean(x[!is.na(x)]))}
hyper <- repalceNAsWithMean(hyper)

#Reemplazar NA por el m?ximo sin incluir los valores NA
datos3 <- datos2 %>%
  mutate_if(is.numeric,  ~if_else(is.na(.x), max(.x,na.rm = TRUE), .x))

#Cambiar formato
#meterle "" , ""
cat(paste(shQuote(names(datos), type="cmd"), collapse=" , "))
#meterle __ + ___
cat(paste(shQuote(names(datos), type="cmd2"), collapse=" + "))

#Eliminar factores con 0
pred[] <- lapply(pred, function(x) if(is.factor(x)) factor(x) else x)

#Pasar a diccionario latino as? reconoce tildes y ?
datos <- datos %>% mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)})

#Quedarse con columnas
datos =datos[ , c("id" , "p_origen" , "p_id_orige" , "valor" , "p_tipodeva" , "p_sitjurid" ,
                  "p_nc" , "p_sup" , "p_forma" , "aglomerado" , "localidad" , "x" , "y" , "d_ruta" ,
                  "d_viasprin" , "d_viassec" , "d_alta" , "d_baja" ,
                  "d_depre" , "d_rio" , "prom_edif", "perc_edif" , "perc_baldm" , "perc_bald" , "d_ruta5" ,
                  "d_bplan" , "d_bpriv" , "d_balnea" , "d_centro" , "denscalle" , "d_puente" , "d_equip" ,
                  "d_bpopu" , "d_supcom" , "d_arroyo" , "spot_pavim" , "spot_veget" , "xy" , "p_muestra" ,
                  "carto_tipo" , "coc" , "p_valor_omi_180501" , "geom")]

#Case when

case_when (
  (omi$p_fechavalor < "2015-12-31"  ~ 2015),
  (omi$p_fechavalor >= "2016-01-01" & omi1$p_fechavalor <= "2016-12-31"  ~ 2016),
  (omi$p_fechavalor >= "2017-01-01" & omi1$p_fechavalor <= "2017-12-31"  ~ 2017),
  (omi$p_fechavalor >= "2018-01-01" & omi1$p_fechavalor <= "2018-12-31"  ~ 2018),
  (omi$p_fechavalor >= "2019-01-01" & omi1$p_fechavalor <= "2019-12-31"  ~ 2019),
  TRUE~ 2019)

datos$quant <- case_when( datos$vut_2022 <= quant[2,] ~ "Q1",
                          datos$vut_2022 > quant[2,] & datos$vut_2022 <= quant[3,] ~ "Q2",
                          datos$vut_2022 > quant[3,] & datos$vut_2022 <= quant[4,] ~ "Q3",
                          datos$vut_2022 > quant[4,]~ "Q4")

datos$m_ubicacion_cuadra <- case_when(  as.character(datos$p_ubicuadra)== "1" ~ "0",
                                        as.character(datos$p_ubicuadra) == "2" ~ "1",
                                        as.character(datos$p_ubicuadra) == "3" ~ "2",
                                        as.character(datos$p_ubicuadra) == "4" ~ "3",
                                        as.character(datos$p_ubicuadra) == "5" ~ "4")

# Para pasar de coordenadas a objeto espacial

st_as_sf(parcelas, coords = c("x", "y"), crs = 22174)

#Para obtener coordenadas desde geom a un x y
library(tidyverse)
coords <- do.call(rbind, st_geometry(datos)) %>%
  as_tibble() %>% setNames(c("x","y"))
datos$x <- coords$x
datos$y <- coords$y

# Setear coordenadas

base = st_set_crs(base, 22174)


#cambiar varios nombres juntos

library(data.table)
setnames(datos, old = c('join_fechavalor', 'join_algo'), new = c('fecha', 'algo'))

# lag en el tiempo

mylag <- function(x, k) c(rep(NA, k), x[1:(length(x)-k)])

# Group by   x %>% f(y) converted into f(x, y)

ingresos <- i10 %>%
  group_by(ch04) %>%
  dplyr::summarise(
    itf = mean(itf),
    ipcf = mean(ipcf),
    ij = mean(p47t),
    n=n())

quant_vut <- st_drop_geometry(datos) %>% group_by(quant) %>% summarise(media = mean(valor_m2),
                                                                       sd = sd(valor_m2))

# Crear columnas en un loop

for(i in 1:6){
  somedata[[paste("name",i ,sep="")]] <- i}

# Join espacial al k vecino mas proximo

library(nngeo)
prueba2 <- st_join (datos, parcelas[,c("quant")], st_nn , k=1 )

# Buscar v en R

library(expss)
vlookup("ubicacion_cuadra1", b_sig, 2)


# Hacer grilla
grilla = st_make_grid(ver, cellsize = 150)
grilla = st_as_sf(grilla)

# Compactar en grilla
grilla2 = aggregate(parcelas, grilla, mean)

# interseccion
datos = st_intersection(datos, peri2019[,c("peri:2019")])

# limpiar memoria
gc()

# Extraer data de osm (plazas)
library(osmdata)
plazas <- opq(bbox) %>% 
  add_osm_feature(key = "leisure", value = "park") %>% 
  osmdata_sf()
plazas

plaza = plazas$osm_multipolygons

ggplot() +
  geom_sf(data = plaza)



