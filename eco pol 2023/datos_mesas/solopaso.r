rm(list=ls())

setwd("/Users/stefa/Documents/Code/Political_Economy/eco pol 2023/datos_mesas"); getwd()
lapply(c("tidyverse", "rvest", "janitor", "data.table", "rio", "here", "dplyr"), require, character.only = TRUE)

#carga de datos
mesas <- import("votacion.csv", setclass =" data.table", encoding="UTF-8")
head(mesas)

#mesas <- mesas[,total:=sum(votos[tipovoto_id=="1"]),by=c("distrito_id","seccion_id","circuito_id","mesa_id","eleccion_id")]
#mesas<-mesas[,id:= .GRP, by=.(distrito_id,seccion_id,circuito_id,mesa_id)]

provincia<-import("distrito.csv",setclass="data.table",encoding="UTF-8")
cargo<-import("cargo.csv",setclass="data.table",encoding="UTF-8")
tipo_voto<-import("tipovoto.csv",setclass="data.table",encoding="UTF-8")
eleccion<-import("eleccion.csv",setclass="data.table",encoding="UTF-8")
agrupacion<-import("agrupacion.csv",setclass="data.table",encoding="UTF-8")
departamentos<-import("seccion.csv",setclass="data.table",encoding="UTF-8")

table(mesas$cargo_id)

#cargo/s a investigar
presidenciales <- mesas[cargo_id == '1']
table(presidenciales$cargo_id)

#eleccion/es a investigar
presidenciales_paso<-presidenciales[eleccion_id=='1'|eleccion_id=='5'|eleccion_id=='10'|eleccion_id=='14']
rm(presidenciales);rm(mesas)
table(presidenciales_paso$agrupacion_id)

#recategorizar partidos
datos_agrupacion<-left_join(presidenciales_paso,agrupacion,by=c("agrupacion_id","eleccion_id"))
rm(presidenciales_paso)
head(datos_agrupacion)
datos_agrupacion$agrupacion_id<-datos_agrupacion$nombre
head(datos_agrupacion)
datos_agrupacion$nombre<-NULL
datos_agrupacion$agrupacioninterna<-NULL
table(datos_agrupacion$agrupacion_id)
tail(datos_agrupacion)

#unificamos los nombres en uno solo (en caso que sea necesario)
agrupacion$nombre<-gsub("ALIANZA COMPROMISO FEDERAL","Alianza Compromiso Federal",agrupacion$nombre,fixed=TRUE)
agrupacion$nombre<-gsub("ALIANZA FRENTE DE IZQUIERDA Y DE LOS TRABAJADORES","Alianza Frente de Izquierda y de los Trabajadores",agrupacion$nombre,fixed=TRUE)
agrupacion$nombre<-gsub("ALIANZA FRENTE AMPLIO PROGRESISTA","Alianza Frente Amplio Progresista",agrupacion$nombre,fixed=TRUE)
agrupacion$nombre<-gsub("ALIANZA FRENTE PARA LA VICTORIA","Alianza Frente para la Victoria",agrupacion$nombre,fixed=TRUE)
agrupacion$nombre<-gsub("ALIANZA FRENTE POPULAR","Alianza Frente Popular",agrupacion$nombre,fixed=TRUE)
agrupacion$nombre<-gsub("ALIANZA UNION PARA EL DESARROLLO SOCIAL - UDESO","Alianza Unión para el Desarrollo Social",agrupacion$nombre,fixed=TRUE)
agrupacion$nombre<-gsub("COALICION CIVICA - AFIRMACION PARA UNA REPUBLICA IGUALITARIA (ARI)","COALICION CIVICA ARI",agrupacion$nombre,fixed=TRUE)
agrupacion$nombre<-gsub("COALICIÓN CÍVICA - AFIRMACIÓN PARA UNA REPÚBLICA IGUALITARIA (ARI)","COALICION CIVICA ARI",agrupacion$nombre,fixed=TRUE)
agrupacion$nombre<-gsub("Coalición Cívica - Afirmación para una República Igualitaria ARI","COALICION CIVICA ARI",agrupacion$nombre,fixed=TRUE)
agrupacion$nombre<-gsub("COALICION CIVICA - ARI","COALICION CIVICA ARI",agrupacion$nombre,fixed=TRUE)
agrupacion$nombre<-gsub("FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD","Alianza Frente de Izquierda y de los Trabajadores",agrupacion$nombre,fixed=TRUE)
agrupacion$nombre<-gsub("FRENTE DE IZQUIERDA Y DE TRABAJADORES UNIDAD","Alianza Frente de Izquierda y de los Trabajadores",agrupacion$nombre,fixed=TRUE)
agrupacion$nombre<-gsub("FRENTE PARA LA VICTORIA","Alianza Frente para la Victoria",agrupacion$nombre,fixed=TRUE)
agrupacion$nombre<-gsub("MOVIMIENTO IZQUIERDA JUVENTUD DIGNIDAD","MOVIMIENTO IZQUIERDA JUVENTUD Y DIGNIDAD",agrupacion$nombre,fixed=TRUE)
agrupacion$nombre<-gsub("MOVIMIENTO LIBRES DEL SUR","Movimiento Libres del Sur",agrupacion$nombre,fixed=TRUE)
agrupacion$nombre<-gsub("MOVIMIENTO IZQUIERDA JUVENTUD DIGNIDAD","MOVIMIENTO IZQUIERDA JUVENTUD Y DIGNIDAD",agrupacion$nombre,fixed=TRUE)
agrupacion$nombre<-gsub("MOVIMIENTO IZQUIERDA JUVENTUD DIGNIDAD","MOVIMIENTO IZQUIERDA JUVENTUD Y DIGNIDAD",agrupacion$nombre,fixed=TRUE)

#recategorizamos en una gran coalición
datos_agrupacion$agrupacion_id[datos_agrupacion$agrupacion_id %in% c("JUNTOS POR EL CAMBIO", "ALIANZA CAMBIEMOS", "CAMBIEMOS","MOVIMIENTO DE ACCION VECINAL")] <- "Cambiemos/Macrismo"
datos_agrupacion$agrupacion_id[datos_agrupacion$agrupacion_id %in% c("Alianza Frente para la Victoria", "ALIANZA FRENTE PARA LA VICTORIA", "FRENTE DE TODOS","FRENTE PARA LA VICTORIA","UNION POR LA PATRIA")] <- "Kirchnerismo"
datos_agrupacion$agrupacion_id[datos_agrupacion$agrupacion_id %in% c("FRENTE LIBER.AR", "LA LIBERTAD AVANZA", "UNION DEL CENTRO DEMOCRATICO","UNITE POR LA LIBERTAD Y LA DIGNIDAD")] <- "Liberalismo"
datos_agrupacion$agrupacion_id[datos_agrupacion$agrupacion_id %in% c("Alianza Compromiso Federal", "ALIANZA COMPROMISO FEDERAL", "Alianza Frente Popular","ALIANZA FRENTE POPULAR","ALIANZA UNIDOS POR UNA NUEVA ALTERNATIVA (UNA)","Frente Entrerriano Federal","HACEMOS POR NUESTRO PAIS","PRINCIPIOS Y VALORES","CONSENSO FEDERAL","PARTIDO MOVIMIENTO DE ACCION  VECINAL")] <- "Peronismo Federal/Tercera Via"
datos_agrupacion$agrupacion_id[datos_agrupacion$agrupacion_id %in% c("Alianza Frente Amplio Progresista", "ALIANZA FRENTE AMPLIO PROGRESISTA", "ALIANZA PROGRESISTAS","ALIANZA PROYECTO SUR","Alianza Unión Para el Desarrollo Social","ALIANZA UNION PARA EL DESARROLLO SOCIAL - UDESO","COALICION CIVICA - AFIRMACION PARA UNA REPUBLICA IGUALITARIA (ARI)","Coalición Cívica - Afirmación para una República Igualitaria ARI","COALICION CIVICA - ARI","COALICION CIVICA ARI","Movimiento Libres del Sur","MOVIMIENTO LIBRES DEL SUR","PARTIDO SOCIAL DE CENTRO","Alianza Unión para el Desarrollo Social","COALICIÓN CÍVICA - AFIRMACIÓN PARA UNA REPÚBLICA IGUALITARIA (ARI)")] <- "Progresismo/Socialdemocracia"
datos_agrupacion$agrupacion_id[datos_agrupacion$agrupacion_id %in% c("Alianza Frente de Izquierda y de los Trabajadores", "ALIANZA FRENTE DE IZQUIERDA Y DE LOS TRABAJADORES", "FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD","FRENTE DE IZQUIERDA Y DE TRABAJADORES UNIDAD","MOVIMIENTO AL SOCIALISMO","MST - NUEVA IZQUIERDA","POLITICA OBRERA","PROYECTO JOVEN")] <- "Izquierda"
datos_agrupacion$agrupacion_id[datos_agrupacion$agrupacion_id %in% c("DEL CAMPO POPULAR", "FRENTE NOS", "FRENTE PATRIOTA","FRENTE PATRIOTA FEDERAL","MOVIMIENTO IZQUIERDA JUVENTUD DIGNIDAD","PARTIDO AUTONOMISTA","PARTIDO POPULAR","MOVIMIENTO IZQUIERDA JUVENTUD Y DIGNIDAD")] <- "Conservadurismo/Nacionalismo"
table(datos_agrupacion$agrupacion_id)
head(datos_agrupacion)
tail(datos_agrupacion)

#recategorizar provincias
datos_provincias<-left_join(datos_agrupacion, provincia, by = "distrito_id")
rm(datos_agrupacion)
datos_provincias$distrito_id<-datos_provincias$nombre
datos_provincias$nombre<-NULL
datos_provincias$provincia_id<-NULL
datos_provincias$eleccionorigen_id<-NULL

#recategorizar cargo a elegir
datos_cargo<-left_join(datos_provincias, cargo, by = "cargo_id")
rm(datos_provincias)
datos_cargo$cargo_id<-datos_cargo$nombre
datos_cargo$nombre<-NULL
datos_cargo$eleccionorigen_id<-NULL

#recategorizar tipo de voto
datos_tipovoto<-left_join(datos_cargo, tipo_voto, by = "tipovoto_id")
rm(datos_cargo)
datos_tipovoto$tipovoto_id<-datos_tipovoto$nombre
datos_tipovoto$nombre<-NULL
datos_tipovoto$nombre2<-NULL
muestra<-datos_agrupacion %>% sample_n(30)
rm(muestra)

#recategorizar eleccion
datos_eleccion<-left_join(datos_tipovoto, eleccion, by = "eleccion_id")
rm(datos_tipovoto)
datos_eleccion$eleccion_id<-datos_eleccion$descripcion
datos_eleccion$anio<-NULL
datos_eleccion$recuento_tipo<-NULL
datos_eleccion$padron_tipo<-NULL
datos_eleccion$eleccion_tipo<-NULL
datos_eleccion$path_origen<-NULL
datos_eleccion$descripcion<-NULL
head(datos_eleccion)
#datos_tipovoto$election_id<-ifelse(datos_tipovoto$election_id==1,"PASO 2011",
                                  # ifelse(datos_tipovoto$election_id==5,"PASO 2015",
                                         # ifelse(datos_tipovoto$election_id==10,"PASO 2019",
                                               #  ifelse(datos_tipovoto$election_id==14,"PASO 2023","Otro"))))

tail(datos_eleccion)

write.csv(datos_tipovoto,'paso.csv')
