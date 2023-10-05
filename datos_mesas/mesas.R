directorio <- "/Users/stefa/Documents/Code/Political_Economy/datos_mesas/"
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


