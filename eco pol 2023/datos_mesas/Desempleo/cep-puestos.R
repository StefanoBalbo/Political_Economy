lapply(c("tidyverse", "rvest", "janitor", "scales", "lubridate",
         "shiny","rsconnect", "ggiraph", "data.table", "ggthemes", "gghighlight"), require, character.only=TRUE)

library(DatosAbiertosCEP)

pue <- descarga_DA(index_base=19)
pue <- dicc_depto_prov(pue)
pue <- diccionario_sectores(pue,agregacion="letra",descripciones=T)
pue <- as.data.table(pue)

pue <- na.omit(pue,cols=c("nombre_provincia_indec","nombre_departamento_indec"))

pue.pro.letra <- pue[ ,.(puestos=sum(puestos)),.(fecha,nombre_provincia_indec,letra_desc)]
pue.dep <- pue[ ,.(puestos=sum(puestos)),.(fecha,nombre_provincia_indec,nombre_departamento_indec)]
pue.act <- pue[ ,.(puestos=sum(puestos)),.(fecha,nombre_provincia_indec,nombre_departamento_indec,clae2)]

ggplot(test1,aes(fecha,V1))+geom_point()+geom_smooth(method="loess",span=.75,se=TRUE,color="red")+ facet_wrap(~nombre_provincia_indec,scales="free")+ylab("Cantidad de puestos de trabajo asalariados registrados")

ggplot(pue.pro.letra[pue.pro.letra$nombre_provincia_indec=="CABA",],aes(fecha,puestos))+geom_line()+facet_wrap(~letra_desc,scales="free",labeller = label_wrap_gen(multi_line = TRUE))+theme(
    axis.text.x = element_text(size = 10, vjust = 0.65),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 15, color = 'steelblue'),
    strip.text = element_text(size=6, color = 'steelblue'),
    strip.background = element_blank())



ggplot(pue.pro.letra[pue.pro.letra$nombre_provincia_indec=="Buenos Aires",],aes(fecha,puestos))+geom_line()+facet_wrap(~letra_desc,scales="free",labeller = label_wrap_gen(multi_line = TRUE))+theme(
    axis.text.x = element_text(size = 10, vjust = 0.65),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 15, color = 'steelblue'),
    strip.text = element_text(size=6, color = 'steelblue'),
    strip.background = element_blank())
