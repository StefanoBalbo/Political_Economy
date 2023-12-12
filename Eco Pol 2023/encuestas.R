rm(list=ls())

lapply(c("tidyverse", "rvest", "janitor", "scales", "lubridate",
         "shiny","rsconnect", "ggiraph", "grid", "data.table", "ggthemes", "gghighlight"), require, character.only=TRUE)

tablas <- read_html("https://es.wikipedia.org/wiki/Anexo:Encuestas_de_intenci%C3%B3n_de_voto_para_las_elecciones_presidenciales_de_Argentina_de_2023") %>%
    html_table(fill=TRUE)

### Function to extract and parse dates

extract_and_parse_date <- function(date_text) {
  if (str_detect(date_text, "-")) {
    date_text <- str_split(date_text, "-", simplify = TRUE)[, 2]
  } else if (!str_detect(date_text, "^\\d+ de [[:alpha:]]+ de \\d{4}$")) {
    date_text <- paste("14 de", date_text)
  }

  date_text <- str_trim(date_text)
  spanish_month <- str_extract(date_text, "(?<=de )[[:alpha:]]+")
  english_month <- spanish_to_english_month(spanish_month)
  date_text <- str_replace(date_text, spanish_month, english_month)
  parsed_date <- dmy(date_text)

  return(parsed_date)
}

### Function to convert spanish to english month names

spanish_to_english_month <- function(spanish_month) {
  month_map <- c("enero" = "January", "febrero" = "February", "marzo" = "March",
                 "abril" = "April", "mayo" = "May", "junio" = "June",
                 "julio" = "July", "agosto" = "August", "septiembre" = "September",
                 "octubre" = "October", "noviembre" = "November", "diciembre" = "December")
  return(month_map[tolower(spanish_month)])
}

### Color codes
cbp1 <- c("#0022ee","#6A041D", "#22bce3", "#db0000", "#000000", "#37853c", "#6605f7", "#f6fa05", "#ff0000", "#00f7ff", "#ff9900",
          "#c9fa05", "#686964", "#46e51a")

###############################################
### TABLAS DE CANDIDATOS PRE-CIERRE DE LISTAS
###############################################

### Extract and clean candidate tables and delete "event" rows - PRE-CIERRE DE LISTAS

sexta <- as.data.table(tablas[6])[-1,]
sexta <- setNames(sexta,c("fecha", "encuestadora", "muestra", "fernandez", "kirchner","scioli", "de pedro", "kicillof", "massa", "larreta", "macri", "bullrich", "vidal", "manes", "lousteau", "morales", "milei", "bregman", "solano", "delcano", "espert", "schiaretti", "otros", "blanco", "indecisos"))
sexta <- sexta[!grepl('anuncia|presidente|candidata|Kirchner',muestra),]
sexta[,fecha:=sapply(fecha, extract_and_parse_date) %>% as.Date(origin = "1970-01-01")]
sexta[,(c(4:25)):=lapply(.SD, function(x) ifelse(x == "-", NA, as.numeric(gsub(",", ".",x)))),.SDcols=c(4:25)]

names(sexta)

### Convert from wide to long format

sexta.l <- melt(sexta,id.vars=c(1:3),measure.vars=c(4:25))
sexta.l[,party:=ifelse(variable=="fernandez"|variable=="kirchner"|variable=="scioli"|variable=="de pedro"|variable=="kicillof"|variable=="massa","UxP",ifelse(variable=="larreta"|variable=="macri"|variable=="bullrich"|variable=="vidal"|variable=="manes"|variable=="lousteau"|variable=="morales","JxC",ifelse(variable=="milei","LLA",ifelse(variable=="bregman"|variable=="solano"|variable=="delcano","FIT",ifelse(variable=="espert","AL",ifelse(variable=="schiaretti","HxNP",ifelse(variable=="otros","otros",ifelse(variable=="blanco","blanco","indecisos"))))))))]
names(sexta.l)

### grafico 1 - por alianza, candidatato y previo a cierre
ggplot(subset(sexta.l,party!="AL"&party!="HxNP"&party!="FIT"&party!="otros"&party!="blanco"&party!="indecisos"&fecha>"2022-01-01"),aes(fecha,value,color=variable))+geom_point()+facet_wrap(~party)+geom_smooth(method = "loess", se =FALSE, show.legend = FALSE,aes(x=fecha,y=value,color=variable))+scale_color_manual(values=cbp1)+ gghighlight(use_direct_label=FALSE)+ylim(0,35)+guides(color = guide_legend(override.aes = list(size=4)))+ylab("Intencion de voto (%)")+ theme_economist()+theme(axis.text.x = element_text(size=10,angle = 0, hjust =0.5))
ggsave("encuestas-pre.png",height=4.5,width=7.5)

###############################################
### TABLAS DE CANDIDATOS POST-CIERRE DE LISTAS
###############################################

### Extract and clean candidate tables and delete "event" rows - POST-CIERRE DE LISTAS

cuarta <- as.data.table(tablas[4])[-1,]
cuarta <- setNames(cuarta,c("fecha","encuestadora","muestra","massa","grabois","bullrich","larreta","milei","bregman","solano","schiaretti","moreno","otros","blanco","indecisos"))
cuarta <- cuarta[!grepl('anuncia|presidente|candidata|Kirchner',muestra),]
cuarta[,fecha:=sapply(fecha, extract_and_parse_date) %>% as.Date(origin ="1970-01-01")]
cuarta[,(c(4:14)):=lapply(.SD, function(x) ifelse(x == "-", NA, as.numeric(gsub(",", ".",x)))),.SDcols=c(4:14)]

cuarta.l <- melt(cuarta,id.vars=c(1:3),measure.vars=c(4:14))
cuarta.l[,party:=ifelse(variable=="massa"|variable=="grabois","UxP",ifelse(variable=="larreta"|variable=="bullrich","JxC",ifelse(variable=="milei","LLA",ifelse(variable=="bregman"|variable=="solano","FIT",ifelse(variable=="schiaretti","HxNP",ifelse(variable=="otros","otros",ifelse(variable=="blanco","blanco","indecisos")))))))]

### grafico 2 - por alianza, sin concatenar tablas
ggplot(subset(cuarta.l,party!="HxNP"&party!="FIT"&party!="otros"&party!="blanco"&party!="indecisos"&fecha>"2023-01-01"),aes(fecha,value,color=variable))+geom_point()+facet_wrap(~party)+geom_smooth(method = "loess", se =FALSE, show.legend = FALSE,aes(x=fecha,y=value,color=variable))+scale_color_manual(values=cbp1)+ gghighlight(use_direct_label=FALSE)+ylim(0,35)+guides(color = guide_legend(override.aes = list(size=4)))+ylab("Intencion de voto (%)")+ theme_economist()+theme(axis.text.x = element_text(size=10,angle = 0, hjust =0.5))
ggsave("encuestas-post-alianza.png",height=4.5,width=7.5)


### grafico 3 - por candidato, sin concatenar tablas
ggplot(subset(cuarta.l,party!="HxNP"&party!="FIT"&party!="otros"&party!="blanco"&party!="indecisos"&fecha>"2023-01-01"&variable!="manes"&variable!="vidal"),aes(fecha,value,color=variable))+geom_point(subset(cuarta.l,party!="AL"&party!="HxNP"&party!="FIT"&party!="otros"&party!="blanco"&party!="indecisos"&fecha>"2023-01-01"&variable!="manes"&variable!="vidal"&variable!="lousteau"&variable!="morales"&variable!="scioli"&variable!="fernandez"&variable!="depedro"&variable!="kicillof"),mapping=aes(fecha,value,color=variable))+geom_smooth(method = "loess", se =FALSE, show.legend = FALSE,aes(x=fecha,y=value,color=variable))+scale_color_manual(values=cbp1)+ gghighlight(use_direct_label=FALSE)+ylim(0,35)+guides(color = guide_legend(override.aes = list(size=4)))+ylab("Intencion de voto (%)")+ theme_economist()+theme(axis.text.x = element_text(size=10,angle = 0, hjust =0.5))
ggsave("encuestas-post-candidato.png",height=4.5,width=7.5)


######################################################################
### CONCATENAR TABLAS PRE Y POST CIERRE DE LISTAS (SOLO PARA GRAFICAR)
######################################################################

all  <- rbind(sexta,cuarta,fill=TRUE)
all  <- all[,-25]  # Remove "indecisos" column since it gives error, clean code

all.l <- melt(all,id.vars=c(1:3),measure.vars=c(4:26))
all.l[,party:=ifelse(variable=="fernandez"|variable=="kirchner"|variable=="scioli"|variable=="de pedro"|variable=="kicillof"|variable=="massa"|variable=="grabois","UxP",ifelse(variable=="larreta"|variable=="macri"|variable=="bullrich"|variable=="vidal"|variable=="manes"|variable=="lousteau"|variable=="morales","JxC",ifelse(variable=="milei","LLA",ifelse(variable=="bregman"|variable=="solano"|variable=="delcano","FIT",ifelse(variable=="espert","AL",ifelse(variable=="schiaretti","HxNP",ifelse(variable=="otros","otros",ifelse(variable=="blanco","blanco","indecisos"))))))))]


grob <- grobTree(textGrob("Cierre listas", x=0.8,  y=0.95, hjust=0,
                          gp=gpar(col="black", fontsize=13)))

# grafico 4 - por candidatos en internas y otros
ggplot(subset(all.l,party!="AL"&party!="HxNP"&party!="FIT"&party!="otros"&party!="blanco"&party!="indecisos"&fecha>"2023-01-01"&variable!="manes"&variable!="vidal"&variable!="lousteau"&variable!="morales"&variable!="scioli"&variable!="fernandez"&variable!="de pedro"&variable!="kicillof"&variable!="macri"),aes(fecha,value,color=variable))+geom_point()+geom_smooth(method = "auto", se =TRUE, show.legend = FALSE,aes(x=fecha,y=value,color=variable))+geom_vline(xintercept=as.numeric(as.Date("2023-06-24")),color = "black",lty=2, linewidth =1)+scale_color_manual(values=c("deepskyblue3","darkgreen","red","gold1","black","magenta"))+annotation_custom(grob) + gghighlight(use_direct_label=FALSE)+guides(color = guide_legend(override.aes = list(size=4)))+ylab("Intencion de voto (%)")+theme(axis.text.x = element_text(size=12,angle = 0, hjust =0.5))+theme_economist()
ggsave("pre-post.png",height=4.5,width=7.5)

