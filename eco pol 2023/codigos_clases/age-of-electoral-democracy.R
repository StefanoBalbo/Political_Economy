rm(list=ls())

getwd()
directorio <- "/Users/stefa/Documents/Code/Political_Economy/eco pol 2023/codigos_clases/"
setwd(directorio)

# Load libraries/packages
lapply(c("rio","here","data.table","dplyr","tmap","sf","RColorBrewer","viridis","areaplot","ggthemes"),require,character.only=TRUE)

# Load data and geography using relative hierarchy
dem <- read.csv("data/age-of-electoral-democracy.csv")
dem  <- rio::import(here("data/age-of-electoral-democracy.csv")) %>% data.table()
map <- st_read("data//") # FALTA ESTE MAPA
map <- here("maps","World_Countries__Generalized_.shp") %>% st_read()

# Join data and geography, create categories
join  <- merge(map,dem,by.x="COUNTRYAFF",by.y="Entity")
join  <- join[join$Year==2021,]
join$cat  <- cut(as.numeric(join$electdem_age_row_owid),breaks=c(0,18,30,60,90,173))
join$cat  <- ifelse(join$electdem_age_row_owid=="electoral autocracy","elec autoc",ifelse(join$electdem_age_row_owid=="closed autocracy","closed autoc",ifelse(as.numeric(join$electdem_age_row_owid)>=0&as.numeric(join$electdem_age_row_owid)<=18,"0 a 18",ifelse(as.numeric(join$electdem_age_row_owid)>=19&as.numeric(join$electdem_age_row_owid)<=30,"19 a 30",ifelse(as.numeric(join$electdem_age_row_owid)>=31&as.numeric(join$electdem_age_row_owid)<=60,"31 a 60",ifelse(as.numeric(join$electdem_age_row_owid)>=61&as.numeric(join$electdem_age_row_owid)<=90,"61 a 90",ifelse(as.numeric(join$electdem_age_row_owid)>=91,"91 a 173","no data")))))))

# Graph to console/browser
pal  <- c(rgb(253,231,37,maxColorValue=255),rgb(109,205,89,maxColorValue=255),rgb(31,158,137,maxColorValue=255),rgb(62,74,137,maxColorValue=255),rgb(68,1,84,maxColorValue=255),"burlywood4","burlywood3")

tmap_mode("view")
tm_shape(join) +tm_polygons("cat",palette=pal)

