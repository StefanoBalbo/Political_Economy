rm(list=ls())

getwd()
directorio <- "/Users/stefa/Documents/Code/Political_Economy/"
setwd(directorio)

load("Latinobarometro/Latinobarometro_2020_Eng_Rdata_v1_0.rdata")

names(Latinobarometro_2020_Eng)
View(Latinobarometro_2020_Eng)
