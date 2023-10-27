rm(list=ls())

getwd()
directorio <- "/Users/stefa/Documents/Code/Political_Economy/"
setwd(directorio)

# Load libraries/packages
lapply(c("rio","here","data.table","ggplot2","dplyr","tmap","sf","RColorBrewer","viridis","ggthemes","hrbrthemes"),require,character.only=TRUE)

# Load data using relative hierarchy
sha <- read.csv("data/share-democracies-bmr.csv")
#sha  <-  import(here("slides","epol","data","share-democracies-bmr.csv"))
sha  <- sha[sha$Entity=="World",]
sha  <- melt(sha,measure.vars=c("number_nondem_bmr_owid","number_dem_bmr_owid"),id=c("Entity","Year"),variable.factor=c("non dem","dem"))
levels(sha$variable)  <- c("Non-democracies","Democracies")

# Graph and save
#png("slides/epol/output/figs/fig-03-002.png",width=1920,height=1080,res=225)
ggplot(sha,aes(fill=as.factor(variable),x=Year,y=value))+geom_area(stat="identity",position="fill",alpha=0.8 , size=.6, colour="white")+scale_fill_viridis(begin=0.1,end=0.5,discrete = T)+theme_ipsum(grid="XY")+scale_x_continuous(expand=c(0,0)) +theme(axis.text.x=element_text(hjust=c(0, 0.5, 0.5, 0.5, 1))) +  theme(legend.title=element_blank(),legend.position="bottom")+labs(x="Year",y="Proportion of contries")
