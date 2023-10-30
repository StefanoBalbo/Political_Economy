rm(list=ls())

getwd()
directorio <- "/Users/stefa/Documents/Code/Political_Economy/eco pol 2023/codigos_clases"
setwd(directorio)

# Load libraries/packages
lapply(c("rio","here","data.table","dplyr","ggplot","RColorBrewer","viridis","areaplot","ggthemes","ggrepel"),require,character.only=TRUE)

# Load data using relative hierarchy

exp <- read.csv("data/country-level-government-spending-vs-income.csv")
#exp <- import(here("slides","epol","data","country-level-government-spending-vs-income.csv"),header=TRUE)
tax <- read.csv("data/country-level-taxes-vs-income.csv")
#tax <- import(here("slides","epol","data","country-level-taxes-vs-income.csv"),header=TRUE)

# Clean data, drop cases
exp <- exp[exp$Code!="QAT",]
exp <- exp[exp$Code!="KIR",]
exp <- exp[exp$Code!="TUV",]
exp2011 <- exp[exp$Year==2011,]
exp2011$`Population (historical estimates)`  <- as.numeric(exp2011$`Population (historical estimates)`)
exp2011  <- exp2011[exp2011$`Population (historical estimates)`<1500000000,]

tax2015 <- tax[tax$Year==2015,]
tax2015$`Population (historical estimates)`  <- as.numeric(tax2015$`Population (historical estimates)`)
tax2015  <- tax2015[tax2015$`Population (historical estimates)`<1500000000,]

# Select countries to include in graph
pointsToLabel <- c("Russia","Venezuela","Iraq","Myanmar","Sudan","Afghanistan","Congo","Greece","Argentina","Italy","South Africa", "Spain","Botswana", "Cape Verde", "Bhutan","Rwanda","France","United States","Germany","Britain","Barbados","Norway","Japan","New Zealand","Singapore","Uruguay","Chile","Turkey","Portugal","Peru","Mexico","Israel","Italy","Lesotho","South Korea","Hong Kong","Lybia","Saudi Arabia","Kuwait","Oman","China","India","Kenya","Brazil","Mexico","Qatar","United Arab Emirates","Ethiophia","Pakistan","Libya","Greece","Fiji","Egypt","Malawi","Mozambique","Burundi")

# Graph and save
png(filename="data/fig01.png",width=1920,height=1080,res=225)
ggplot(exp2011, aes(x=`Government Expenditure (IMF based on Mauro et al. (2015))`,y=`GDP per capita, PPP (constant 2017 international $)`)) + geom_point(colour="purple",alpha=0.5,na.rm=TRUE,aes(size=`Population (historical estimates)`))+scale_size_continuous(range=c(1,30)) + geom_text_repel(size=4,aes(label=Entity), data=exp2011[exp2011$Entity %in% pointsToLabel,],min.segment.length = Inf)+theme(legend.position="none")+ ylab("PIB per capita (2011)")+xlab("Gasto publico como fraccion del PIB (2011)")+ theme(text=element_text(size=14),axis.text=element_text(size=14))+xlim(20,65)+scale_y_log10(breaks=c(1000,2000,5000,10000,20000,50000))
dev.off()

png(filename="data/fig02.png",width=1920,height=1080,res=225)
ggplot(tax2015, aes(x=`Total tax revenue (% of GDP) (ICTD (2021))`, y=`GDP per capita, PPP (constant 2017 international $)`)) + geom_point(colour="purple",alpha=0.5,aes(size=`Population (historical estimates)`))+scale_size_continuous(range=c(1,30))+geom_text_repel(size=4,aes(label=Entity), data=tax2015[tax2015$Entity %in% pointsToLabel,],min.segment.length = Inf)+ylab("PIB per capita (2010)")+xlab("Recaudacion tributaria como fraccion del PIB (2010)")+ theme(text=element_text(size=14),axis.text=element_text(size=14))+xlim(20,50)+scale_y_log10(breaks=c(1000,2000,5000,10000,20000,50000))+theme(legend.position="none")
dev.off()
