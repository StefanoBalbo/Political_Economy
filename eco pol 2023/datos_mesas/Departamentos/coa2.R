directorio <- "C:\\Users\\IDECOR\\Documents\\Code\\Political_Economy\\eco pol 2023\\datos_mesas\\Seba_ver\\"
setwd(directorio); getwd()
library(rgdal)
library(ggplot2)

coa <- read.csv("coa.csv",header=TRUE,sep=";",dec=".")

coa$departamento <- as.factor(toupper(coa$departamento))
coa$provincia <- as.factor(toupper(coa$provincia))
coa$diff1 <- coa$sh_gob-coa$sh_prez
coa$diff1.cat <- as.factor(ifelse(coa$diff1>=0,"POS","NEG"))

arg <- readOGR("departamentos.shp","departamentos",encoding="iso-8859-1")
plot(arg)

### Eliminate Islas Malvinas and Antartida from both
arg <- arg[arg$OBJECTID!="488"&arg$OBJECTID!="489",]
plot(arg)

#2003
map03 <- merge(arg,coa[coa$year==2003,],by.x=c("DEPARTA","PROVINCIA"),by.y=c("departamento","provincia"))
#2007
map07 <- merge(arg,coa[coa$year==2007,],by.x=c("DEPARTA","PROVINCIA"),by.y=c("departamento","provincia"))
#2011
map11 <- merge(arg,coa[coa$year==2011,],by.x=c("DEPARTA","PROVINCIA"),by.y=c("departamento","provincia"))
#2015
map15 <- merge(arg,coa[coa$year==2015,],by.x=c("DEPARTA","PROVINCIA"),by.y=c("departamento","provincia"))

png("mapping.png",width=800,height=1280,res=400)
p1=spplot(map03,"diff1",col.regions=heat.colors(100),at=seq(-1.0,1.0,0.2))
p2=spplot(map07,"diff1",col.regions=heat.colors(100),at=seq(-1.0,1.0,0.2))
p3=spplot(map11,"diff1",col.regions=heat.colors(100),at=seq(-1.0,1.0,0.2))
p4=spplot(map15,"diff1",col.regions=heat.colors(100),at=seq(-1.0,1.0,0.2))
print(p1,position = c(0,0,.33,1),more=T)
print(p2, position = c(.33,0,.66,1),more = T)
print(p3, position = c(0.66,0,1,1))
dev.off()

jpeg("map2003.jpg",height=600,width=480)
spplot(map03,"diff1",col.regions=heat.colors(100),at=seq(-1.0,1.0,0.2),main="GOBsh-PREsh, 2003")
dev.off()

jpeg("map2007.jpg",height=600,width=480)
spplot(map07,"diff1",col.regions=heat.colors(100),at=seq(-1.0,1.0,0.2),main="GOBsh-PREsh, 2007")
dev.off()

jpeg("map2011.jpg",height=600,width=480)
spplot(map11,"diff1",col.regions=heat.colors(100),at=seq(-1.0,1.0,0.2),main="GOBsh-PREsh, 2011")
dev.off()

jpeg("map2015.jpg",height=600,width=480)
spplot(map15,"diff1",col.regions=heat.colors(100),at=seq(-1.0,1.0,0.2),main="GOBsh-PREsh, 2015")
dev.off()


par(mfrow=c(1,4))
spplot(map03,"diff1.cat",col.regions=c("red","green"))
spplot(map07,"diff1.cat",col.regions=c("red","green"))
spplot(map11,"diff1.cat",col.regions=c("red","green"))
spplot(map15,"diff1.cat",col.regions=c("red","green"))

jpeg("correlacion1.jpg",height=480,width=800)
par(mfrow=c(2,2))
plot(map03$sh_gob,map03$sh_prez,col="blue",pch=16,main="Elecci?n 2003",xlab="% votos gobernador electo, por depto",ylab="% votos presidente electo",cex=1.3,cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
plot(map07$sh_gob,map07$sh_prez,col="blue",pch=16,main="Elecci?n 2007",xlab="% votos gobernador electo, por depto",ylab="% votos presidente electo",cex=1.3,cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
plot(map11$sh_gob,map11$sh_prez,col="blue",pch=16,main="Elecci?n 2011",xlab="% votos gobernador electo, por depto",ylab="% votos presidente electo",cex=1.3,cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
plot(map15$sh_gob,map15$sh_prez,col="blue",pch=16,main="Elecci?n 2015",xlab="% votos gobernador electo, por depto",ylab="% votos presidente electo",cex=1.3,cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
dev.off()


jpeg("correlacion2.jpg",height=480,width=800)
par(mfrow=c(2,2))
plot(map03$sh_prez,map03$sh_gob,col="blue",pch=16,xlim=c(0,1),ylim=c(0,1),main="Elecci?n 2003",xlab="% votos presidente electo, por depto",ylab="% votos gobernador electo",cex=1.3,cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
plot(map07$sh_prez,map07$sh_gob,col="blue",pch=16,xlim=c(0,1),ylim=c(0,1),main="Elecci?n 2007",xlab="% votos presidente electo, por depto",ylab="% votos gobernador electo",cex=1.3,cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
plot(map11$sh_prez,map11$sh_gob,col="blue",pch=16,xlim=c(0,1),ylim=c(0,1),main="Elecci?n 2011",xlab="% votos presidente electo, por depto",ylab="% votos gobernador electo",cex=1.3,cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
plot(map15$sh_prez,map15$sh_gob,col="blue",pch=16,xlim=c(0,1),ylim=c(0,1),main="Elecci?n 2015",xlab="% votos presidente electo, por depto",ylab="% votos gobernador electo",cex=1.3,cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
dev.off()


jpeg("coattails1.jpg",height=500,width=800)
par(mfrow=c(2,2))
    plot(map03$sh_prez,map03$sh_gob,col=c("blue","red")[factor(map03$diff1.cat)],pch=16,xlim=c(0,1),ylim=c(0,1),main="% votos presidente y gobernador (electos)\n Por depto. A?o 2003",xlab="% votos presidente electo",ylab="% votos gobernador electo",cex=1.5,cex.main=1.5,cex.lab=1.5)
    legend("bottomright",legend=c("GOBsh-PREsh=pos","GOBsh-PREsh=neg"),col=c("blue","red"),lty=1:1,lwd=4)
abline(0,1,lwd=2)
 plot(map07$sh_prez,map07$sh_gob,col=c("blue","red")[factor(map07$diff1.cat)],pch=16,xlim=c(0,1),ylim=c(0,1),main="% votos presidente y gobernador (electos)\n Por depto. A?o 2007",xlab="% votos presidente electo",ylab="% votos gobernador electo",cex=1.5,cex.main=1.5,cex.lab=1.5)
    legend("bottomright",legend=c("GOBsh-PREsh=pos","GOBsh-PREsh=neg"),col=c("blue","red"),lty=1:1,lwd=4)
abline(0,1,lwd=2)
 plot(map11$sh_prez,map11$sh_gob,col=c("blue","red")[factor(map11$diff1.cat)],pch=16,xlim=c(0,1),ylim=c(0,1),main="% votos presidente y gobernador (electos)\n Por depto. A?o 2011",xlab="% votos presidente electo",ylab="% votos gobernador electo",cex=1.5,cex.main=1.5,cex.lab=1.5)
    legend("bottomright",legend=c("GOBsh-PREsh=pos","GOBsh-PREsh=neg"),col=c("blue","red"),lty=1:1,lwd=4)
abline(0,1,lwd=2)
 plot(map15$sh_prez,map15$sh_gob,col=c("blue","red")[factor(map15$diff1.cat)],pch=16,xlim=c(0,1),ylim=c(0,1),main="% votos presidente y gobernador (electos)\n Por depto. A?o 2015",xlab="% votos presidente electo",ylab="% votos gobernador electo",cex=1.5,cex.main=1.5,cex.lab=1.5)
    legend("bottomright",legend=c("GOBsh-PREsh=pos","GOBsh-PREsh=neg"),col=c("blue","red"),lty=1:1,lwd=4)
abline(0,1,lwd=2)
dev.off()
