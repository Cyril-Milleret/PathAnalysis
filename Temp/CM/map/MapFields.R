rm(list=ls())
# MAPS PARA MI NOVIA! 
library(rgdal)
library(sp)
library(maptools)
setwd("C:/Personal_Cloud/OneDrive/Work/Ana/PathAnalysis/data")
## ---- 1. LOAD THE DATA ---- 
FIELDS2015 <- readOGR(".", "SG_2015_ANALYZED")
FIELDS2016 <- readOGR(".", "SG_2016_ANALYZED")
FIELDS2017 <- readOGR(".", "SG_2017_ANALYZED")

SPA <-  readOGR(".", "Special_Protection_Areas")

#SPLIT BY THE 3 ZONES
SPA1 <- SPA[SPA$NOM_N2==SPA$NOM_N2[1],]
SPA2 <- SPA[SPA$NOM_N2==SPA$NOM_N2[2],]
SPA3 <- SPA[SPA$NOM_N2==SPA$NOM_N2[6],]
SPA4 <- SPA[SPA$NOM_N2==SPA$NOM_N2[9],]
SPA5 <- SPA[SPA$NOM_N2==SPA$NOM_N2[11],]
SPA6 <- SPA[SPA$NOM_N2==SPA$NOM_N2[15],]

SPA_1 <- as(rbind(SPA1,SPA3,SPA5), "SpatialPolygonsDataFrame")
SPA_1$ID <- 1
SPA_1 <- unionSpatialPolygons(SPA_1,  SPA_1$ID, threshold=NULL, 
                              avoidGEOS=FALSE, avoidUnaryUnion=FALSE)

SPA_2 <- as(SPA2, "SpatialPolygonsDataFrame")
SPA_2$ID <- 1
SPA_2 <- unionSpatialPolygons(SPA_2,  SPA_2$ID, threshold=NULL, 
                              avoidGEOS=FALSE, avoidUnaryUnion=FALSE)
SPA_3 <- as(rbind(SPA6,SPA4), "SpatialPolygonsDataFrame")
SPA_3$ID <- 1
SPA_3 <- unionSpatialPolygons(SPA_3,  SPA_3$ID, threshold=NULL, 
                              avoidGEOS=FALSE, avoidUnaryUnion=FALSE)

## CHECK
plot(SPA)

plot(SPA_1, col="red",add=T)
plot(SPA_2, col="blue",add=T)
plot(SPA_3, col="yellow",add=T)

## ---- 2. SET COLORS ---- 
FIELDS2015$col <- "yellow"  
FIELDS2015$col[FIELDS2015$agr_prc=="C"] <- "blue"
FIELDS2015$col[FIELDS2015$agr_prc=="A"] <- "darkgreen"
FIELDS2015$col[FIELDS2015$agr_prc=="S"] <- "green"
FIELDS2015$col[FIELDS2015$agr_prc=="T"] <- "orange"


FIELDS2016$col <- "yellow"  
FIELDS2016$col[FIELDS2016$agr_prc=="C"] <- "blue"
FIELDS2016$col[FIELDS2016$agr_prc=="A"] <- "darkgreen"
FIELDS2016$col[FIELDS2016$agr_prc=="S"] <- "green"
FIELDS2016$col[FIELDS2016$agr_prc=="T"] <- "orange"


FIELDS2017$col <- "yellow"  
FIELDS2017$col[FIELDS2017$agr_prc=="C"] <- "blue"
FIELDS2017$col[FIELDS2017$agr_prc=="A"] <- "darkgreen"
FIELDS2017$col[FIELDS2017$agr_prc=="S"] <- "green"
FIELDS2017$col[FIELDS2017$agr_prc=="T"] <- "orange"

# colors background map
col.back <- grey(0.8)
# size of the scale (I assume the coords are in meters)
scale.size <- 5000
## ---- 3. PLOT DATA ---- 
#2015
pdf(file="Fields2015.pdf",width = 5,height = 8)
#ZONE 1
par(mfrow=c(3,1),mar=c(0,0,0,0))
plot(SPA_1, col=col.back, border=col.back)
plot(FIELDS2015, add=T, col=adjustcolor(FIELDS2015$col, alpha.f = 0.8), 
     border=FIELDS2015$col, lwd=0.1)
segments(x0=325000, x1=325000 + scale.size,y0= 4615000,y1= 4615000)
text(325000 + scale.size/2, y= 4615000+1000, "5 km")
box()
legend("bottomleft",fill=adjustcolor(c("darkgreen","blue","green","yellow","orange"),alpha.f = 0.8),
       border=c("darkgreen","blue","green","yellow","orange"),legend = c("A", "C", "S", "S+H", "T"),
       box.lwd=0.1,
       bty = "n")
mtext("A)", side=3,line = -2,adj = 1)

#ZONE 2
plot(SPA_2, col=col.back, border=col.back)
plot(FIELDS2015, add=T, col = adjustcolor(FIELDS2015$col, alpha.f = 0.8), 
     border=FIELDS2015$col, lwd=0.1)
segments(x0=340000, x1=340000 + scale.size, y0= 4601500, y1= 4601500)
text(340000 + scale.size/2, y= 4601500+500, "5 km")
box()
mtext("B)", side=3,line = -2,adj = 1)

#ZONE 3 
plot(SPA_3, col=col.back, border = col.back)
plot(FIELDS2015, add=T, col = adjustcolor(FIELDS2015$col, alpha.f = 0.8), 
     border=FIELDS2015$col, lwd=0.1)
segments(x0=300000, x1=300000 + scale.size, y0= 4585000, y1= 4585000)
text(300000 + scale.size/2, y= 4585000+700, "5 km")
box()
mtext("C)", side=3,line = -2,adj = 1)

dev.off()


#2016
pdf(file="Fields2016.pdf",width = 5,height = 8)
#ZONE 1
par(mfrow=c(3,1),mar=c(0,0,0,0))
plot(SPA_1, col=col.back, border=col.back)
plot(FIELDS2016, add=T, col=adjustcolor(FIELDS2016$col, alpha.f = 0.8), 
     border=FIELDS2016$col, lwd=0.1)
segments(x0=325000, x1=325000 + scale.size,y0= 4615000,y1= 4615000)
text(325000 + scale.size/2, y= 4615000+1000, "5 km")
box()
legend("bottomleft",fill=adjustcolor(c("darkgreen","blue","green","yellow","orange"),alpha.f = 0.8),
       border=c("darkgreen","blue","green","yellow","orange"),legend = c("A", "C", "S", "S+H", "T"),
       box.lwd=0.1,
       bty = "n")
mtext("A)", side=3,line = -2,adj = 1)

#ZONE 2
plot(SPA_2, col=col.back, border=col.back)
plot(FIELDS2016, add=T, col = adjustcolor(FIELDS2016$col, alpha.f = 0.8), 
     border=FIELDS2016$col, lwd=0.1)
segments(x0=340000, x1=340000 + scale.size, y0= 4601500, y1= 4601500)
text(340000 + scale.size/2, y= 4601500+500, "5 km")
box()
mtext("B)", side=3,line = -2,adj = 1)

#ZONE 3 
plot(SPA_3, col=col.back, border = col.back)
plot(FIELDS2016, add=T, col = adjustcolor(FIELDS2016$col, alpha.f = 0.8), 
     border=FIELDS2016$col, lwd=0.1)
segments(x0=300000, x1=300000 + scale.size, y0= 4585000, y1= 4585000)
text(300000 + scale.size/2, y= 4585000+700, "5 km")
box()
mtext("C)", side=3,line = -2,adj = 1)

dev.off()



#2017
pdf(file="Fields2017.pdf",width = 5,height = 8)
#ZONE 1
par(mfrow=c(3,1),mar=c(0,0,0,0))
plot(SPA_1, col=col.back, border=col.back)
plot(FIELDS2017, add=T, col=adjustcolor(FIELDS2017$col, alpha.f = 0.8), 
     border=FIELDS2017$col, lwd=0.1)
segments(x0=325000, x1=325000 + scale.size,y0= 4615000,y1= 4615000)
text(325000 + scale.size/2, y= 4615000+1000, "5 km")
box()
legend("bottomleft",fill=adjustcolor(c("darkgreen","blue","green","yellow","orange"),alpha.f = 0.8),
       border=c("darkgreen","blue","green","yellow","orange"),legend = c("A", "C", "S", "S+H", "T"),
       box.lwd=0.1,
       bty = "n")
mtext("A)", side=3,line = -2,adj = 1)

#ZONE 2
plot(SPA_2, col=col.back, border=col.back)
plot(FIELDS2017, add=T, col = adjustcolor(FIELDS2017$col, alpha.f = 0.8), 
     border=FIELDS2017$col, lwd=0.1)
segments(x0=340000, x1=340000 + scale.size, y0= 4601500, y1= 4601500)
text(340000 + scale.size/2, y= 4601500+500, "5 km")
box()
mtext("B)", side=3,line = -2,adj = 1)

#ZONE 3 
plot(SPA_3, col=col.back, border = col.back)
plot(FIELDS2017, add=T, col = adjustcolor(FIELDS2017$col, alpha.f = 0.8), 
     border=FIELDS2017$col, lwd=0.1)
segments(x0=300000, x1=300000 + scale.size, y0= 4585000, y1= 4585000)
text(300000 + scale.size/2, y= 4585000+700, "5 km")
box()
mtext("C)", side=3,line = -2,adj = 1)

dev.off()

