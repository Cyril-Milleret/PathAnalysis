#### plotting function
rm(list=ls())
library(sp)
library(plotrix)



grid.size.max <- 40
grid.size.min <- 0
coords <- matrix(c(grid.size.min         , grid.size.min ,
                   grid.size.max , grid.size.min ,
                   grid.size.max , grid.size.max ,
                   grid.size.min          , grid.size.max,
                   grid.size.min          , grid.size.min
), ncol = 2, byrow = TRUE)

P1 <- Polygon(coords)
myStudyArea.poly <-  SpatialPolygons(list(Polygons(list(P1), ID = "a")),
                                     proj4string=CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))


### LOAD THE DATA 
setwd("C:/My_documents/ana/PathAnalysis")

load("ecoef.RData")

e.coefs <- e.coefs2

source("C:/My_documents/ana/PathAnalysis/PathAnalysis/Functions/PlotPath.R")




setwd("C:/My_documents/ana/PathAnalysis/PathAnalysis")
pdf(file="path.pdf", width=7, height = 6.5)
par(mar=c(1,1,1,1))

PlotPath(e.coefs
          ,cex.text =0.6
          ,cex.text1 = 0.75
          ,offset.poly = 2
          ,significant = 0.05
          ,xlim=c(-20,70)
          ,ylim=c(-30,60)
          ,col.pos="blue"
          ,col.neg="red"
          ,col.non.signifi="grey"
          ,Treatment.name= "CYRIL"
          ,Species.name="PRESENCE \n cyril"
          ,cex.category = 0.5
          ,plot.axis=FALSE
          ,text.box.width=c(2, 1))


dev.off()


