#### plotting function
library(sp)
library(plotrix)



grid.size.max <- 40
grid.size.min <- 0
coords <- matrix(c(grid.size.min         , grid.size.min ,
                   grid.size , grid.size.min ,
                   grid.size , grid.size ,
                   grid.size.min          , grid.size,
                   grid.size.min          , grid.size.min
), ncol = 2, byrow = TRUE)

P1 <- Polygon(coords)
myStudyArea.poly <-  SpatialPolygons(list(Polygons(list(P1), ID = "a")),
                                     proj4string=CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))



pdf(file="path.pdf", width=7, height = 7)
par(mar=c(2,2,2,2))
plot(myStudyArea.poly, xlim=c(-20,60), ylim=c(-25,60), border="white")
axis(1)

axis(2)

cex.text <- 0.6
cex.text1 <- 0.75
offset.poly <- 2


TREATMENT=cbind(c(-12,3,3,-12), c(16,16,23,23)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(TREATMENT[,1], TREATMENT[,2], col =  adjustcolor("orange",alpha.f = 0.5), border = "white")
text(mean(TREATMENT[,1]),mean(TREATMENT[,2]), "TREATMENT", cex = cex.text1)

Pres=cbind(c(47,65,65,47), c(16,16,23,23))
polygon(Pres[,1], Pres[,2], col =  adjustcolor("blue",alpha.f = 0.5), border = "white")
text(mean(Pres[,1]),mean(Pres[,2]), "PRESENCE", cex = cex.text1) 


### FOOD 


SAI_sd=cbind(c(9,25,25,9), c(8,8,12,12))
LAI_sd=cbind(c(9,25,25,9), c(2,2,6,6))
biom=cbind(c(9,25,25,9), c(-4,-4,0,0))

food= rbind(SAI_sd,LAI_sd,biom)
min1 <- min(food[,1])-offset.poly
min2 <- min(food[,2])-offset.poly
max1 <- max(food[,1])+offset.poly
max2 <- max(food[,2])+offset.poly


polygon(c(min1,max1,max1,min1) ,
        c(min2,min2,max2,max2)  ,
         col =  adjustcolor("purple",alpha.f = 0.1), border = "white")

  
polygon(SAI_sd[,1], SAI_sd[,2], col =  adjustcolor("purple",alpha.f = 0.5), border = "white")
text(mean(SAI_sd[,1]),mean(SAI_sd[,2]), "SAI", cex = cex.text)

polygon(LAI_sd[,1], LAI_sd[,2], col =  adjustcolor("purple",alpha.f = 0.5), border = "white")
text(mean(LAI_sd[,1]),mean(LAI_sd[,2]), "LAI", cex = cex.text)

polygon(biom[,1], biom[,2], col =  adjustcolor("purple",alpha.f = 0.5), border = "white")
text(mean(biom[,1]),mean(biom[,2]), "ORTHOPTERA", cex = cex.text)




## VEGETATION 
Diver=cbind(c(9,25,25,9), c(28,28,32,32))
Heter=cbind(c(9,25,25,9), c(34,34,38,38))
Height=cbind(c(9,25,25,9), c(40,40,44,44))
Cover=cbind(c(9,25,25,9), c(46,46,50,50))
Cover_dead=cbind(c(9,25,25,9), c(52,52,56,56))


veg= rbind(Diver,Heter,Height,Cover,Cover_dead)
min1 <- min(veg[,1])-offset.poly
min2 <- min(veg[,2])-offset.poly
max1 <- max(veg[,1])+offset.poly
max2 <- max(veg[,2])+offset.poly


polygon(c(min1,max1,max1,min1) ,
        c(min2,min2,max2,max2)  ,
        col =  adjustcolor("green",alpha.f = 0.1), border = "white")


polygon(Diver[,1], Diver[,2], col =  adjustcolor("green",alpha.f = 0.5), border = "white")
text(mean(Diver[,1]),mean(Diver[,2]), "DIVERSITY", cex = cex.text)

polygon(Heter[,1], Heter[,2], col =  adjustcolor("green",alpha.f = 0.5), border = "white")
text(mean(Heter[,1]),mean(Heter[,2]), "HETEROGENEITY", cex = cex.text)

polygon(Height[,1], Height[,2], col =  adjustcolor("green",alpha.f = 0.5), border = "white")
text(mean(Height[,1]),mean(Height[,2]), "HEIGHT", cex = cex.text)

polygon(Cover[,1], Cover[,2], col =  adjustcolor("green",alpha.f = 0.5), border = "white")
text(mean(Cover[,1]),mean(Cover[,2]), "COVER", cex = cex.text)

polygon(Cover_dead[,1], Cover_dead[,2], col =  adjustcolor("green",alpha.f = 0.5), border = "white")
text(mean(Cover_dead[,1]),mean(Cover_dead[,2]), "COVER_DEAD", cex = cex.text)

##LANDSCAPE ##
tbl=cbind(c(40,54,54,40), c(-4,-4,0,0))
par=cbind(c(40,54,54,40), c(-10,-10,-6,-6))
Fallow=cbind(c(40,54,54,40), c(-16,-16,-12,-12))
Irrig=cbind(c(40,54,54,40), c(-22,-22,-18,-18))


landscape= rbind(tbl,par,Fallow,Irrig)
min1 <- min(landscape[,1])-offset.poly
min2 <- min(landscape[,2])-offset.poly
max1 <- max(landscape[,1])+offset.poly
max2 <- max(landscape[,2])+offset.poly


polygon(c(min1,max1,max1,min1) ,
        c(min2,min2,max2,max2)  ,
        col =  adjustcolor("brown",alpha.f = 0.1), border = "white")


polygon(tbl[,1], tbl[,2], col =  adjustcolor("brown",alpha.f = 0.5), border = "white")
text(mean(tbl[,1]),mean(tbl[,2]), "TBL", cex = cex.text)


polygon(par[,1], par[,2], col =  adjustcolor("brown",alpha.f = 0.5), border = "white")
text(mean(par[,1]),mean(par[,2]), "PAR", cex = cex.text)

polygon(Fallow[,1], Fallow[,2], col =  adjustcolor("brown",alpha.f = 0.5), border = "white")
text(mean(Fallow[,1]),mean(Fallow[,2]), "FALLOW", cex = cex.text)

polygon(Irrig[,1], Irrig[,2], col =  adjustcolor("brown",alpha.f = 0.5), border = "white")
text(mean(Irrig[,1]),mean(Irrig[,2]), "IRRIGATION", cex = cex.text)



#FROM TREATMENT 
arrows(x0=TREATMENT[2,1], x1=Pres[1,1], y0=mean(TREATMENT[2:3,2]), y1= mean(Pres[c(1,4),2]),length = 0.1)
arrows(x0=mean(TREATMENT[c(1:2),1]), x1=SAI_sd[1,1], y0=mean(TREATMENT[1,2]), y1= mean(SAI_sd[c(1,4),2]),length = 0.1)
arrows(x0=mean(TREATMENT[c(1:2),1]), x1=LAI_sd[1,1], y0=mean(TREATMENT[1,2]), y1= mean(LAI_sd[c(1,4),2]),length = 0.1)
arrows(x0=mean(TREATMENT[c(1:2),1]), x1=biom[1,1], y0=mean(TREATMENT[1,2]), y1= mean(biom[c(1,4),2]),length = 0.1)
arrows(x0=mean(TREATMENT[c(1:2),1]), x1=Diver[1,1], y0=mean(TREATMENT[3,2]), y1= mean(Diver[c(1,4),2]),length = 0.1)
arrows(x0=mean(TREATMENT[c(1:2),1]), x1=Heter[1,1], y0=mean(TREATMENT[3,2]), y1= mean(Heter[c(1,4),2]),length = 0.1)
arrows(x0=mean(TREATMENT[c(1:2),1]), x1=Height[1,1], y0=mean(TREATMENT[3,2]), y1= mean(Height[c(1,4),2]),length = 0.1)
arrows(x0=mean(TREATMENT[c(1:2),1]), x1=Cover[1,1], y0=mean(TREATMENT[3,2]), y1= mean(Cover[c(1,4),2]),length = 0.1)
arrows(x0=mean(TREATMENT[c(1:2),1]), x1=Cover_dead[1,1], y0=mean(TREATMENT[3,2]), y1= mean(Cover_dead[c(1,4),2]),length = 0.1)

#from vegetation 
arrows(x0=mean(Diver[c(2),1]), x1=mean(Pres[1:2,1]), y0=mean(Diver[3:2,2]), y1= mean(Pres[c(4),2]),length = 0.1)
arrows(x0=mean(Heter[c(2),1]), x1=mean(Pres[1:2,1]), y0=mean(Heter[3:2,2]), y1= mean(Pres[c(4),2]),length = 0.1)
arrows(x0=mean(Height[c(2),1]), x1=mean(Pres[1:2,1]), y0=mean(Height[3:2,2]), y1= mean(Pres[c(4),2]),length = 0.1)
arrows(x0=mean(Cover[c(2),1]), x1=mean(Pres[1:2,1]), y0=mean(Cover[3:2,2]), y1= mean(Pres[c(4),2]),length = 0.1)
arrows(x0=mean(Cover_dead[c(2),1]), x1=mean(Pres[1:2,1]), y0=mean(Cover_dead[3:2,2]), y1= mean(Pres[c(4),2]),length = 0.1)

#from landscape
arrows(x0=mean(tbl[c(1),1]), x1=mean(biom[2,1]), y0=mean(tbl[3:2,2]), y1= mean(biom[c(2:3),2]),length = 0.1)
arrows(x0=mean(par[c(1),1]), x1=mean(biom[2,1]), y0=mean(par[3:2,2]), y1= mean(biom[c(2:3),2]),length = 0.1)
arrows(x0=mean(Fallow[c(1),1]), x1=mean(biom[2,1]), y0=mean(Fallow[3:2,2]), y1= mean(biom[c(2:3),2]),length = 0.1)
arrows(x0=mean(Irrig[c(1),1]), x1=mean(biom[2,1]), y0=mean(Irrig[3:2,2]), y1= mean(biom[c(2:3),2]),length = 0.1)


arrows(x0=mean(Pres[1:2,1])+6, x1=mean(Pres[1:2,1])+6, y0=mean(Irrig[3:2,2]), y1= mean(Pres[c(2),2]),length = 0.1)
segments(x0=mean(Irrig[2,1]), x1=mean(Pres[1:2,1])+6, y0=mean(Irrig[3:2,2]), y1= mean(Irrig[3:2,2]))


arrows(x0=mean(Pres[1:2,1])+4, x1=mean(Pres[1:2,1])+4, y0=mean(Fallow[3:2,2]), y1= mean(Pres[c(2),2]),length = 0.1)
segments(x0=mean(Fallow[2,1]), x1=mean(Pres[1:2,1])+4, y0=mean(Fallow[3:2,2]), y1= mean(Fallow[3:2,2]))

arrows(x0=mean(Pres[1:2,1])+1, x1=mean(Pres[1:2,1])+1, y0=mean(par[3:2,2]), y1= mean(Pres[c(2),2]),length = 0.1)
segments(x0=mean(par[2,1]), x1=mean(Pres[1:2,1])+1, y0=mean(par[3:2,2]), y1= mean(par[3:2,2]))

arrows(x0=mean(Pres[1:2,1])-1, x1=mean(Pres[1:2,1])-1, y0=mean(tbl[3:2,2]), y1= mean(Pres[c(2),2]),length = 0.1)
segments(x0=mean(tbl[2,1]), x1=mean(Pres[1:2,1])-1, y0=mean(tbl[3:2,2]), y1= mean(tbl[3:2,2]))



#from vegetation 
arrows(x0=SAI_sd[3,1], x1=Pres[1,1], y0=SAI_sd[3,2], y1= Pres[1,2],length = 0.1)
arrows(x0=LAI_sd[3,1], x1=Pres[1,1], y0=LAI_sd[3,2], y1= Pres[1,2],length = 0.1)
arrows(x0=biom[3,1], x1=Pres[1,1], y0=biom[3,2], y1= Pres[1,2],length = 0.1)


dev.off()


