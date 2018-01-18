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




par(mar=c(2,2,2,2))
plot(myStudyArea.poly, xlim=c(-20,60), ylim=c(-25,50), border="white")
axis(1)

axis(2)



TREATMENT=cbind(c(-11,2,2,-11), c(18,18,22,22)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(TREATMENT[,1], TREATMENT[,2], col =  adjustcolor("orange",alpha.f = 0.5), border = "white")
text(-10,20, "TREATMENT")

Pres=cbind(c(39,52,52,39), c(18,18,22,22))
polygon(Pres[,1], Pres[,2], col =  adjustcolor("blue",alpha.f = 0.5), border = "white")
text(40,20, "PRESENCE")


### FOOD 
SAI_sd=cbind(c(9,25,25,9), c(8,8,12,12))
polygon(SAI_sd[,1], SAI_sd[,2], col =  adjustcolor("purple",alpha.f = 0.5), border = "white")
text(10,10, "SAI", cex = 0.8)

LAI_sd=cbind(c(9,25,25,9), c(2,2,6,6))
polygon(LAI_sd[,1], LAI_sd[,2], col =  adjustcolor("purple",alpha.f = 0.5), border = "white")
text(10,4, "LAI", cex = 0.8)

biom=cbind(c(9,25,25,9), c(-4,-4,0,0))
polygon(biom[,1], biom[,2], col =  adjustcolor("purple",alpha.f = 0.5), border = "white")
text(10,-2, "ORTHOPTERA", cex = 0.8)




## VEGETATION 
veg=cbind(c(6,28,28,6), c(25,25,59,59))
polygon(veg[,1], veg[,2], col =  adjustcolor("green",alpha.f = 0.2), border = "white")


Diver=cbind(c(9,25,25,9), c(28,28,32,32))
polygon(Diver[,1], Diver[,2], col =  adjustcolor("green",alpha.f = 0.5), border = "white")
text(10,30, "DIVERSITY", cex = 0.8)

Heter=cbind(c(9,25,25,9), c(34,34,38,38))
polygon(Heter[,1], Heter[,2], col =  adjustcolor("green",alpha.f = 0.5), border = "white")
text(10,36, "HETEROGENEITY", cex = 0.8)

Height=cbind(c(9,25,25,9), c(40,40,44,44))
polygon(Height[,1], Height[,2], col =  adjustcolor("green",alpha.f = 0.5), border = "white")
text(10,42, "HEIGHT", cex = 0.8)

Cover=cbind(c(9,25,25,9), c(46,46,50,50))
polygon(Cover[,1], Cover[,2], col =  adjustcolor("green",alpha.f = 0.5), border = "white")
text(10,48, "COVER", cex = 0.8)

Cover_dead=cbind(c(9,25,25,9), c(52,52,56,56))
polygon(Cover_dead[,1], Cover_dead[,2], col =  adjustcolor("green",alpha.f = 0.5), border = "white")
text(10,54, "COVER_DEAD", cex = 0.8)

##LANDSCAPE ##

tbl=cbind(c(40,54,54,40), c(-4,-4,0,0))
polygon(tbl[,1], tbl[,2], col =  adjustcolor("brown",alpha.f = 0.5), border = "white")
text(43,-2, "TBL", cex = 0.8)


par=cbind(c(40,54,54,40), c(-10,-10,-6,-6))
polygon(par[,1], par[,2], col =  adjustcolor("brown",alpha.f = 0.5), border = "white")
text(43,-8, "PAR", cex = 0.8)

Fallow=cbind(c(40,54,54,40), c(-16,-16,-12,-12))
polygon(Fallow[,1], Fallow[,2], col =  adjustcolor("brown",alpha.f = 0.5), border = "white")
text(43,-14, "FALLOW", cex = 0.8)

Irrig=cbind(c(40,54,54,40), c(-22,-22,-18,-18))
polygon(Irrig[,1], Irrig[,2], col =  adjustcolor("brown",alpha.f = 0.5), border = "white")
text(43,-20, "IRRIGATION", cex = 0.8)


arrows(x0=TREATMENT[2,1], x1=Pres[1,1], y0=mean(TREATMENT[2:3,2]), y1= mean(Pres[c(1,4),2]),length = 0.1)

arrows(x0=SAI_sd[3,1], x1=Pres[1,1], y0=SAI_sd[3,2], y1= Pres[1,2],length = 0.1)
arrows(x0=LAI_sd[3,1], x1=Pres[1,1], y0=LAI_sd[3,2], y1= Pres[1,2],length = 0.1)
arrows(x0=biom[3,1], x1=Pres[1,1], y0=biom[3,2], y1= Pres[1,2],length = 0.1)

arrows(x0=mean(TREATMENT[c(1:2),1]), x1=SAI_sd[1,1], y0=mean(TREATMENT[1,2]), y1= mean(SAI_sd[c(1,4),2]),length = 0.1)
arrows(x0=mean(TREATMENT[c(1:2),1]), x1=LAI_sd[1,1], y0=mean(TREATMENT[1,2]), y1= mean(LAI_sd[c(1,4),2]),length = 0.1)
arrows(x0=mean(TREATMENT[c(1:2),1]), x1=biom[1,1], y0=mean(TREATMENT[1,2]), y1= mean(biom[c(1,4),2]),length = 0.1)




arrows(x0=mean(TREATMENT[c(1:2),1]), x1=Diver[1,1], y0=mean(TREATMENT[3,2]), y1= mean(Diver[c(1,4),2]),length = 0.1)
arrows(x0=mean(TREATMENT[c(1:2),1]), x1=Heter[1,1], y0=mean(TREATMENT[3,2]), y1= mean(Heter[c(1,4),2]),length = 0.1)
arrows(x0=mean(TREATMENT[c(1:2),1]), x1=Height[1,1], y0=mean(TREATMENT[3,2]), y1= mean(Height[c(1,4),2]),length = 0.1)
arrows(x0=mean(TREATMENT[c(1:2),1]), x1=Cover[1,1], y0=mean(TREATMENT[3,2]), y1= mean(Cover[c(1,4),2]),length = 0.1)
arrows(x0=mean(TREATMENT[c(1:2),1]), x1=Cover_dead[1,1], y0=mean(TREATMENT[3,2]), y1= mean(Cover_dead[c(1,4),2]),length = 0.1)


arrows(x0=mean(Diver[c(2),1]), x1=mean(Pres[1:2,1]), y0=mean(Diver[3:2,2]), y1= mean(Pres[c(4),2]),length = 0.1)
arrows(x0=mean(Heter[c(2),1]), x1=mean(Pres[1:2,1]), y0=mean(Heter[3:2,2]), y1= mean(Pres[c(4),2]),length = 0.1)
arrows(x0=mean(Height[c(2),1]), x1=mean(Pres[1:2,1]), y0=mean(Height[3:2,2]), y1= mean(Pres[c(4),2]),length = 0.1)
arrows(x0=mean(Cover[c(2),1]), x1=mean(Pres[1:2,1]), y0=mean(Cover[3:2,2]), y1= mean(Pres[c(4),2]),length = 0.1)
arrows(x0=mean(Cover_dead[c(2),1]), x1=mean(Pres[1:2,1]), y0=mean(Cover_dead[3:2,2]), y1= mean(Pres[c(4),2]),length = 0.1)


arrows(x0=mean(tbl[c(1),1]), x1=mean(biom[2,1]), y0=mean(tbl[3:2,2]), y1= mean(biom[c(2:3),2]),length = 0.1)
arrows(x0=mean(par[c(1),1]), x1=mean(biom[2,1]), y0=mean(par[3:2,2]), y1= mean(biom[c(2:3),2]),length = 0.1)
arrows(x0=mean(Fallow[c(1),1]), x1=mean(biom[2,1]), y0=mean(Fallow[3:2,2]), y1= mean(biom[c(2:3),2]),length = 0.1)
arrows(x0=mean(Irrig[c(1),1]), x1=mean(biom[2,1]), y0=mean(Irrig[3:2,2]), y1= mean(biom[c(2:3),2]),length = 0.1)












text( mean(c(SAI_sd[3,1],Pres[1,1])), mean(c(SAI_sd[3,2], Pres[1,2])), "0.78" , offset = 5 )



textbox(x = c(0,0), y= 20,textlist = "TREATMENT", fill="red", box=T)
plot.new()
textbox(c(0,0.2), 1, c("many words","more words","why not?",
                       "keep going",rep("and going",10)))
textbox(c(0.3,0.5), 1, c("keep going",rep("and going",10)), cex=0.45,
        col="blue", border="red", fill="#00FFEE80", density=25, angle=60)
textbox(c(0.6,0.8), 1, c("keep going",rep("and going",10)), justify='c', cex=0.6,
        leading=1, font=4, border="gold", lty=2, lwd=4, margin=0.025)
textbox(c(0.6,0.8), 0.5, c("keep going",rep("and going",10)), justify='r', cex=0.7,
        col="purple", font=3, border="green", margin=-0.025)
lines(c(0,1), c(1,1), col="red", lty=2)
lines(c(0,1), c(0.5,0.5), col="red", lty=2)











