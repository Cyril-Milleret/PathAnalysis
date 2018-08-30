
# Plot of meta-model in Appendix
rm(list=ls())
setwd("~/First chapter/Path analysis")
pdf(file = "Path_Simple.pdf")

  require(sp) 
  require(plotrix)
  
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
  
  
  
  plot(myStudyArea.poly, xlim=c(-20,70), ylim=c(-30,60), border="white")
  
    #axis(1)
    #axis(2)
  
    # ==== I. DEFINE THE BASIC PATH STRUCTURE ==== # Define position of boxes
    # ---- 1. Treatment/ PRESENCE ---- 
    Treatment=cbind(c(-23,-3,-3,-23), c(16,16,23,23)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
    polygon(Treatment[,1], Treatment[,2], col =  adjustcolor("orange",alpha.f = 0.5), border = "white")
    text(mean(Treatment[,1]),mean(Treatment[,2]), "AGRICULTURAL \n PRACTICE", cex = 0.75)
    
    Pres=cbind(c(48,72,72,48), c(16,16,23,23))
    polygon(Pres[,1], Pres[,2], col =  adjustcolor("blue",alpha.f = 0.5), border = "white")
    text(mean(Pres[,1]),mean(Pres[,2]), "SPECIES \n PRESENCE", cex = 0.75) 
    
    # ---- 2. FOOD ---- 
    Food=cbind(c(12,32,32,12), c(-10,-10,-3,-3))
    polygon(Food[,1], Food[,2], col =  adjustcolor("purple",alpha.f = 0.5), border = "white")
    text(mean(Food[,1]),mean(Food[,2]), "FOOD", cex = 0.75)

    # ---- 3. VEGETATION  ---- 
    Height=cbind(c(12,32,32,12), c(40,40,47,47))  
    polygon(Height[,1], Height[,2], col =  adjustcolor("green",alpha.f = 0.5), border = "white")
    text(mean(Height[,1]),mean(Height[,2]), "VEGETATION", cex = 0.75)
    
    # ---- 4. LANDSCAPE  ---- 
    tbl=cbind(c(48,72,72,48), c(-10,-10,-3,-3))
    polygon(tbl[,1], tbl[,2], col =  adjustcolor("brown",alpha.f = 0.5), border = "white")
    text(mean(tbl[,1]),mean(tbl[,2]), "LANDSCAPE", cex = 0.75)
    
    
    # ==== II. ADD THE ARROW ====  
    
    # ---- 1. Treatment -> Veg/Food ---- 

    x0=mean(Treatment[1:2,1]) #Veg
    x1=mean(Height[1,1])
    y0=mean(Treatment[3,2])
    y1= mean(Height[c(2:3),2])
    
    arrows(x0=x0, x1=x1, y0=y0, y1=y1, length = 0.1, lwd=1, col="grey")
    draw.circle(mean(c(x0, x1)), mean(c(y0, y1)), radius = 3, border = "darkorange", col = "white")
    text(mean(c(x0, x1)) , mean(c(y0, y1)),"1", cex=1, col = "darkorange")
    
    

    x0=mean(Treatment[1:2,1]) #Food
    x1=mean(Food[1,1])
    y0=mean(Treatment[2,2])
    y1=mean(Food[c(2:3),2])
    
    arrows(x0=x0, x1=x1, y0=y0, y1=y1, length = 0.1, lwd=1, col="grey")
    draw.circle(mean(c(x0, x1)), mean(c(y0, y1)), radius = 3, border = "darkorange", col = "white")
    text(mean(c(x0, x1)) , mean(c(y0, y1)),"1", cex=1, col = "darkorange")
    
    

    # ---- 2. Co-lateral -> Food ---- 
    
    x0=mean(tbl[c(1),1]) #Lands
    x1=mean(Food[c(2),1])
    y0=mean(Food[2:3,2])-1
    y1=mean(tbl[2:3,2])-1
    
    arrows(x0=x0, x1=x1, y0=y0, y1=y1, length = 0.1, lwd=1, col="grey")
    draw.circle(mean(c(x0, x1)), mean(c(y0, y1)), radius = 3, border = adjustcolor("purple",alpha.f = 0.5), 
                col = "white")
    text(mean(c(x0, x1)) , mean(c(y0, y1)),"2", cex = 1, col = adjustcolor("purple",alpha.f = 0.5))
   
    
    
    x0=mean(Height[c(1:2),1]) #Veg
    x1=mean(Height[c(1:2),1])
    y0=mean(Height[2,2])
    y1=mean(Food[3,2])
    
    arrows(x0=x0, x1=x1, y0=y0, y1=y1, length = 0.1, lwd=1, col="grey")
    draw.circle(mean(c(x0, x1)), mean(c(y0, y1)), radius = 3, border = adjustcolor("purple",alpha.f = 0.5),
                col = "white")
    text(mean(c(x0, x1)) , mean(c(y0, y1)),"2", cex = 1, col = adjustcolor("purple",alpha.f = 0.5))
  
    
    # ---- 3. Veg/Food/lands -> Presence ---- 
    
    x0=mean(Height[c(2),1]) #Veg
    x1=mean(Pres[c(1:2),1])
    y0=mean(Height[2:3,2])
    y1=mean(Pres[3,2])
    
    arrows(x0=x0, x1=x1, y0=y0, y1=y1, length = 0.1, lwd=1, col="grey")
    draw.circle(mean(c(x0, x1)), mean(c(y0, y1)), radius = 3, border = adjustcolor("blue",alpha.f = 0.5),
                col = "white")
    text(mean(c(x0, x1)) , mean(c(y0, y1)),"3", cex = 1, col = adjustcolor("blue",alpha.f = 0.5))

    
   
    
    x0=mean(Food[c(2),1]) #Food
    x1=mean(Pres[c(1:2),1])-1
    y0=mean(Food[2:3,2])
    y1=mean(Pres[2,2])
    
    arrows(x0=x0, x1=x1, y0=y0, y1=y1, length = 0.1, lwd=1, col="grey")
    draw.circle(mean(c(x0, x1)), mean(c(y0, y1)), radius = 3, border = adjustcolor("blue",alpha.f = 0.5),
                col = "white")
    text(mean(c(x0, x1)) , mean(c(y0, y1)),"3", cex = 1, col = adjustcolor("blue",alpha.f = 0.5))

    

    
    x0=mean(Pres[1:2,1]) #Lands
    x1=mean(Pres[1:2,1])
    y0=mean(tbl[3,2])
    y1=mean(Pres[c(2),2])
    
    arrows(x0=x0, x1=x1, y0=y0, y1=y1, length = 0.1, lwd=1, col="grey")
    draw.circle(mean(c(x0, x1)), mean(c(y0, y1)), radius = 3, border = adjustcolor("blue",alpha.f = 0.5),
                col = "white")
    text(mean(c(x0, x1)) , mean(c(y0, y1)),"3", cex = 1, col = adjustcolor("blue",alpha.f = 0.5))

    


    

    

    dev.off()



    