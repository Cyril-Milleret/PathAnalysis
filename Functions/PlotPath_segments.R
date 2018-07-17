
#No direct arrow; + field area; - LAI (PARA ALCARAVÁN Y CALANDRIA)

PlotPath <- function(e.coefs
                     ,
                     cex.text = 0.7              # text of the of predictors e.g cover SAI, TBL...
                     ,
                     cex.text1 = 0.75            # cex of the treatment and presence
                     ,
                     offset.poly = 2             # offset of the background light box for VEGETATION FOOD LANSCAPE 
                     ,
                     offset.arrow = 2
                     ,
                     arr.length = 0.2
                     ,
                     arr.width = 0.2
                     ,
                     significant = 0.05          # significant value when arrows should be plotted 
                     ,
                     xlim=c(-20,70)              # xlim values for the window 
                     ,
                     ylim=c(-30,60)              # ylim values for the window 
                     ,
                     cex.category = 0.5          # Cex for the category (VEGETATION)
                     ,
                     col.pos="black"               # color of positive arrows 
                     ,
                     col.neg="red"              # color of negative arrows 
                     ,
                     col.non.signifi="grey"      # color of non-significant arrows 
                     ,
                     Treatment.name="TREATMENT"  # Name of the TREATMENT 
                     ,
                     Species.name="SPECIES"      # SPECIES name 
                     , 
                     plot.axis=FALSE             # if axis should be plotted (for developping)
                     ,
                     estimate.box.width=c(3,3)   # width of the white box behind estimates values : first element :x axis and second : on the y axis 
                     ,
                     cex.estimate = 0.7
                     ,
                     digits.estimate = 2
)


{  
  require(sp)
  require(shape)
  
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
  
  if(plot.axis==TRUE){
    axis(1)
    axis(2)
  }
  
  
  # ==== I. DEFINE THE BASIC PATH STRUCTURE ==== # Define position of boxes
  # ---- 1. Treatment/ PRESENCE ---- 
  Treatment=cbind(c(-21,-5,-5,-21), c(16,16,23,23)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
  polygon(Treatment[,1], Treatment[,2], col =  adjustcolor("orange",alpha.f = 0.2), border = "white")
  text(mean(Treatment[,1]),mean(Treatment[,2]), Treatment.name, cex = cex.text1)
  
  Pres=cbind(c(50,70,70,50), c(16,16,23,23))
  polygon(Pres[,1], Pres[,2], col =  adjustcolor("lightblue",alpha.f = 1), border = "white")
  text(mean(Pres[,1]),mean(Pres[,2]), Species.name, cex = cex.text1) 
  
  
  # ---- 2. FOOD ---- 
  SAI_sd=cbind(c(15,29,29,15), c(10,10,14,14))
  biom=cbind(c(15,29,29,15), c(4,4,8,8))
  
  
  food= rbind(SAI_sd,biom)
  min1 <- min(food[,1])-offset.poly
  min2 <- min(food[,2])-offset.poly
  max1 <- max(food[,1])+offset.poly
  max2 <- max(food[,2])+4
  
  
  polygon(c(min1,max1,max1,min1) ,
          c(min2,min2,max2,max2)  ,
          col =  adjustcolor("mediumpurple1",alpha.f = 0.3), border = "white")
  text(mean(food[,1]),max(food[,2])+2 , "FOOD", col=grey(0.3), cex=cex.category)
  
  
  polygon(SAI_sd[,1], SAI_sd[,2], col =  adjustcolor("mediumpurple1",alpha.f = 0.5), border = "white")
  text(mean(SAI_sd[,1]),mean(SAI_sd[,2]), "SAI", cex = cex.text)
  
  
  polygon(biom[,1], biom[,2], col =  adjustcolor("mediumpurple1",alpha.f = 0.5), border = "white")
  text(mean(biom[,1]),mean(biom[,2]), "ORTHOPTERA", cex = cex.text)
  
  # ---- 3. VEGETATION  ---- 
  Diver=cbind(c(15,29,29,15), c(28,28,32,32))
  Heter=cbind(c(15,29,29,15), c(34,34,38,38))
  Height=cbind(c(15,29,29,15), c(40,40,44,44))
  Cover=cbind(c(15,29,29,15), c(46,46,50,50))
  Cover_dead=cbind(c(15,29,29,15), c(52,52,56,56))
  
  veg= rbind(Diver,Heter,Height,Cover,Cover_dead)
  min1 <- min(veg[,1])-offset.poly
  min2 <- min(veg[,2])-offset.poly
  max1 <- max(veg[,1])+offset.poly
  max2 <- max(veg[,2])+4
  
  polygon(c(min1,max1,max1,min1) ,
          c(min2,min2,max2,max2)  ,
          col =  adjustcolor("olivedrab3",alpha.f = 0.3), border = "white")
  text(mean(veg[,1]),max(veg[,2])+2 , "VEGETATION", col=grey(0.3), cex=cex.category)
  
  
  polygon(Diver[,1], Diver[,2], col =  adjustcolor("olivedrab3",alpha.f = 0.5), border = "white")
  text(mean(Diver[,1]),mean(Diver[,2]), "DIVERSITY", cex = cex.text)
  
  polygon(Heter[,1], Heter[,2], col =  adjustcolor("olivedrab3",alpha.f = 0.5), border = "white")
  text(mean(Heter[,1]),mean(Heter[,2]), "HETEROGENEITY", cex = cex.text)
  
  polygon(Height[,1], Height[,2], col =  adjustcolor("olivedrab3",alpha.f = 0.5), border = "white")
  text(mean(Height[,1]),mean(Height[,2]), "HEIGHT", cex = cex.text)
  
  polygon(Cover[,1], Cover[,2], col =  adjustcolor("olivedrab3",alpha.f = 0.5), border = "white")
  text(mean(Cover[,1]),mean(Cover[,2]), "COVER", cex = cex.text)
  
  polygon(Cover_dead[,1], Cover_dead[,2], col =  adjustcolor("olivedrab3",alpha.f = 1), border = "white")
  text(mean(Cover_dead[,1]),mean(Cover_dead[,2]), "COVER_DEAD", cex = cex.text)
  
  # ---- 4. VEGETATION  2 ---- 
  Diver1=cbind(c(-22,-6,-6,-22), c(-27,-27,-23,-23))
  Heter1=cbind(c(-22,-6,-6,-22), c(-21,-21,-17,-17))
  Height1=cbind(c(-22,-6,-6,-22), c(-15,-15,-11,-11))
  Cover1=cbind(c(-22,-6,-6,-22), c(-9,-9,-5,-5))
  Cover_dead1=cbind(c(-22,-6,-6,-22), c(-3,-3,1,1))
  
  veg1= rbind(Diver1,Heter1,Height1,Cover1,Cover_dead1)
  min1 <- min(veg1[,1])-offset.poly
  min2 <- min(veg1[,2])-offset.poly
  max1 <- max(veg1[,1])+offset.poly
  max2 <- max(veg1[,2])+4
  
  
  polygon(c(min1,max1,max1,min1) ,
          c(min2,min2,max2,max2)  ,
          col =  adjustcolor("olivedrab3",alpha.f = 0.3), border = "white")
  text(mean(veg1[,1]),max(veg1[,2])+2 , "VEGETATION", col=grey(0.3), cex=cex.category)
  
  
  polygon(Diver1[,1], Diver1[,2], col =  adjustcolor("olivedrab3",alpha.f = 0.5), border = "white")
  text(mean(Diver1[,1]),mean(Diver1[,2]), "DIVERSITY", cex = cex.text)
  
  polygon(Heter1[,1], Heter1[,2], col =  adjustcolor("olivedrab3",alpha.f = 0.5), border = "white")
  text(mean(Heter1[,1]),mean(Heter1[,2]), "HETEROGENEITY", cex = cex.text)
  
  polygon(Height1[,1], Height1[,2], col =  adjustcolor("olivedrab3",alpha.f = 0.5), border = "white")
  text(mean(Height1[,1]),mean(Height1[,2]), "HEIGHT", cex = cex.text)
  
  polygon(Cover1[,1], Cover1[,2], col =  adjustcolor("olivedrab3",alpha.f = 0.5), border = "white")
  text(mean(Cover1[,1]),mean(Cover1[,2]), "COVER", cex = cex.text)
  
  polygon(Cover_dead1[,1], Cover_dead1[,2], col =  adjustcolor("olivedrab3",alpha.f = 0.5), border = "white")
  text(mean(Cover_dead1[,1]),mean(Cover_dead1[,2]), "COVER_DEAD", cex = cex.text)
  
  
  # ---- 5. LANDSCAPE  ---- 
  tbl=cbind(c(40,54,54,40), c(-4,-4,0,0))
  par=cbind(c(40,54,54,40), c(-10,-10,-6,-6))
  Fallow=cbind(c(40,54,54,40), c(-16,-16,-12,-12))
  crop_diver=cbind(c(40,54,54,40), c(-22,-22,-18,-18))
  
  area=cbind(c(63,73,73,63), c(58,58,62,62))
  
  
  landscape= rbind(tbl,par,Fallow,crop_diver)
  min1 <- min(landscape[,1])-offset.poly
  min2 <- min(landscape[,2])-offset.poly
  max1 <- max(landscape[,1])+offset.poly
  max2 <- max(landscape[,2])+4
  
  
  polygon(c(min1,max1,max1,min1) ,
          c(min2,min2,max2,max2)  ,
          col =  adjustcolor("navajowhite4",alpha.f = 0.3), border = "white")
  
  text(mean(landscape[,1]),max(landscape[,2])+2 , "LANDSCAPE", col=grey(0.3), cex=cex.category)
  
  polygon(tbl[,1], tbl[,2], col =  adjustcolor("navajowhite4",alpha.f = 0.5), border = "white")
  text(mean(tbl[,1]),mean(tbl[,2]), "TBL", cex = cex.text)
  
  
  polygon(par[,1], par[,2], col =  adjustcolor("navajowhite4",alpha.f = 0.5), border = "white")
  text(mean(par[,1]),mean(par[,2]), "PAR", cex = cex.text)
  
  polygon(Fallow[,1], Fallow[,2], col =  adjustcolor("navajowhite4",alpha.f = 0.5), border = "white")
  text(mean(Fallow[,1]),mean(Fallow[,2]), "FALLOW", cex = cex.text)
  
  polygon(crop_diver[,1], crop_diver[,2], col =  adjustcolor("navajowhite4",alpha.f = 0.5), border = "white")
  text(mean(crop_diver[,1]),mean(crop_diver[,2]), "CROP DIVERSITY", cex = cex.text)
  
  polygon(area[,1], area[,2], col =  adjustcolor("yellow",alpha.f = 0.5), border = "white")
  text(mean(area[,1]),mean(area[,2]), "FIELD AREA", cex = cex.text)
  
  
  # ==== II. ADD THE ARROW ====  
  
  # -----1. Treatment -> Vegetation 1 -------
 
  
  coeffs <- e.coefs[e.coefs$predictor== "Treatment" & e.coefs$response=="Cover_dead"  ,]
  
  if((coeffs$p.value > 0.10)){
    arrows(x0=(Treatment[c(1),1] + 4), x1=mean(Cover_dead[1,1]), y0=mean(Cover_dead[3:2,2]), y1= mean(Cover_dead[3:2,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0=mean(Treatment[c(1),1] + 4), x1=mean(Treatment[c(1),1] + 4), y0=Treatment[c(3),2], y1= mean(Cover_dead[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25))) }
  
  if((0.05 < coeffs$p.value) & (coeffs$p.value < 0.10)){
    arrows(x0=(Treatment[c(1),1] + 4), x1=mean(Cover_dead[1,1]), y0=mean(Cover_dead[3:2,2]), y1= mean(Cover_dead[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    segments(x0=mean(Treatment[c(1),1] + 4), x1=mean(Treatment[c(1),1] + 4), y0=Treatment[3,2], y1= mean(Cover_dead[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    text(mean(c(mean(Treatment[1,1] + 4),Cover_dead[1,1]))  , mean(Cover_dead[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }


  if((0.01 < coeffs$p.value) & (coeffs$p.value < 0.05)){
    arrows(x0=(Treatment[c(1),1] + 4), x1=mean(Cover_dead[1,1]), y0=mean(Cover_dead[3:2,2]), y1= mean(Cover_dead[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    segments(x0=mean(Treatment[c(1),1] + 4), x1=mean(Treatment[c(1),1] + 4), y0=Treatment[3,2], y1= mean(Cover_dead[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    text(mean(c(mean(Treatment[1,1] + 4),Cover_dead[1,1]))  , mean(Cover_dead[3:2,2]) + offset.arrow, labels = paste(round(coeffs$estimate, digits = digits.estimate), "*"), cex=cex.estimate) }
  
  if(coeffs$p.value < 0.01){
    arrows(x0=(Treatment[c(1),1] + 4), x1=mean(Cover_dead[1,1]), y0=mean(Cover_dead[3:2,2]), y1= mean(Cover_dead[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    segments(x0=mean(Treatment[c(1),1] + 4), x1=mean(Treatment[c(1),1] + 4), y0=Treatment[3,2], y1= mean(Cover_dead[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    text(mean(c(mean(Treatment[1,1] + 4),Cover_dead[1,1]))  , mean(Cover_dead[3:2,2]) + offset.arrow, labels = paste(round(coeffs$estimate, digits = digits.estimate), "**"), cex=cex.estimate) }
  
  
  
  coeffs <- e.coefs[e.coefs$predictor== "Treatment" & e.coefs$response=="Cover"  ,]
  
  if((coeffs$p.value > 0.10)){
    arrows(x0=(Treatment[c(1),1] + 6), x1=mean(Cover[1,1]), y0=mean(Cover[3:2,2]), y1= mean(Cover[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0=mean(Treatment[c(1),1] + 6), x1=mean(Treatment[c(1),1] + 6), y0=Treatment[c(3),2], y1= mean(Cover[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25))) }
  
  if((0.05 < coeffs$p.value) & (coeffs$p.value < 0.10)){
    arrows(x0=(Treatment[c(1),1] + 6), x1=mean(Cover[1,1]), y0=mean(Cover[3:2,2]), y1= mean(Cover[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    segments(x0=mean(Treatment[c(1),1] + 6), x1=mean(Treatment[c(1),1] + 6), y0=Treatment[3,2], y1= mean(Cover[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    text(mean(c(mean(Treatment[1,1] + 6),Cover[1,1]))  , mean(Cover[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  if((0.01 < coeffs$p.value) & (coeffs$p.value < 0.05)){
    arrows(x0=(Treatment[c(1),1] + 6), x1=mean(Cover[1,1]), y0=mean(Cover[3:2,2]), y1= mean(Cover[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    segments(x0=mean(Treatment[c(1),1] + 6), x1=mean(Treatment[c(1),1] + 6), y0=Treatment[3,2], y1= mean(Cover[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    text(mean(c(mean(Treatment[1,1] + 6),Cover[1,1]))  , mean(Cover[3:2,2]) + offset.arrow, labels = paste(round(coeffs$estimate, digits = digits.estimate), "*"), cex=cex.estimate) }
  
  if(coeffs$p.value < 0.01){
    arrows(x0=(Treatment[c(1),1] + 6), x1=mean(Cover[1,1]), y0=mean(Cover[3:2,2]), y1= mean(Cover[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    segments(x0=mean(Treatment[c(1),1] + 6), x1=mean(Treatment[c(1),1] + 6), y0=Treatment[3,2], y1= mean(Cover[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    text(mean(c(mean(Treatment[1,1] + 6),Cover[1,1]))  , mean(Cover[3:2,2]) + offset.arrow, labels = paste(round(coeffs$estimate, digits = digits.estimate), "**"), cex=cex.estimate) }

  
  coeffs <- e.coefs[e.coefs$predictor== "Treatment" & e.coefs$response=="Height"  ,]
  
  if((coeffs$p.value > 0.10)){
    arrows(x0=(Treatment[c(1),1] + 8), x1=mean(Height[1,1]), y0=mean(Height[3:2,2]), y1= mean(Height[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0=mean(Treatment[c(1),1] + 8), x1=mean(Treatment[c(1),1] + 8), y0=Treatment[c(3),2], y1= mean(Height[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25))) }
  
  if((0.05 < coeffs$p.value) & (coeffs$p.value < 0.10)){
    arrows(x0=(Treatment[c(1),1] + 8), x1=mean(Height[1,1]), y0=mean(Height[3:2,2]), y1= mean(Height[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    segments(x0=mean(Treatment[c(1),1] + 8), x1=mean(Treatment[c(1),1] + 8), y0=Treatment[3,2], y1= mean(Height[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    text(mean(c(mean(Treatment[1,1] + 8),Height[1,1]))  , mean(Height[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  if((0.01 < coeffs$p.value) & (coeffs$p.value < 0.05)){
    arrows(x0=(Treatment[c(1),1] + 8), x1=mean(Height[1,1]), y0=mean(Height[3:2,2]), y1= mean(Height[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    segments(x0=mean(Treatment[c(1),1] + 8), x1=mean(Treatment[c(1),1] + 8), y0=Treatment[3,2], y1= mean(Height[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    text(mean(c(mean(Treatment[1,1] + 8),Height[1,1]))  , mean(Height[3:2,2]) + offset.arrow, labels = paste(round(coeffs$estimate, digits = digits.estimate), "*"), cex=cex.estimate) }
  
  if(coeffs$p.value < 0.01){
    arrows(x0=(Treatment[c(1),1] + 8), x1=mean(Height[1,1]), y0=mean(Height[3:2,2]), y1= mean(Height[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    segments(x0=mean(Treatment[c(1),1] + 8), x1=mean(Treatment[c(1),1] + 8), y0=Treatment[3,2], y1= mean(Height[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    text(mean(c(mean(Treatment[1,1] + 8),Height[1,1]))  , mean(Height[3:2,2]) + offset.arrow, labels = paste(round(coeffs$estimate, digits = digits.estimate), "**"), cex=cex.estimate) }
  
  
  coeffs <- e.coefs[e.coefs$predictor== "Treatment" & e.coefs$response=="Heter"  ,]
  
  if((coeffs$p.value > 0.10)){
    arrows(x0=(Treatment[c(1),1] + 10), x1=mean(Heter[1,1]), y0=mean(Heter[3:2,2]), y1= mean(Heter[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0=mean(Treatment[c(1),1] + 10), x1=mean(Treatment[c(1),1] + 10), y0=Treatment[3,2], y1= mean(Heter[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25))) }
  
  if((0.05 < coeffs$p.value) & (coeffs$p.value < 0.10)){
    arrows(x0=(Treatment[c(1),1] + 10), x1=mean(Heter[1,1]), y0=mean(Heter[3:2,2]), y1= mean(Heter[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    segments(x0=mean(Treatment[c(1),1] + 10), x1=mean(Treatment[c(1),1] + 10), y0=Treatment[3,2], y1= mean(Heter[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    text(mean(c(mean(Treatment[1,1] + 10),Heter[1,1]))  , mean(Heter[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  if((0.01 < coeffs$p.value) & (coeffs$p.value < 0.05)){
    arrows(x0=(Treatment[c(1),1] + 10), x1=mean(Heter[1,1]), y0=mean(Heter[3:2,2]), y1= mean(Heter[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    segments(x0=mean(Treatment[c(1),1] + 10), x1=mean(Treatment[c(1),1] + 10), y0=Treatment[3,2], y1= mean(Heter[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    text(mean(c(mean(Treatment[1,1] + 10),Heter[1,1]))  , mean(Heter[3:2,2]) + offset.arrow, labels = paste(round(coeffs$estimate, digits = digits.estimate), "*"), cex=cex.estimate) }
  
  if(coeffs$p.value < 0.01){
    arrows(x0=(Treatment[c(1),1] + 10), x1=mean(Heter[1,1]), y0=mean(Heter[3:2,2]), y1= mean(Heter[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    segments(x0=mean(Treatment[c(1),1] + 10), x1=mean(Treatment[c(1),1] + 10), y0=Treatment[3,2], y1= mean(Heter[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    text(mean(c(mean(Treatment[1,1] + 10),Heter[1,1]))  , mean(Heter[3:2,2]) + offset.arrow, labels = paste(round(coeffs$estimate, digits = digits.estimate), "**"), cex=cex.estimate) }
  
  
  
  coeffs <- e.coefs[e.coefs$predictor== "Treatment" & e.coefs$response=="Diver"  ,]
  
  if((coeffs$p.value > 0.10)){
    arrows(x0=(Treatment[c(1),1] + 12), x1=mean(Diver[1,1]), y0=mean(Diver[3:2,2]), y1= mean(Diver[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0=mean(Treatment[c(1),1] + 12), x1=mean(Treatment[c(1),1] + 12), y0=Treatment[3,2], y1= mean(Diver[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25))) }
  
  if((0.05 < coeffs$p.value) & (coeffs$p.value < 0.10)){
    arrows(x0=(Treatment[c(1),1] + 12), x1=mean(Diver[1,1]), y0=mean(Diver[3:2,2]), y1= mean(Diver[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    segments(x0=mean(Treatment[c(1),1] + 12), x1=mean(Treatment[c(1),1] + 12), y0=Treatment[3,2], y1= mean(Diver[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    text(mean(c(mean(Treatment[1,1] + 12),Diver[1,1]))  , mean(Diver[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  if((0.01 < coeffs$p.value) & (coeffs$p.value < 0.05)){
    arrows(x0=(Treatment[c(1),1] + 12), x1=mean(Diver[1,1]), y0=mean(Diver[3:2,2]), y1= mean(Diver[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    segments(x0=mean(Treatment[c(1),1] + 12), x1=mean(Treatment[c(1),1] + 12), y0=Treatment[3,2], y1= mean(Diver[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    text(mean(c(mean(Treatment[1,1] + 12),Diver[1,1]))  , mean(Diver[3:2,2]) + offset.arrow, labels = paste(round(coeffs$estimate, digits = digits.estimate), "*"), cex=cex.estimate) }
  
  if(coeffs$p.value < 0.01){
    arrows(x0=(Treatment[c(1),1] + 12), x1=mean(Diver[1,1]), y0=mean(Diver[3:2,2]), y1= mean(Diver[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    segments(x0=mean(Treatment[c(1),1] + 12), x1=mean(Treatment[c(1),1] + 12), y0=Treatment[3,2], y1= mean(Diver[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    text(mean(c(mean(Treatment[1,1] + 12),Diver[1,1]))  , mean(Diver[3:2,2]) + offset.arrow, labels = paste(round(coeffs$estimate, digits = digits.estimate), "**"), cex=cex.estimate) }
  
  
  # -----2. Treatment -> Food -------
  

  coeffs <- e.coefs[e.coefs$predictor== "Treatment" & e.coefs$response=="SAI_sd"  ,]
  
  if((coeffs$p.value > 0.10)){
    arrows(x0=(Treatment[c(1),1] + 8), x1=mean(SAI_sd[1,1]), y0=mean(SAI_sd[3:2,2]), y1= mean(SAI_sd[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0=mean(Treatment[c(1),1] + 8), x1=mean(Treatment[c(1),1] + 8), y0=Treatment[c(2),2], y1= mean(SAI_sd[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25))) }
  
  if((0.05 < coeffs$p.value) & (coeffs$p.value < 0.10)){
    arrows(x0=(Treatment[c(1),1] + 8), x1=mean(SAI_sd[1,1]), y0=mean(SAI_sd[3:2,2]), y1= mean(SAI_sd[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    segments(x0=mean(Treatment[c(1),1] + 8), x1=mean(Treatment[c(1),1] + 8), y0=Treatment[2,2], y1= mean(SAI_sd[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    text(mean(c(mean(Treatment[1,1] + 8),SAI_sd[1,1]))  , mean(SAI_sd[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  if((0.01 < coeffs$p.value) & (coeffs$p.value < 0.05)){
    arrows(x0=(Treatment[c(1),1] + 8), x1=mean(SAI_sd[1,1]), y0=mean(SAI_sd[3:2,2]), y1= mean(SAI_sd[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    segments(x0=mean(Treatment[c(1),1] + 8), x1=mean(Treatment[c(1),1] + 8), y0=Treatment[2,2], y1= mean(SAI_sd[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    text(mean(c(mean(Treatment[1,1] + 8),SAI_sd[1,1]))  , mean(SAI_sd[3:2,2]) + offset.arrow, labels = paste(round(coeffs$estimate, digits = digits.estimate), "*"), cex=cex.estimate) }
  
  if(coeffs$p.value < 0.01){
    arrows(x0=(Treatment[c(1),1] + 8), x1=mean(SAI_sd[1,1]), y0=mean(SAI_sd[3:2,2]), y1= mean(SAI_sd[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    segments(x0=mean(Treatment[c(1),1] + 8), x1=mean(Treatment[c(1),1] + 8), y0=Treatment[2,2], y1= mean(SAI_sd[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    text(mean(c(mean(Treatment[1,1] + 8),SAI_sd[1,1]))  , mean(SAI_sd[3:2,2]) + offset.arrow, labels = paste(round(coeffs$estimate, digits = digits.estimate), "**"), cex=cex.estimate) }
  
  
  coeffs <- e.coefs[e.coefs$predictor== "Treatment" & e.coefs$response=="biom"  ,]
  
  if((coeffs$p.value > 0.10)){
    arrows(x0=(Treatment[c(1),1] + 6), x1=mean(biom[1,1]), y0=mean(biom[3:2,2]), y1= mean(biom[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0=mean(Treatment[c(1),1] + 6), x1=mean(Treatment[c(1),1] + 6), y0=Treatment[c(2),2], y1= mean(biom[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25))) }
  
  if((0.05 < coeffs$p.value) & (coeffs$p.value < 0.10)){
    arrows(x0=(Treatment[c(1),1] + 6), x1=mean(biom[1,1]), y0=mean(biom[3:2,2]), y1= mean(biom[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    segments(x0=mean(Treatment[c(1),1] + 6), x1=mean(Treatment[c(1),1] + 6), y0=Treatment[2,2], y1= mean(biom[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    text(mean(c(mean(Treatment[1,1] + 6),biom[1,1]))  , mean(biom[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  if((0.01 < coeffs$p.value) & (coeffs$p.value < 0.05)){
    arrows(x0=(Treatment[c(1),1] + 6), x1=mean(biom[1,1]), y0=mean(biom[3:2,2]), y1= mean(biom[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    segments(x0=mean(Treatment[c(1),1] + 6), x1=mean(Treatment[c(1),1] + 6), y0=Treatment[2,2], y1= mean(biom[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    text(mean(c(mean(Treatment[1,1] + 6),biom[1,1]))  , mean(biom[3:2,2]) + offset.arrow, labels = paste(round(coeffs$estimate, digits = digits.estimate), "*"), cex=cex.estimate) }
  
  if(coeffs$p.value < 0.01){
    arrows(x0=(Treatment[c(1),1] + 6), x1=mean(biom[1,1]), y0=mean(biom[3:2,2]), y1= mean(biom[3:2,2]), length = 0.1,
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    segments(x0=mean(Treatment[c(1),1] + 6), x1=mean(Treatment[c(1),1] + 6), y0=Treatment[2,2], y1= mean(biom[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    text(mean(c(mean(Treatment[1,1] + 6),biom[1,1]))  , mean(biom[3:2,2]) + offset.arrow, labels = paste(round(coeffs$estimate, digits = digits.estimate), "**"), cex=cex.estimate) }
  
  
  # -----3. Vegetation -> Species -------
  
  
  coeffs <- e.coefs[e.coefs$predictor== "Diver" & e.coefs$response=="Pres"  ,]
  
  if((coeffs$p.value > 0.10)){
    arrows(x0=mean(Pres[1,1]) + 3, x1=mean(Pres[1,1]) + 3, y0=mean(Diver[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Diver[2,1], x1=mean(Pres[1,1]) + 3, y0=mean(Diver[2:3,2]), y1= mean(Diver[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25))) }
  
  if((0.05 < coeffs$p.value) & (coeffs$p.value < 0.10)){
    arrows(x0=mean(Pres[1,1]) + 3, x1=mean(Pres[1,1]) + 3, y0=mean(Diver[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Diver[2,1], x1=mean(Pres[1,1]) + 3, y0=mean(Diver[2:3,2]), y1= mean(Diver[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    text(mean(c(mean(Diver[2,1]),mean(Pres[1,1]) + 3))  , mean(Diver[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  if((0.01 < coeffs$p.value) & (coeffs$p.value < 0.05)){
    arrows(x0=mean(Pres[1,1]) + 3, x1=mean(Pres[1,1]) + 3, y0=mean(Diver[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Diver[2,1], x1=mean(Pres[1,1]) + 3, y0=mean(Diver[2:3,2]), y1= mean(Diver[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    text(mean(c(mean(Diver[2,1]),mean(Pres[1,1]) + 3))  , mean(Diver[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  if(coeffs$p.value < 0.01){
    arrows(x0=mean(Pres[1,1]) + 3, x1=mean(Pres[1,1]) + 3, y0=mean(Diver[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Diver[2,1], x1=mean(Pres[1,1]) + 3, y0=mean(Diver[2:3,2]), y1= mean(Diver[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    text(mean(c(mean(Diver[2,1]),mean(Pres[1,1]) + 3))  , mean(Diver[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  coeffs <- e.coefs[e.coefs$predictor== "Heter" & e.coefs$response=="Pres"  ,]
  
  if((coeffs$p.value > 0.10)){
    arrows(x0=mean(Pres[1,1]) + 6, x1=mean(Pres[1,1]) + 6, y0=mean(Heter[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Heter[2,1], x1=mean(Pres[1,1]) + 6, y0=mean(Heter[2:3,2]), y1= mean(Heter[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25))) }
  
  if((0.05 < coeffs$p.value) & (coeffs$p.value < 0.10)){
    arrows(x0=mean(Pres[1,1]) + 6, x1=mean(Pres[1,1]) + 6, y0=mean(Heter[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Heter[2,1], x1=mean(Pres[1,1]) + 6, y0=mean(Heter[2:3,2]), y1= mean(Heter[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    text(mean(c(mean(Heter[2,1]),mean(Pres[1,1]) + 6))  , mean(Heter[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  if((0.01 < coeffs$p.value) & (coeffs$p.value < 0.05)){
    arrows(x0=mean(Pres[1,1]) + 6, x1=mean(Pres[1,1]) + 6, y0=mean(Heter[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Heter[2,1], x1=mean(Pres[1,1]) + 6, y0=mean(Heter[2:3,2]), y1= mean(Heter[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    text(mean(c(mean(Heter[2,1]),mean(Pres[1,1]) + 6))  , mean(Heter[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  if(coeffs$p.value < 0.01){
    arrows(x0=mean(Pres[1,1]) + 6, x1=mean(Pres[1,1]) + 6, y0=mean(Heter[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Heter[2,1], x1=mean(Pres[1,1]) + 6, y0=mean(Heter[2:3,2]), y1= mean(Heter[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    text(mean(c(mean(Heter[2,1]),mean(Pres[1,1]) + 6))  , mean(Heter[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  coeffs <- e.coefs[e.coefs$predictor== "Height" & e.coefs$response=="Pres"  ,]
  
  if((coeffs$p.value > 0.10)){
    arrows(x0=mean(Pres[1,1]) + 9, x1=mean(Pres[1,1]) + 9, y0=mean(Height[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Height[2,1], x1=mean(Pres[1,1]) + 9, y0=mean(Height[2:3,2]), y1= mean(Height[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25))) }
  
  if((0.05 < coeffs$p.value) & (coeffs$p.value < 0.10)){
    arrows(x0=mean(Pres[1,1]) + 9, x1=mean(Pres[1,1]) + 9, y0=mean(Height[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Height[2,1], x1=mean(Pres[1,1]) + 9, y0=mean(Height[2:3,2]), y1= mean(Height[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    text(mean(c(mean(Height[2,1]),mean(Pres[1,1]) + 9))  , mean(Height[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  if((0.01 < coeffs$p.value) & (coeffs$p.value < 0.05)){
    arrows(x0=mean(Pres[1,1]) + 9, x1=mean(Pres[1,1]) + 9, y0=mean(Height[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Height[2,1], x1=mean(Pres[1,1]) + 9, y0=mean(Height[2:3,2]), y1= mean(Height[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    text(mean(c(mean(Height[2,1]),mean(Pres[1,1]) + 9))  , mean(Height[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  if(coeffs$p.value < 0.01){
    arrows(x0=mean(Pres[1,1]) + 9, x1=mean(Pres[1,1]) + 9, y0=mean(Height[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Height[2,1], x1=mean(Pres[1,1]) + 9, y0=mean(Height[2:3,2]), y1= mean(Height[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    text(mean(c(mean(Height[2,1]),mean(Pres[1,1]) + 9))  , mean(Height[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  coeffs <- e.coefs[e.coefs$predictor== "Cover" & e.coefs$response=="Pres"  ,]
  
  if((coeffs$p.value > 0.10)){
    arrows(x0=mean(Pres[1,1]) + 12, x1=mean(Pres[1,1]) + 12, y0=mean(Cover[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Cover[2,1], x1=mean(Pres[1,1]) + 12, y0=mean(Cover[2:3,2]), y1= mean(Cover[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25))) }
  
  if((0.05 < coeffs$p.value) & (coeffs$p.value < 0.10)){
    arrows(x0=mean(Pres[1,1]) + 12, x1=mean(Pres[1,1]) + 12, y0=mean(Cover[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Cover[2,1], x1=mean(Pres[1,1]) + 12, y0=mean(Cover[2:3,2]), y1= mean(Cover[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    text(mean(c(mean(Cover[2,1]),mean(Pres[1,1]) + 12))  , mean(Cover[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  if((0.01 < coeffs$p.value) & (coeffs$p.value < 0.05)){
    arrows(x0=mean(Pres[1,1]) + 12, x1=mean(Pres[1,1]) + 12, y0=mean(Cover[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Cover[2,1], x1=mean(Pres[1,1]) + 12, y0=mean(Cover[2:3,2]), y1= mean(Cover[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    text(mean(c(mean(Cover[2,1]),mean(Pres[1,1]) + 12))  , mean(Cover[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  if(coeffs$p.value < 0.01){
    arrows(x0=mean(Pres[1,1]) + 12, x1=mean(Pres[1,1]) + 12, y0=mean(Cover[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Cover[2,1], x1=mean(Pres[1,1]) + 12, y0=mean(Cover[2:3,2]), y1= mean(Cover[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    text(mean(c(mean(Cover[2,1]),mean(Pres[1,1]) + 12))  , mean(Cover[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  coeffs <- e.coefs[e.coefs$predictor== "Cover_dead" & e.coefs$response=="Pres"  ,]
  
  if((coeffs$p.value > 0.10)){
    arrows(x0=mean(Pres[1,1]) + 15, x1=mean(Pres[1,1]) + 15, y0=mean(Cover_dead[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Cover_dead[2,1], x1=mean(Pres[1,1]) + 15, y0=mean(Cover_dead[2:3,2]), y1= mean(Cover_dead[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25))) }
  
  if((0.05 < coeffs$p.value) & (coeffs$p.value < 0.10)){
    arrows(x0=mean(Pres[1,1]) + 15, x1=mean(Pres[1,1]) + 15, y0=mean(Cover_dead[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Cover_dead[2,1], x1=mean(Pres[1,1]) + 15, y0=mean(Cover_dead[2:3,2]), y1= mean(Cover_dead[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    text(mean(c(mean(Cover_dead[2,1]),mean(Pres[1,1]) + 15))  , mean(Cover_dead[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  if((0.01 < coeffs$p.value) & (coeffs$p.value < 0.05)){
    arrows(x0=mean(Pres[1,1]) + 15, x1=mean(Pres[1,1]) + 15, y0=mean(Cover_dead[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Cover_dead[2,1], x1=mean(Pres[1,1]) + 15, y0=mean(Cover_dead[2:3,2]), y1= mean(Cover_dead[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    text(mean(c(mean(Cover_dead[2,1]),mean(Pres[1,1]) + 15))  , mean(Cover_dead[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  if(coeffs$p.value < 0.01){
    arrows(x0=mean(Pres[1,1]) + 15, x1=mean(Pres[1,1]) + 15, y0=mean(Cover_dead[2:3,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0= Cover_dead[2,1], x1=mean(Pres[1,1]) + 15, y0=mean(Cover_dead[2:3,2]), y1= mean(Cover_dead[2:3,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    text(mean(c(mean(Cover_dead[2,1]),mean(Pres[1,1]) + 15))  , mean(Cover_dead[3:2,2]) + offset.arrow, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  # ----- 4. Area -> Species -------
  
  coeffs <- e.coefs[e.coefs$predictor== "area" & e.coefs$response=="Pres"  ,]
  
  if((coeffs$p.value > 0.10)){
    arrows(x0=mean(Pres[1,1]) + 18, x1=mean(Pres[1,1]) + 18, y0=mean(area[2,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25))) }
  
  if((0.05 < coeffs$p.value) & (coeffs$p.value < 0.10)){
    arrows(x0=mean(Pres[1,1]) + 18, x1=mean(Pres[1,1]) + 18, y0=mean(area[2,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    text( mean(Pres[1,1]) + 18 + offset.arrow, mean(c(area[3,2], Pres[3,2])), round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  if((0.01 < coeffs$p.value) & (coeffs$p.value < 0.05)){
    arrows(x0=mean(Pres[1,1]) + 18, x1=mean(Pres[1,1]) + 18, y0=mean(area[2,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    text( mean(Pres[1,1]) + 18 + offset.arrow, mean(c(area[3,2], Pres[3,2])), round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  if(coeffs$p.value < 0.01){
    arrows(x0=mean(Pres[1,1]) + 18, x1=mean(Pres[1,1]) + 18, y0=mean(area[2,2]), y1= mean(Pres[3,2]),length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)),
           arr.type = "triangle", arr.length = arr.length, arr.width = arr.width, arr.adj = 1, arr.lwd = (abs(coeffs$estimate))*3, arr.col = ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    text( mean(Pres[1,1]) + 18 + offset.arrow, mean(c(area[3,2], Pres[3,2])), round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #NOW, ADD LANDSCAPE ARROWS
  coeffs <- e.coefs[e.coefs$predictor== "crop_diver" & e.coefs$response=="Pres"  ,]
  
  if((coeffs$p.value > 0.10)){
    arrows(x0=mean(Pres[1:2,1])+6, x1=mean(Pres[1:2,1])+6, y0=mean(crop_diver[3:2,2]), y1= mean(Pres[c(2),2]), length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0=mean(crop_diver[2,1]), x1=mean(Pres[1:2,1])+6, y0=mean(crop_diver[3:2,2]), y1= mean(crop_diver[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25))) }
  
  if((0.05 < coeffs$p.value) & (coeffs$p.value < 0.10)){
    arrows(x0=mean(Pres[1:2,1])+6, x1=mean(Pres[1:2,1])+6, y0=mean(crop_diver[3:2,2]), y1= mean(Pres[c(2),2]), length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    segments(x0=mean(crop_diver[2,1]), x1=mean(Pres[1:2,1])+6, y0=mean(crop_diver[3:2,2]), y1= mean(crop_diver[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    text(mean(c(mean(crop_diver[2,1]),mean(Pres[1:2,1])+6))  , mean(crop_diver[3:2,2])+1.2, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  if((0.01 < coeffs$p.value) & (coeffs$p.value < 0.05)){
    arrows(x0=mean(Pres[1:2,1])+6, x1=mean(Pres[1:2,1])+6, y0=mean(crop_diver[3:2,2]), y1= mean(Pres[c(2),2]), length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    segments(x0=mean(crop_diver[2,1]), x1=mean(Pres[1:2,1])+6, y0=mean(crop_diver[3:2,2]), y1= mean(crop_diver[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    text(mean(c(mean(crop_diver[2,1]),mean(Pres[1:2,1])+6))  , mean(crop_diver[3:2,2])+1.2, labels = paste(round(coeffs$estimate, digits = digits.estimate), "*"), cex=cex.estimate) }
  
  if(coeffs$p.value < 0.01){
    arrows(x0=mean(Pres[1:2,1])+6, x1=mean(Pres[1:2,1])+6, y0=mean(crop_diver[3:2,2]), y1= mean(Pres[c(2),2]), length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    segments(x0=mean(crop_diver[2,1]), x1=mean(Pres[1:2,1])+6, y0=mean(crop_diver[3:2,2]), y1= mean(crop_diver[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    text(mean(c(mean(crop_diver[2,1]),mean(Pres[1:2,1])+6))  , mean(crop_diver[3:2,2])+1.2, labels = paste(round(coeffs$estimate, digits = digits.estimate), "**"), cex=cex.estimate) }
  
  
  
  coeffs <- e.coefs[e.coefs$predictor== "Fallow" & e.coefs$response=="Pres"  ,]
  
  if (coeffs$p.value > 0.10){
    arrows(x0=mean(Pres[1:2,1])+4, x1=mean(Pres[1:2,1])+4, y0=mean(Fallow[3:2,2]), y1= mean(Pres[c(2),2]), length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0=mean(Fallow[2,1]), x1=mean(Pres[1:2,1])+4, y0=mean(Fallow[3:2,2]), y1= mean(Fallow[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25))) }
  
  if ((0.05 < coeffs$p.value) & (coeffs$p.value < 0.10)){
    arrows(x0=mean(Pres[1:2,1])+4, x1=mean(Pres[1:2,1])+4, y0=mean(Fallow[3:2,2]), y1= mean(Pres[c(2),2]), length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    segments(x0=mean(Fallow[2,1]), x1=mean(Pres[1:2,1])+4, y0=mean(Fallow[3:2,2]), y1= mean(Fallow[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    text(mean(c(mean(Fallow[2,1]),mean(Pres[1:2,1])+4))  , mean(Fallow[3:2,2])+1.2, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate) }
  
  if ((0.01 < coeffs$p.value) & (coeffs$p.value < 0.05)){
    arrows(x0=mean(Pres[1:2,1])+4, x1=mean(Pres[1:2,1])+4, y0=mean(Fallow[3:2,2]), y1= mean(Pres[c(2),2]), length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    segments(x0=mean(Fallow[2,1]), x1=mean(Pres[1:2,1])+4, y0=mean(Fallow[3:2,2]), y1= mean(Fallow[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    text(mean(c(mean(Fallow[2,1]),mean(Pres[1:2,1])+4))  , mean(Fallow[3:2,2])+1.2, labels = paste(round(coeffs$estimate, digits = digits.estimate), "*"), cex=cex.estimate) }
  
  if (coeffs$p.value < 0.01){
    arrows(x0=mean(Pres[1:2,1])+4, x1=mean(Pres[1:2,1])+4, y0=mean(Fallow[3:2,2]), y1= mean(Pres[c(2),2]), length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    segments(x0=mean(Fallow[2,1]), x1=mean(Pres[1:2,1])+4, y0=mean(Fallow[3:2,2]), y1= mean(Fallow[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    text(mean(c(mean(Fallow[2,1]),mean(Pres[1:2,1])+4))  , mean(Fallow[3:2,2])+1.2, labels = paste(round(coeffs$estimate, digits = digits.estimate), "**"), cex=cex.estimate) }
  
  
  
  coeffs <- e.coefs[e.coefs$predictor== "par" & e.coefs$response=="Pres"  ,]
  
  if(coeffs$p.value > 0.10){
    arrows(x0=mean(Pres[1:2,1])+1, x1=mean(Pres[1:2,1])+1, y0=mean(par[3:2,2]), y1= mean(Pres[c(2),2]), length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0=mean(par[2,1]), x1=mean(Pres[1:2,1])+1, y0=mean(par[3:2,2]), y1= mean(par[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25))) }
  
  if ((0.05 < coeffs$p.value) & (coeffs$p.value < 0.10)){
    arrows(x0=mean(Pres[1:2,1])+1, x1=mean(Pres[1:2,1])+1, y0=mean(par[3:2,2]), y1= mean(Pres[c(2),2]), length = 0.1, 
           lwd= (abs(coeffs$estimate))*3,
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    segments(x0=mean(par[2,1]), x1=mean(Pres[1:2,1])+1, y0=mean(par[3:2,2]), y1= mean(par[3:2,2]),
             lwd= (abs(coeffs$estimate))*3,
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    text(mean(c(mean(par[2,1]),mean(Pres[1:2,1])+1))  , mean(par[3:2,2])+1.2, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate)}
  
  if ((0.01 < coeffs$p.value) & (coeffs$p.value < 0.05)){
    arrows(x0=mean(Pres[1:2,1])+1, x1=mean(Pres[1:2,1])+1, y0=mean(par[3:2,2]), y1= mean(Pres[c(2),2]), length = 0.1, 
           lwd= (abs(coeffs$estimate))*3,
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    segments(x0=mean(par[2,1]), x1=mean(Pres[1:2,1])+1, y0=mean(par[3:2,2]), y1= mean(par[3:2,2]),
             lwd= (abs(coeffs$estimate))*3,
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    text(mean(c(mean(Fallow[2,1]),mean(Pres[1:2,1])+4))  , mean(Fallow[3:2,2])+1.2, labels = paste(round(coeffs$estimate, digits = digits.estimate), "*"), cex=cex.estimate) }
  
  if (coeffs$p.value < 0.01){
    arrows(x0=mean(Pres[1:2,1])+1, x1=mean(Pres[1:2,1])+1, y0=mean(par[3:2,2]), y1= mean(Pres[c(2),2]), length = 0.1, 
           lwd= (abs(coeffs$estimate))*3,
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    segments(x0=mean(par[2,1]), x1=mean(Pres[1:2,1])+1, y0=mean(par[3:2,2]), y1= mean(par[3:2,2]),
             lwd= (abs(coeffs$estimate))*3,
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    text(mean(c(mean(Fallow[2,1]),mean(Pres[1:2,1])+4))  , mean(Fallow[3:2,2])+1.2, labels = paste(round(coeffs$estimate, digits = digits.estimate), "**"), cex=cex.estimate) }
  
  
  coeffs <- e.coefs[e.coefs$predictor== "tbl" & e.coefs$response=="Pres"  ,]
  
  if(coeffs$p.value > 0.10){
    arrows(x0=mean(Pres[1:2,1])-1, x1=mean(Pres[1:2,1])-1, y0=mean(tbl[3:2,2]), y1= mean(Pres[c(2),2]), length = 0.1, 
           lwd= (abs(coeffs$estimate))*3, 
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))
    segments(x0=mean(tbl[2,1]), x1=mean(Pres[1:2,1])-1, y0=mean(tbl[3:2,2]), y1= mean(tbl[3:2,2]),
             lwd= (abs(coeffs$estimate))*3, 
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.25), adjustcolor(col.neg,alpha.f = 0.25)))}
  
  if ((0.05 < coeffs$p.value) & (coeffs$p.value < 0.10)){
    arrows(x0=mean(Pres[1:2,1])+1, x1=mean(Pres[1:2,1])-1, y0=mean(tbl[3:2,2]), y1= mean(Pres[c(2),2]), length = 0.1, 
           lwd= (abs(coeffs$estimate))*3,
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    segments(x0=mean(tbl[2,1]), x1=mean(Pres[1:2,1])-1, y0=mean(tbl[3:2,2]), y1= mean(tbl[3:2,2]),
             lwd= (abs(coeffs$estimate))*3,
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.5), adjustcolor(col.neg,alpha.f = 0.5)))
    text(mean(c(mean(tbl[2,1]),mean(Pres[1:2,1])-1))  , mean(tbl[3:2,2])+1.2, round(coeffs$estimate, digits = digits.estimate), cex=cex.estimate)}
  
  if ((0.01 < coeffs$p.value) & (coeffs$p.value < 0.05)){
    arrows(x0=mean(Pres[1:2,1])+1, x1=mean(Pres[1:2,1])-1, y0=mean(tbl[3:2,2]), y1= mean(Pres[c(2),2]), length = 0.1, 
           lwd= (abs(coeffs$estimate))*3,
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    segments(x0=mean(tbl[2,1]), x1=mean(Pres[1:2,1])-1, y0=mean(tbl[3:2,2]), y1= mean(tbl[3:2,2]),
             lwd= (abs(coeffs$estimate))*3,
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 0.75), adjustcolor(col.neg,alpha.f = 0.75)))
    text(mean(c(mean(tbl[2,1]),mean(Pres[1:2,1])-1))  , mean(tbl[3:2,2])+1.2, labels = paste(round(coeffs$estimate, digits = digits.estimate), "*"), cex=cex.estimate)}
  
  if (coeffs$p.value < 0.01){
    arrows(x0=mean(Pres[1:2,1])+1, x1=mean(Pres[1:2,1])-1, y0=mean(tbl[3:2,2]), y1= mean(Pres[c(2),2]), length = 0.1, 
           lwd= (abs(coeffs$estimate))*3,
           col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    segments(x0=mean(tbl[2,1]), x1=mean(Pres[1:2,1])-1, y0=mean(tbl[3:2,2]), y1= mean(tbl[3:2,2]),
             lwd= (abs(coeffs$estimate))*3,
             col=ifelse(coeffs$estimate>0, adjustcolor(col.pos,alpha.f = 1), adjustcolor(col.neg,alpha.f = 1)))
    text(mean(c(mean(tbl[2,1]),mean(Pres[1:2,1])-1))  , mean(tbl[3:2,2])+1.2, labels = paste(round(coeffs$estimate, digits = digits.estimate), "**"), cex=cex.estimate)}
  
  
  #REPEAT POLYGONS TO FIX OVERLAP WITH LINES
  
  polygon(Treatment[,1], Treatment[,2], col =  adjustcolor("orange",alpha.f = 0.5), border = "white")
  text(mean(Treatment[,1]),mean(Treatment[,2]), Treatment.name, cex = cex.text1)
  
  polygon(Pres[,1], Pres[,2], col =  adjustcolor("lightblue",alpha.f = 1), border = "white")
  text(mean(Pres[,1]),mean(Pres[,2]), Species.name, cex = cex.text1) 
  
  
  
  polygon(SAI_sd[,1], SAI_sd[,2], col =  adjustcolor("mediumpurple1",alpha.f = 0.5), border = "white")
  text(mean(SAI_sd[,1]),mean(SAI_sd[,2]), "SAI", cex = cex.text)
  
  polygon(biom[,1], biom[,2], col =  adjustcolor("mediumpurple1",alpha.f = 0.5), border = "white")
  text(mean(biom[,1]),mean(biom[,2]), "ORTHOPTERA", cex = cex.text)
  
  
  
  polygon(Diver[,1], Diver[,2], col =  adjustcolor("olivedrab3",alpha.f = 1), border = "white")
  text(mean(Diver[,1]),mean(Diver[,2]), "DIVERSITY", cex = cex.text)
  
  polygon(Heter[,1], Heter[,2], col =  adjustcolor("olivedrab3",alpha.f = 1), border = "white")
  text(mean(Heter[,1]),mean(Heter[,2]), "HETEROGENEITY", cex = cex.text)
  
  polygon(Height[,1], Height[,2], col =  adjustcolor("olivedrab3",alpha.f = 1), border = "white")
  text(mean(Height[,1]),mean(Height[,2]), "HEIGHT", cex = cex.text)
  
  polygon(Cover[,1], Cover[,2], col =  adjustcolor("olivedrab3",alpha.f = 1), border = "white")
  text(mean(Cover[,1]),mean(Cover[,2]), "COVER", cex = cex.text)
  
  polygon(Cover_dead[,1], Cover_dead[,2], col =  adjustcolor("olivedrab3",alpha.f = 1), border = "white")
  text(mean(Cover_dead[,1]),mean(Cover_dead[,2]), "COVER_DEAD", cex = cex.text)
  
  
  
  polygon(Diver1[,1], Diver1[,2], col =  adjustcolor("olivedrab3",alpha.f = 0.5), border = "white")
  text(mean(Diver1[,1]),mean(Diver1[,2]), "DIVERSITY", cex = cex.text)
  
  polygon(Heter1[,1], Heter1[,2], col =  adjustcolor("olivedrab3",alpha.f = 0.5), border = "white")
  text(mean(Heter1[,1]),mean(Heter1[,2]), "HETEROGENEITY", cex = cex.text)
  
  polygon(Height1[,1], Height1[,2], col =  adjustcolor("olivedrab3",alpha.f = 0.5), border = "white")
  text(mean(Height1[,1]),mean(Height1[,2]), "HEIGHT", cex = cex.text)
  
  polygon(Cover1[,1], Cover1[,2], col =  adjustcolor("olivedrab3",alpha.f = 0.5), border = "white")
  text(mean(Cover1[,1]),mean(Cover1[,2]), "COVER", cex = cex.text)
  
  polygon(Cover_dead1[,1], Cover_dead1[,2], col =  adjustcolor("olivedrab3",alpha.f = 0.5), border = "white")
  text(mean(Cover_dead1[,1]),mean(Cover_dead1[,2]), "COVER_DEAD", cex = cex.text)
  
  
  
  
  polygon(tbl[,1], tbl[,2], col =  adjustcolor("navajowhite4",alpha.f = 0.5), border = "white")
  text(mean(tbl[,1]),mean(tbl[,2]), "TBL", cex = cex.text)
  
  polygon(par[,1], par[,2], col =  adjustcolor("navajowhite4",alpha.f = 0.5), border = "white")
  text(mean(par[,1]),mean(par[,2]), "PAR", cex = cex.text)
  
  polygon(Fallow[,1], Fallow[,2], col =  adjustcolor("navajowhite4",alpha.f = 0.5), border = "white")
  text(mean(Fallow[,1]),mean(Fallow[,2]), "FALLOW", cex = cex.text)
  
  polygon(crop_diver[,1], crop_diver[,2], col =  adjustcolor("navajowhite4",alpha.f = 0.5), border = "white")
  text(mean(crop_diver[,1]),mean(crop_diver[,2]), "CROP DIVERSITY", cex = cex.text)
  
  polygon(area[,1], area[,2], col =  adjustcolor("yellow",alpha.f = 1), border = "white")
  text(mean(area[,1]),mean(area[,2]), "FIELD AREA", cex = cex.text) 
  
  
  
  # DEFINE POSITION OF ARROWS ONE BY ONE TO PLACE IT EASIER IN X0, X1, Y0, Y1
  # arrows(x0=Treatment[2,1], x1=Pres[1,1], y0=mean(Treatment[2:3,2]), y1= mean(Pres[c(1,4),2]),length = 0.1)
  # arrows(x0=mean(Treatment[c(1:2),1]), x1=SAI_sd[1,1], y0=mean(Treatment[1,2]), y1= mean(SAI_sd[c(1,4),2]),length = 0.1)
  # arrows(x0=mean(Treatment[c(1:2),1]), x1=LAI_sd[1,1], y0=mean(Treatment[1,2]), y1= mean(LAI_sd[c(1,4),2]),length = 0.1)
  # arrows(x0=mean(Treatment[c(1:2),1]), x1=biom[1,1], y0=mean(Treatment[1,2]), y1= mean(biom[c(1,4),2]),length = 0.1)
  # arrows(x0=mean(Treatment[c(1:2),1]), x1=Diver[1,1], y0=mean(Treatment[3,2]), y1= mean(Diver[c(1,4),2]),length = 0.1)
  # arrows(x0=mean(Treatment[c(1:2),1]), x1=Heter[1,1], y0=mean(Treatment[3,2]), y1= mean(Heter[c(1,4),2]),length = 0.1)
  # arrows(x0=mean(Treatment[c(1:2),1]), x1=Height[1,1], y0=mean(Treatment[3,2]), y1= mean(Height[c(1,4),2]),length = 0.1)
  # arrows(x0=mean(Treatment[c(1:2),1]), x1=Cover[1,1], y0=mean(Treatment[3,2]), y1= mean(Cover[c(1,4),2]),length = 0.1)
  # arrows(x0=mean(Treatment[c(1:2),1]), x1=Cover_dead[1,1], y0=mean(Treatment[3,2]), y1= mean(Cover_dead[c(1,4),2]),length = 0.1)
  
  #from vegetation 
  # arrows(x0=mean(Diver[c(2),1]), x1=mean(Pres[1:2,1]), y0=mean(Diver[3:2,2]), y1= mean(Pres[c(4),2]),length = 0.1)
  # arrows(x0=mean(Heter[c(2),1]), x1=mean(Pres[1:2,1]), y0=mean(Heter[3:2,2]), y1= mean(Pres[c(4),2]),length = 0.1)
  # arrows(x0=mean(Height[c(2),1]), x1=mean(Pres[1:2,1]), y0=mean(Height[3:2,2]), y1= mean(Pres[c(4),2]),length = 0.1)
  # arrows(x0=mean(Cover[c(2),1]), x1=mean(Pres[1:2,1]), y0=mean(Cover[3:2,2]), y1= mean(Pres[c(4),2]),length = 0.1)
  # arrows(x0=mean(Cover_dead[c(2),1]), x1=mean(Pres[1:2,1]), y0=mean(Cover_dead[3:2,2]), y1= mean(Pres[c(4),2]),length = 0.1)
  
  
  
  #from vegetation2
  # arrows(x0=mean(Diver1[c(2),1]), x1=mean(biom[1:2,1]), y0=mean(Diver1[3:2,2]), y1= mean(biom[c(1),2]),length = 0.1)
  # arrows(x0=mean(Heter1[c(2),1]), x1=mean(biom[1:2,1]), y0=mean(Heter1[3:2,2]), y1= mean(biom[c(1),2]),length = 0.1)
  # arrows(x0=mean(Height1[c(2),1]), x1=mean(biom[1:2,1]), y0=mean(Height1[3:2,2]), y1= mean(biom[c(1),2]),length = 0.1)
  # arrows(x0=mean(Cover1[c(2),1]), x1=mean(biom[1:2,1]), y0=mean(Cover1[3:2,2]), y1= mean(biom[c(1),2]),length = 0.1)
  # arrows(x0=mean(Cover_dead1[c(2),1]), x1=mean(biom[1:2,1]), y0=mean(Cover_dead1[3:2,2]), y1= mean(biom[c(1),2]),length = 0.1)
  # 
  
  #from food
  # arrows(x0=SAI_sd[3,1], x1=Pres[1,1], y0=SAI_sd[3,2], y1= Pres[1,2],length = 0.1)
  # arrows(x0=LAI_sd[3,1], x1=Pres[1,1], y0=LAI_sd[3,2], y1= Pres[1,2],length = 0.1)
  # arrows(x0=biom[3,1], x1=Pres[1,1], y0=biom[3,2], y1= Pres[1,2],length = 0.1)
  # 
  
  #from landscape
  # arrows(x0=mean(tbl[c(1),1]), x1=mean(biom[2,1]), y0=mean(tbl[3:2,2]), y1= mean(biom[c(2:3),2]),length = 0.1)
  # arrows(x0=mean(par[c(1),1]), x1=mean(biom[2,1]), y0=mean(par[3:2,2]), y1= mean(biom[c(2:3),2]),length = 0.1)
  # arrows(x0=mean(Fallow[c(1),1]), x1=mean(biom[2,1]), y0=mean(Fallow[3:2,2]), y1= mean(biom[c(2:3),2]),length = 0.1)
  # arrows(x0=mean(crop_diver[c(1),1]), x1=mean(biom[2,1]), y0=mean(crop_diver[3:2,2]), y1= mean(biom[c(2:3),2]),length = 0.1)
  
}



