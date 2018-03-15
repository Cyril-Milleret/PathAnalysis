
rm(list=ls())

#PATH ANALYSIS TERAX. Plot with PlotPath4 (-SAI)

library(dplyr)
library(tidyr)
library(piecewiseSEM)
library(nlme)
library(lme4)

setwd("~/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Variables.csv", sep = ",", header=TRUE, fill = TRUE)
colnames(f)[6] <- "EspecieObj"
f <- f[which(f$EspecieObj == "TERAX_m"), ]


f[f == 99.00] <- NA
f <- f[-which(duplicated(f[ , 2:15])), ]
f <- f[which(complete.cases(f$Contatge)), ]
length(which(f$Contatge > 0)) 

f_or <- f[ which(f$Zone == "ORIENTAL"), ] # Occidental y oriental or only oriental?
length(which(f_or$Contatge > 0))
f_oc <- f[ which(f$Zone == "OCCIDENTAL"), ] 
length(which(f_oc$Contatge > 0)) #INCLUDE BOTH



################################### PICAR Y HERBICIDAR #################################################################### 

e <- f[ , which(colnames(f) %in% c("CF_A", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom", "SAI_sd","LAI_sd", "TBL_500", "PAR_500","Fallow_500", 
                                   "Irri_500", "Tree_500", "shan_500", "Zone", "Tractament", "Any", "Codi_Finca", "area"))]


e <-e[-which(duplicated(e[ , 8])), ] #Modify adding good duplicates!

e <- e[ which(e$Tractament %in% c("Control", "Picar i herbicidar")), ] #Select treatment

e <- e %>% 
  unnest(Tractament) %>% 
  mutate(new = 1) %>% 
  spread(Tractament, new, fill = 0) #Create dummy variable for treatment


length(which(e$Zone == "OCCIDENTAL" & e$`Picar i herbicidar` == "1"))
length(which(e$Zone == "ORIENTAL" & e$`Picar i herbicidar` == "1"))
length(which(e$Zone == "OCCIDENTAL")) # 178 #Possibility to add ZONE?SI
length(which(e$Zone == "ORIENTAL")) #302

e$Zone <- as.character(e$Zone)
e$Zone[which(e$Zone == "OCCIDENTAL")] <- 0
e$Zone[which(e$Zone == "ORIENTAL")] <- 1
e$Zone <- as.factor(e$Zone)


colnames(e)[3] <- "Pres"
colnames(e)[4] <- "Cover"
colnames(e)[5] <- "Cover_dead"
colnames(e)[6] <- "Height"
colnames(e)[7] <- "Diver"
colnames(e)[9] <- "Heter"
colnames(e)[10] <- "area"
colnames(e)[11] <- "tbl"
colnames(e)[12] <- "par"
colnames(e)[13] <- "Fallow"
colnames(e)[14] <- "Tree"
colnames(e)[15] <- "Irrig"
colnames(e)[20] <- "crop_diver"
colnames(e)[2] <- "Year"
colnames(e)[22] <- "Treatment"

e$Cover<-scale(e$Cover)
e$Cover_sq<-scale(I(e$Cover^2))
e$Height<-scale(e$Height)
e$Height_sq<-scale(I(e$Height^2))
e$biom<-scale(e$biom)
e$Cover_dead<-scale(e$Cover_dead)
e$Heter<-scale(e$Heter)
e$Diver<-scale(e$Diver)
e$tbl<-scale(e$tbl)
e$par<-scale(e$par)
e$Fallow<-scale(e$Fallow)
e$Irrig<-scale(e$Irrig)
e$Tree<-scale(e$Tree)
e$crop_diver<-scale(e$crop_diver)
e$area <- scale(e$area)


e$Pres[e$Pres > 1] <- 1 # Binomial response

#Check correlations
g <- e[ ,c(4:7,9:15, 17:20)]
cor(g, use = "complete.obs") #Crop_diver - Tree: 0.66 (Quitar Tree?)
#LAI_sd - SAI_sd: 0.57 (quitar SAI)


#PATH ANALYSIS
e <- na.omit(e)

# Random intercept Zone + Year

e.list1 <- list( 
  lmer( Cover ~ Treatment + (1|Zone) + (1|Year), data = e),
  lmer( Cover_sq ~ Treatment + (1|Zone) + (1|Year), data = e),
  lmer( Cover_dead ~ Treatment + (1|Zone) + (1|Year), data = e),
  lmer( Height ~ Treatment + (1|Zone) + (1|Year), data = e),
  lmer( Height_sq ~ Treatment + (1|Zone) + (1|Year), data = e),
  lmer( Heter ~ Treatment + (1|Zone) + (1|Year), data = e),
  lmer( Diver ~ Treatment + (1|Zone) + (1|Year), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Cover_sq + Height + Height_sq + Cover_dead + Heter + Diver + LAI_sd + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + Cover + Height + (1|Zone) + (1|Year), data = e),
  
  glmer( Pres ~ Treatment + Cover + Cover_sq + Height + Height_sq + Cover_dead + Heter + Diver + biom + 
           LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Zone) + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit1 <- sem.fit(e.list1, e) # AIC = 663


# BASIS SET NO QUADRATIC

e.list2 <- list( 
  lmer( Cover ~ Treatment + par + area + Fallow + (1|Year) + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + area + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Height ~ Treatment + Cover + tbl + area + (1|Year) + (1|Zone), data = e),
  lmer( Heter ~ Treatment + crop_diver + Cover + Cover_dead + Height + (1|Year) + (1|Zone), data = e),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + Height + (1|Year) + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year) + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + Cover + Height + par + tbl + area + Diver + Cover_dead + (1|Year) + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) # AIC = 214
e.coefs2 <- sem.coefs(e.list2,e)

# DEFINDE BASIS SET WITH QUADRATIC EFFECTS (FROM E.LIST1)

e.list3 <- list( 
  lmer( Cover ~ Treatment + par + area + Fallow + (1|Zone) + (1|Year), data = e),
  lmer( Cover_sq ~ Treatment + par + Cover + (1|Zone) + (1|Year), data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + area + Cover + Cover_sq + (1|Zone) + (1|Year), data = e),
  lmer( Height ~ Treatment + Cover + tbl + area + (1|Zone) + (1|Year), data = e),
  lmer( Height_sq ~ Treatment + Fallow + area + Height + Cover + Cover_sq + (1|Zone) + (1|Year), data = e),
  lmer( Heter ~ Treatment + crop_diver + Cover + Cover_sq + Cover_dead + Height + Height_sq + (1|Zone) + (1|Year), data = e),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_sq + Cover_dead + Height + Height_sq + (1|Zone) + (1|Year), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Cover_sq + Height + Height_sq + Cover_dead + Heter + Diver + LAI_sd + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + Cover + Height + par + tbl + area + Cover_sq + Height_sq + Diver + Cover_dead + Fallow + (1|Zone) + (1|Year), data = e),
  
  glmer( Pres ~ Treatment + Cover + Cover_sq + Height + Height_sq + Cover_dead + Heter + Diver + biom + 
           LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Zone) + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit3 <- sem.fit(e.list3, e) # AIC = 663
e.coefs3 <- sem.coefs(e.list3,e) # No effects of quadractic cover and height on presence

setwd("~/First chapter/Path analysis/Results")
pdf(file = "Terax_SH.pdf")
par(mar=c(1,1,1,1))

PlotPath(e.coefs2
         ,cex.text =0.6
         ,cex.text1 = 0.75
         ,offset.poly = 2
         ,significant = 0.05
         ,xlim=c(-20,70)
         ,ylim=c(-30,60)
         ,col.pos="blue"
         ,col.neg="red"
         ,col.non.signifi="grey"
         ,Treatment.name= "SHREDDING +\n HERBICIDE"
         ,Species.name="PRESENCE \n TERAX"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(2, 1),
         cex.estimate = 0.7,
         digits.estimate = 2)
dev.off()

####################################### PICAR ##############################

e <- f[ , which(colnames(f) %in% c("CF_A", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom", "SAI_sd","LAI_sd", "TBL_500", "PAR_500","Fallow_500", 
                                   "Irri_500", "Tree_500", "shan_500", "Zone", "Tractament", "Any", "Codi_Finca", "area"))]


e <-e[-which(duplicated(e[ , 8])), ] #Modify adding good duplicates!

e <- e[ which(e$Tractament %in% c("Control", "Picar")), ] #Select treatment

e <- e %>% 
  unnest(Tractament) %>% 
  mutate(new = 1) %>% 
  spread(Tractament, new, fill = 0) #Create dummy variable for treatment


length(which(e$Zone == "OCCIDENTAL" & e$`Picar i herbicidar` == "1"))
length(which(e$Zone == "ORIENTAL" & e$`Picar i herbicidar` == "1"))
length(which(e$Zone == "OCCIDENTAL")) # 178 #Possibility to add ZONE?SI
length(which(e$Zone == "ORIENTAL")) #302

e$Zone <- as.character(e$Zone)
e$Zone[which(e$Zone == "OCCIDENTAL")] <- 0
e$Zone[which(e$Zone == "ORIENTAL")] <- 1
e$Zone <- as.factor(e$Zone)


colnames(e)[3] <- "Pres"
colnames(e)[4] <- "Cover"
colnames(e)[5] <- "Cover_dead"
colnames(e)[6] <- "Height"
colnames(e)[7] <- "Diver"
colnames(e)[9] <- "Heter"
colnames(e)[10] <- "area"
colnames(e)[11] <- "tbl"
colnames(e)[12] <- "par"
colnames(e)[13] <- "Fallow"
colnames(e)[14] <- "Tree"
colnames(e)[15] <- "Irrig"
colnames(e)[20] <- "crop_diver"
colnames(e)[2] <- "Year"
colnames(e)[22] <- "Treatment"

e$Cover<-scale(e$Cover)
e$Cover_sq<-scale(I(e$Cover^2))
e$Height<-scale(e$Height)
e$Height_sq<-scale(I(e$Height^2))
e$biom<-scale(e$biom)
e$Cover_dead<-scale(e$Cover_dead)
e$Heter<-scale(e$Heter)
e$Diver<-scale(e$Diver)
e$tbl<-scale(e$tbl)
e$par<-scale(e$par)
e$Fallow<-scale(e$Fallow)
e$Irrig<-scale(e$Irrig)
e$Tree<-scale(e$Tree)
e$crop_diver<-scale(e$crop_diver)
e$area <- scale(e$area)


e$Pres[e$Pres > 1] <- 1 # Binomial response

#Check correlations
g <- e[ ,c(4:7,9:15, 17:20)]
cor(g, use = "complete.obs") #Crop_diver - Tree: 0.66 (Quitar Tree?)
#LAI_sd - SAI_sd: 0.57 (quitar SAI)


#PATH ANALYSIS
e <- na.omit(e)



# Random intercept Zone + Year

e.list1 <- list( 
  lmer( Cover ~ Treatment + (1|Zone) + (1|Year), data = e),
  lmer( Cover_sq ~ Treatment + (1|Zone) + (1|Year), data = e),
  lmer( Cover_dead ~ Treatment + (1|Zone) + (1|Year), data = e),
  lmer( Height ~ Treatment + (1|Zone) + (1|Year), data = e),
  lmer( Height_sq ~ Treatment + (1|Zone) + (1|Year), data = e),
  lmer( Heter ~ Treatment + (1|Zone) + (1|Year), data = e),
  lmer( Diver ~ Treatment + (1|Zone) + (1|Year), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Cover_sq + Height + Height_sq + Cover_dead + Heter + Diver + LAI_sd + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + Cover + Height + (1|Zone) + (1|Year), data = e),
  
  glmer( Pres ~ Treatment + Cover + Cover_sq + Height + Height_sq + Cover_dead + Heter + Diver + biom + 
           LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Zone) + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit1 <- sem.fit(e.list1, e) # AIC = 663

# DEFINE BASIS SET YEAR + ZONE: Model with only directed path doesnt fit the data. Include missing paths and correlated errors

e.list2 <- list( 
  lmer( Cover ~ Treatment + par + area + Fallow + (1|Year) + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + area + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Height ~ Treatment + Fallow + Cover + par + (1|Year) + (1|Zone), data = e),
  lmer( Heter ~ Treatment + crop_diver + tbl + Cover + Cover_dead + Height + (1|Year) + (1|Zone), data = e),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + (1|Year) + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year) + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + Cover + Height + par + area + Diver + tbl + Fallow + (1|Year) + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))


e.fit2 <- sem.fit(e.list2, e) # AIC = 213
e.coefs2 <- sem.coefs(e.list2,e) 

# BASIS SET QUADRATIC EFFECTS 

e.list3 <- list( 
  lmer( Cover ~ Treatment + par + area + Fallow + (1|Zone) + (1|Year), data = e),
  lmer( Cover_sq ~ Treatment + par + Cover + (1|Zone) + (1|Year), data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + area + Cover + Cover_sq + (1|Zone) + (1|Year), data = e),
  lmer( Height ~ Treatment + Fallow + Cover + Cover_sq + par + (1|Zone) + (1|Year), data = e),
  lmer( Height_sq ~ Treatment + Fallow + Height + Cover_sq + Cover + (1|Zone) + (1|Year), data = e),
  lmer( Heter ~ Treatment + crop_diver + Cover + tbl + Cover_sq + Cover_dead + Height + Height_sq + (1|Zone) + (1|Year), data = e),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_sq + Cover_dead + (1|Zone) + (1|Year), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Cover_sq + Height + Height_sq + Cover_dead + Heter + Diver + LAI_sd + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + Cover + Height + par + area + Cover_sq + Diver + tbl + Fallow + (1|Zone) + (1|Year), data = e),
  
  glmer( Pres ~ Treatment + Cover + Cover_sq + Height + Height_sq + Cover_dead + Heter + Diver + biom + 
           LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Zone) + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit3 <- sem.fit(e.list3, e) # AIC = 663
e.coefs3 <- sem.coefs(e.list3,e) #No efectos cuadráticos en la presencia de sisón


setwd("~/First chapter/Path analysis/Results")
pdf(file = "Terax_S.pdf")
par(mar=c(1,1,1,1))

PlotPath(e.coefs2
         ,cex.text =0.6
         ,cex.text1 = 0.75
         ,offset.poly = 2
         ,significant = 0.05
         ,xlim=c(-20,70)
         ,ylim=c(-30,60)
         ,col.pos="blue"
         ,col.neg="red"
         ,col.non.signifi="grey"
         ,Treatment.name= "SHREDDING"
         ,Species.name="PRESENCE \n TERAX"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(2, 1),
         cex.estimate = 0.7,
         digits.estimate = 2)
dev.off()

################################### LABRAR ###############################################

e <- f[ , which(colnames(f) %in% c("CF_A", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom", "SAI_sd","LAI_sd", "TBL_500", "PAR_500","Fallow_500", 
                                   "Irri_500", "Tree_500", "shan_500", "Zone", "Tractament", "Any", "Codi_Finca", "area"))]


e <-e[-which(duplicated(e[ , 8])), ] #Modify adding good duplicates!

e$Tractament[which(e$Tractament == "Curronar")] <- "Llaurar" 
e <- e[ which(e$Tractament %in% c("Control", "Llaurar")), ] 

e <- e %>% 
  unnest(Tractament) %>% 
  mutate(new = 1) %>% 
  spread(Tractament, new, fill = 0) #Create dummy variable for treatment


length(which(e$Zone == "OCCIDENTAL" & e$`Picar i herbicidar` == "1"))
length(which(e$Zone == "ORIENTAL" & e$`Picar i herbicidar` == "1"))
length(which(e$Zone == "OCCIDENTAL")) # 178 #Possibility to add ZONE?SI
length(which(e$Zone == "ORIENTAL")) #302

e$Zone <- as.character(e$Zone)
e$Zone[which(e$Zone == "OCCIDENTAL")] <- 0
e$Zone[which(e$Zone == "ORIENTAL")] <- 1
e$Zone <- as.factor(e$Zone)


colnames(e)[3] <- "Pres"
colnames(e)[4] <- "Cover"
colnames(e)[5] <- "Cover_dead"
colnames(e)[6] <- "Height"
colnames(e)[7] <- "Diver"
colnames(e)[9] <- "Heter"
colnames(e)[10] <- "area"
colnames(e)[11] <- "tbl"
colnames(e)[12] <- "par"
colnames(e)[13] <- "Fallow"
colnames(e)[14] <- "Tree"
colnames(e)[15] <- "Irrig"
colnames(e)[20] <- "crop_diver"
colnames(e)[2] <- "Year"
colnames(e)[22] <- "Treatment"

e$Cover<-scale(e$Cover)
e$Cover_sq<-scale(I(e$Cover^2))
e$Height<-scale(e$Height)
e$Height_sq<-scale(I(e$Height^2))
e$biom<-scale(e$biom)
e$Cover_dead<-scale(e$Cover_dead)
e$Heter<-scale(e$Heter)
e$Diver<-scale(e$Diver)
e$tbl<-scale(e$tbl)
e$par<-scale(e$par)
e$Fallow<-scale(e$Fallow)
e$Irrig<-scale(e$Irrig)
e$Tree<-scale(e$Tree)
e$crop_diver<-scale(e$crop_diver)
e$area <- scale(e$area)


e$Pres[e$Pres > 1] <- 1 # Binomial response

#Check correlations
g <- e[ ,c(4:7,9:15, 17:20)]
cor(g, use = "complete.obs") #Crop_diver - Tree: 0.66 (Quitar Tree?)
#LAI_sd - SAI_sd: 0.57 (quitar SAI)


#PATH ANALYSIS
e <- na.omit(e)

# Random intercept zone
e.list1 <- list( 
  lmer( Cover ~ Treatment + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + (1|Zone), data = e),
  lmer( Height ~ Treatment + (1|Zone), data = e),
  lmer( Heter ~ Treatment + (1|Zone), data = e),
  lmer( Diver ~ Treatment + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + Cover + Height + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit1 <- sem.fit(e.list1, e) # AIC = 676

# Random intercept Year
e.list2 <- list( 
  lmer( Cover ~ Treatment + (1|Year), data = e),
  lmer( Cover_dead ~ Treatment + (1|Year), data = e),
  lmer( Height ~ Treatment + (1|Year), data = e),
  lmer( Heter ~ Treatment + (1|Year), data = e),
  lmer( Diver ~ Treatment + (1|Year), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year), data = e),
  lmer( LAI_sd ~ Treatment + Cover + Height + (1|Year), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) # AIC = 700


# Random intercept Zone + Year
e.list3 <- list( 
  lmer( Cover ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Height ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Heter ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Diver ~ Treatment + (1|Year) + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year) + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + Cover + Height + (1|Year) + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit3 <- sem.fit(e.list3, e) # AIC = 657

# DEFINE BASIS SET YEAR + ZONE: Model with only directed path doesnt fit the data. Include missing paths and correlated errors

e.list4 <- list( 
  lmer( Cover ~ Treatment + par + area + Fallow + (1|Year) + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Height ~ Treatment + Fallow + Cover + par + (1|Year) + (1|Zone), data = e),
  lmer( Heter ~ Treatment + crop_diver + par + Cover_dead + Height + (1|Year) + (1|Zone), data = e),
  lmer( Diver ~ Treatment + par + Cover + Cover_dead + Height + area + (1|Year) + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year) + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + Cover + Height + par + area + Diver + Fallow + tbl + Cover_dead + (1|Year) + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit4 <- sem.fit(e.list4, e) 
e.coefs4 <- sem.coefs(e.list4,e)

setwd("~/First chapter/Path analysis/Results")
pdf(file = "Terax_T.pdf")
par(mar=c(1,1,1,1))

PlotPath(e.coefs4
         ,cex.text =0.6
         ,cex.text1 = 0.75
         ,offset.poly = 2
         ,significant = 0.05
         ,xlim=c(-20,70)
         ,ylim=c(-30,60)
         ,col.pos="blue"
         ,col.neg="red"
         ,col.non.signifi="grey"
         ,Treatment.name= "TILLAGE"
         ,Species.name="PRESENCE \n TERAX"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(2, 1),
         cex.estimate = 0.7,
         digits.estimate = 2)
dev.off()



################################### ALFALFA ###############################################

e <- f[ , which(colnames(f) %in% c("CF_A", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom", "SAI_sd","LAI_sd", "TBL_500", "PAR_500","Fallow_500", 
                                   "Irri_500", "Tree_500", "shan_500", "Zone", "Tractament", "Any", "Codi_Finca", "area"))]


e <-e[-which(duplicated(e[ , 8])), ] #Modify adding good duplicates!

e <- e[ which(e$Tractament %in% c("Control", "Alfals")), ] #Select treatment

e <- e %>% 
  unnest(Tractament) %>% 
  mutate(new = 1) %>% 
  spread(Tractament, new, fill = 0) #Create dummy variable for treatment


length(which(e$Zone == "OCCIDENTAL" & e$`Picar i herbicidar` == "1"))
length(which(e$Zone == "ORIENTAL" & e$`Picar i herbicidar` == "1"))
length(which(e$Zone == "OCCIDENTAL")) # 178 #Possibility to add ZONE?SI
length(which(e$Zone == "ORIENTAL")) #302

e$Zone <- as.character(e$Zone)
e$Zone[which(e$Zone == "OCCIDENTAL")] <- 0
e$Zone[which(e$Zone == "ORIENTAL")] <- 1
e$Zone <- as.factor(e$Zone)


colnames(e)[3] <- "Pres"
colnames(e)[4] <- "Cover"
colnames(e)[5] <- "Cover_dead"
colnames(e)[6] <- "Height"
colnames(e)[7] <- "Diver"
colnames(e)[9] <- "Heter"
colnames(e)[10] <- "area"
colnames(e)[11] <- "tbl"
colnames(e)[12] <- "par"
colnames(e)[13] <- "Fallow"
colnames(e)[14] <- "Tree"
colnames(e)[15] <- "Irrig"
colnames(e)[20] <- "crop_diver"
colnames(e)[2] <- "Year"
colnames(e)[22] <- "Treatment"

e$Cover<-scale(e$Cover)
e$Cover_sq<-scale(I(e$Cover^2))
e$Height<-scale(e$Height)
e$Height_sq<-scale(I(e$Height^2))
e$biom<-scale(e$biom)
e$Cover_dead<-scale(e$Cover_dead)
e$Heter<-scale(e$Heter)
e$Diver<-scale(e$Diver)
e$tbl<-scale(e$tbl)
e$par<-scale(e$par)
e$Fallow<-scale(e$Fallow)
e$Irrig<-scale(e$Irrig)
e$Tree<-scale(e$Tree)
e$crop_diver<-scale(e$crop_diver)
e$area <- scale(e$area)


e$Pres[e$Pres > 1] <- 1 # Binomial response

#Check correlations
g <- e[ ,c(4:7,9:15, 17:20)]
cor(g, use = "complete.obs") #Crop_diver - Tree: 0.66 (Quitar Tree?)
#LAI_sd - SAI_sd: 0.57 (quitar SAI)


#PATH ANALYSIS
e <- na.omit(e)

# Random intercept zone
e.list1 <- list( 
  lmer( Cover ~ Treatment + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + (1|Zone), data = e),
  lmer( Height ~ Treatment + (1|Zone), data = e),
  lmer( Heter ~ Treatment + (1|Zone), data = e),
  lmer( Diver ~ Treatment + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + Cover + Height + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit1 <- sem.fit(e.list1, e) # AIC = 764

# Random intercept Year
e.list2 <- list( 
  lmer( Cover ~ Treatment + (1|Year), data = e),
  lmer( Cover_dead ~ Treatment + (1|Year), data = e),
  lmer( Height ~ Treatment + (1|Year), data = e),
  lmer( Heter ~ Treatment + (1|Year), data = e),
  lmer( Diver ~ Treatment + (1|Year), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year), data = e),
  lmer( LAI_sd ~ Treatment + Cover + Height + (1|Year), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) # AIC = 758


# Random intercept Zone + Year
e.list3 <- list( 
  lmer( Cover ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Height ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Heter ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Diver ~ Treatment + (1|Year) + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year) + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + Cover + Height + (1|Year) + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit3 <- sem.fit(e.list3, e) # AIC = 769

# DEFINE BASIS SET YEAR + ZONE: Model with only directed path doesnt fit the data. Include missing paths and correlated errors

e.list4 <- list( 
  lmer( Cover ~ Treatment +  par + area + Fallow + (1|Year) + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + tbl + area + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Height ~ Treatment + Fallow + Cover + Cover_dead + (1|Year) + (1|Zone), data = e),
  lmer( Heter ~ Treatment + Cover_dead + Height + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + (1|Year) + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year) + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + Cover + Height +  par + area + Diver + Fallow + tbl + (1|Year) + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit4 <- sem.fit(e.list4, e) # AIC = 769
e.coefs4 <- sem.coefs(e.list4,e)

setwd("~/Path analysis/Path_species")
pdf(file = "Terax_A.pdf")
par(mar=c(1,1,1,1))

PlotPath(e.coefs4
         ,cex.text =0.6
         ,cex.text1 = 0.75
         ,offset.poly = 2
         ,significant = 0.05
         ,xlim=c(-20,70)
         ,ylim=c(-30,60)
         ,col.pos="blue"
         ,col.neg="red"
         ,col.non.signifi="grey"
         ,Treatment.name= "ALFALFA"
         ,Species.name="PRESENCE \n TERAX"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(2, 1),
         cex.estimate = 0.7,
         digits.estimate = 2)
dev.off()

