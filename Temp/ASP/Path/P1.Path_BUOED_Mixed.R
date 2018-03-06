
rm(list=ls())

# PATH ANALYSIS BUOED. 
  # - Plot with PlotPath3 (-LAI)
  # - All with Year + Zone as random intercepts

library(dplyr)
library(tidyr)
library(piecewiseSEM)
library(nlme)
library(lme4)

setwd("~/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Variables.csv", sep = ",", header=TRUE, fill = TRUE)
colnames(f)[6] <- "EspecieObj"
f <- f[which(f$EspecieObj == "BUOED"), ]


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
e$Height<-scale(e$Height)
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
                             #LAI_sd - SAI_sd: 0.57 (quitar LAI)


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
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit1 <- sem.fit(e.list1, e) # AIC = 649

# Random intercept Year
e.list2 <- list( 
  lmer( Cover ~ Treatment + (1|Year), data = e),
  lmer( Cover_dead ~ Treatment + (1|Year), data = e),
  lmer( Height ~ Treatment + (1|Year), data = e),
  lmer( Heter ~ Treatment + (1|Year), data = e),
  lmer( Diver ~ Treatment + (1|Year), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year), data = e),
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Year), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + (1|Year), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) # AIC = 623


# Random intercept Zone + Year
e.list3 <- list( 
  lmer( Cover ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Height ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Heter ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Diver ~ Treatment + (1|Year) + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year) + (1|Zone), data = e),
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Year) + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + (1|Year) + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit3 <- sem.fit(e.list3, e) # AIC = 630


# DEFINE BASIS SET YEAR + ZONE: Model with only directed path doesnt fit the data. Include missing paths and correlated errors

e.list4 <- list( 
  lmer( Cover ~ Treatment + par + area + (1|Year) + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + area + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Height ~ Treatment + Cover + tbl + (1|Year) + (1|Zone), data = e),
  lmer( Heter ~ Treatment + crop_diver + Cover + Cover_dead + Height + (1|Year) + (1|Zone), data = e),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + Height + (1|Year) + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year) + (1|Zone), data = e),
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Year) + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + par + tbl + area + Diver + Cover_dead + (1|Year) + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit4 <- sem.fit(e.list4, e) # AIC = 630

e.coefs4 <- sem.coefs(e.list4,e)

#TRY

# 1 - Only Year Random (Lowest AIC) -> SAME RESULTS

e.list5 <- list( 
  lmer( Cover ~ Treatment + par + area + (1|Year), data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + area + Cover + (1|Year), data = e),
  lmer( Height ~ Treatment + Cover + tbl + (1|Year), data = e),
  lmer( Heter ~ Treatment + crop_diver + Cover + Cover_dead + Height + (1|Year), data = e),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + Height + (1|Year), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year), data = e),
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Year), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + par + tbl + area + Diver + Cover_dead + (1|Year), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit5 <- sem.fit(e.list5, e) # AIC = 630

e.coefs5 <- sem.coefs(e.list5,e)

# 2 - SAI <-> LAI correlated (0.57): REMOVE LAI -> All the same but direct effect of treatment (+)

e.list6 <- list( 
  lmer( Cover ~ Treatment + par + area + (1|Year), data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + area + Cover + (1|Year), data = e),
  lmer( Height ~ Treatment + Cover + tbl + (1|Year), data = e),
  lmer( Heter ~ Treatment + crop_diver + Cover + Cover_dead + Height + (1|Year), data = e),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + Height + (1|Year), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + (1|Year), data = e),
  lmer( SAI_sd ~ Treatment + Cover + Height + (1|Year), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit6 <- sem.fit(e.list6, e) 

e.coefs6 <- sem.coefs(e.list6,e)

          e.list7 <- list( #FROM YEAR + ZONE -> Similar
            lmer( Cover ~ Treatment + par + area + (1|Year) + (1|Zone), data = e),
            lmer( Cover_dead ~ Treatment + Fallow + par + area + Cover + (1|Year) + (1|Zone), data = e),
            lmer( Height ~ Treatment + Cover + tbl + (1|Year) + (1|Zone), data = e),
            lmer( Heter ~ Treatment + crop_diver + Cover + Cover_dead + Height + (1|Year) + (1|Zone), data = e),
            lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + Height + (1|Year) + (1|Zone), data = e),
            
            lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
                    Cover + Height + Cover_dead + Heter + Diver + (1|Year) + (1|Zone), data = e),
            lmer( SAI_sd ~ Treatment + Cover + Height + (1|Year) + (1|Zone), data = e),
            
            glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
                     SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
                   family = "binomial"(link = "logit"), data = e))
          
          e.fit7 <- sem.fit(e.list7, e) # AIC = 630
          
          e.coefs7 <- sem.coefs(e.list7,e)


setwd("~/First chapter/Path analysis/Results")
pdf(file = "Buoed_SH.pdf")
par(mar=c(1,1,1,1))

PlotPath(e.coefs7
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
         ,Species.name="PRESENCE \n BUOED"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(2, 1),
         cex.estimate = 0.7,
         digits.estimate = 2)
dev.off()

####################################### PICAR #############################################

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
e$Height<-scale(e$Height)
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
#LAI_sd - SAI_sd: 0.57 (quitar una?)


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
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit1 <- sem.fit(e.list1, e) # AIC = 617

# Random intercept Year
e.list2 <- list( 
  lmer( Cover ~ Treatment + (1|Year), data = e),
  lmer( Cover_dead ~ Treatment + (1|Year), data = e),
  lmer( Height ~ Treatment + (1|Year), data = e),
  lmer( Heter ~ Treatment + (1|Year), data = e),
  lmer( Diver ~ Treatment + (1|Year), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year), data = e),
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Year), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + (1|Year), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) # AIC = 607


# Random intercept Zone + Year
e.list3 <- list( 
  lmer( Cover ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Height ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Heter ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Diver ~ Treatment + (1|Year) + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year) + (1|Zone), data = e),
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Year) + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + (1|Year) + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit3 <- sem.fit(e.list3, e) # AIC = 613


# DEFINE BASIS SET YEAR + ZONE: Model with only directed path doesnt fit the data. Include missing paths and correlated errors

e.list4 <- list( 
  lmer( Cover ~ Treatment + par + area + (1|Year) + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + area + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Height ~ Treatment + Fallow + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Heter ~ Treatment + crop_diver + tbl + Cover + Cover_dead + Height + (1|Year) + (1|Zone), data = e),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + (1|Year) + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year) + (1|Zone), data = e),
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Year) + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + par + area + Diver + (1|Year) + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit4 <- sem.fit(e.list4, e) # AIC = 613

e.coefs4 <- sem.coefs(e.list4,e)

#TRY

#1 - Only year (Lowest AIC) -> LESS RESULTS

e.list5 <- list( 
  lmer( Cover ~ Treatment + par + area + (1|Year), data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + area + Cover + (1|Year), data = e),
  lmer( Height ~ Treatment + Fallow + Cover + (1|Year), data = e),
  lmer( Heter ~ Treatment + crop_diver + tbl + Cover + Cover_dead + Height + (1|Year), data = e),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + (1|Year), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year), data = e),
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Year), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + par + area + Diver + (1|Year), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit5 <- sem.fit(e.list5, e) # AIC = 613

e.coefs5 <- sem.coefs(e.list5,e)

# 2 - SAI <-> LAI correlated (0.57): REMOVE LAI 

e.list6 <- list( 
  lmer( Cover ~ Treatment + par + area + (1|Year), data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + area + Cover + (1|Year), data = e),
  lmer( Height ~ Treatment + Fallow + Cover + (1|Year), data = e),
  lmer( Heter ~ Treatment + crop_diver + tbl + Cover + Cover_dead + Height + (1|Year), data = e),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + (1|Year), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + (1|Year), data = e),
  lmer( SAI_sd ~ Treatment + Cover + Height + (1|Year), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit6 <- sem.fit(e.list6, e) # AIC = 613

e.coefs6 <- sem.coefs(e.list6,e)

      e.list7 <- list(  #YEAR + ZONE -> GOOD!
        lmer( Cover ~ Treatment + par + area + (1|Year) + (1|Zone), data = e),
        lmer( Cover_dead ~ Treatment + Fallow + par + area + Cover + (1|Year) + (1|Zone), data = e),
        lmer( Height ~ Treatment + Fallow + Cover + (1|Year) + (1|Zone), data = e),
        lmer( Heter ~ Treatment + crop_diver + tbl + Cover + Cover_dead + Height + (1|Year) + (1|Zone), data = e),
        lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + (1|Year) + (1|Zone), data = e),
        
        lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
                Cover + Height + Cover_dead + Heter + Diver + (1|Year) + (1|Zone), data = e),
        lmer( SAI_sd ~ Treatment + Cover + Height + (1|Year) + (1|Zone), data = e),
        
        glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
                 SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
               family = "binomial"(link = "logit"), data = e))
      
      e.fit7 <- sem.fit(e.list7, e) # AIC = 613
      
      e.coefs7 <- sem.coefs(e.list7,e)



setwd("~/First chapter/Path analysis/Results")
pdf(file = "Buoed_S.pdf")
par(mar=c(1,1,1,1))

PlotPath(e.coefs7
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
         ,Species.name="PRESENCE \n BUOED"
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
e$Height<-scale(e$Height)
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
#LAI_sd - SAI_sd: 0.57 (quitar una?)


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
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit1 <- sem.fit(e.list1, e) # AIC = 666

# Random intercept Year
e.list2 <- list( 
  lmer( Cover ~ Treatment + (1|Year), data = e),
  lmer( Cover_dead ~ Treatment + (1|Year), data = e),
  lmer( Height ~ Treatment + (1|Year), data = e),
  lmer( Heter ~ Treatment + (1|Year), data = e),
  lmer( Diver ~ Treatment + (1|Year), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year), data = e),
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Year), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + (1|Year), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) # AIC = 686


# Random intercept Zone + Year
e.list3 <- list( 
  lmer( Cover ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Height ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Heter ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Diver ~ Treatment + (1|Year) + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year) + (1|Zone), data = e),
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Year) + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + (1|Year) + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit3 <- sem.fit(e.list3, e) # AIC = 648


# DEFINE BASIS SET ZONE + YEAR (Lowest AIC): Model with only directed path doesnt fit the data. Include missing paths and correlated errors

e.list4 <- list( 
  lmer( Cover ~ Treatment + par + area + (1|Year) + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Height ~ Treatment + Fallow + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Heter ~ Treatment + crop_diver + par + Cover_dead + Height + (1|Year) + (1|Zone), data = e),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + Height + (1|Year) + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year) + (1|Zone), data = e),
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Year) + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + area + Cover_dead + Diver + (1|Year) + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit4 <- sem.fit(e.list4, e)

e.coefs4 <- sem.coefs(e.list4,e)

#TRY

# 1 - Only Year random: Don't try, is not the lowest AIC

# 2 - SAI <-> LAI correlated (0.57): REMOVE LAI -> aalmost better (p-value 0.053...)

e.list5 <- list( 
  lmer( Cover ~ Treatment + par + area + (1|Year) + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Height ~ Treatment + Fallow + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Heter ~ Treatment + crop_diver + par + Cover_dead + Height + (1|Year) + (1|Zone), data = e),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + Height + (1|Year) + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + (1|Year) + (1|Zone), data = e),
  lmer( SAI_sd ~ Treatment + Cover + Height + (1|Year) + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit5 <- sem.fit(e.list5, e)

e.coefs5 <- sem.coefs(e.list5,e)


setwd("~/First chapter/Path analysis/Results")
pdf(file = "Buoed_T.pdf")
par(mar=c(1,1,1,1))

PlotPath(e.coefs5
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
         ,Species.name="PRESENCE \n BUOED"
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
colnames(e)[21] <- "Treatment"

e$Cover<-scale(e$Cover)
e$Height<-scale(e$Height)
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
#LAI_sd - SAI_sd: 0.57 (quitar una?)


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
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit1 <- sem.fit(e.list1, e) # AIC = 738

# Random intercept Year
e.list2 <- list( 
  lmer( Cover ~ Treatment + (1|Year), data = e),
  lmer( Cover_dead ~ Treatment + (1|Year), data = e),
  lmer( Height ~ Treatment + (1|Year), data = e),
  lmer( Heter ~ Treatment + (1|Year), data = e),
  lmer( Diver ~ Treatment + (1|Year), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year), data = e),
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Year), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + (1|Year), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) # AIC = 727


# Random intercept Zone + Year
e.list3 <- list( 
  lmer( Cover ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Height ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Heter ~ Treatment + (1|Year) + (1|Zone), data = e),
  lmer( Diver ~ Treatment + (1|Year) + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year) + (1|Zone), data = e),
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Year) + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + (1|Year) + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit3 <- sem.fit(e.list3, e) # AIC = 746


# DEFINE BASIS SET YEAR + ZONE: Model with only directed path doesnt fit the data. Include missing paths and correlated errors

e.list4 <- list( 
  lmer( Cover ~ Treatment + par + area + Fallow + (1|Year) + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par +  tbl + area + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Height ~ Treatment + Fallow + Cover + Cover_dead + (1|Year) + (1|Zone), data = e),
  lmer( Heter ~ Treatment + Cover_dead + Height + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + (1|Year) + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year) + (1|Zone), data = e),
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Year) + (1|Zone), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + par + area + Diver + Cover_dead + (1|Year) + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit4 <- sem.fit(e.list4, e) # AIC = 261

e.coefs4 <- sem.coefs(e.list4,e)

#TRY

#1 - Only Year random (Lowest AIC) -> SIMILAR RESULTS

e.list5 <- list( 
  lmer( Cover ~ Treatment + par + area + Fallow + (1|Year) , data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par +  tbl + area + Cover + (1|Year) , data = e),
  lmer( Height ~ Treatment + Fallow + Cover + Cover_dead + (1|Year) , data = e),
  lmer( Heter ~ Treatment + Cover_dead + Height + Cover + (1|Year) , data = e),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + (1|Year) , data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year), data = e),
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Year), data = e),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + par + area + Diver + Cover_dead + (1|Year), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit5 <- sem.fit(e.list5, e) # AIC = 261

e.coefs5 <- sem.coefs(e.list5,e)

# 2 - SAI <-> LAI correlated (0.57): REMOVE LAI (From Year + Zone) -> Similar

e.list6 <- list( 
  lmer( Cover ~ Treatment + par + area + Fallow + (1|Year) + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par +  tbl + area + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Height ~ Treatment + Fallow + Cover + Cover_dead + (1|Year) + (1|Zone), data = e),
  lmer( Heter ~ Treatment + Cover_dead + Height + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + (1|Year) + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + (1|Year) + (1|Zone), data = e),
  lmer( SAI_sd ~ Treatment + Cover + Height + (1|Year) + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit6 <- sem.fit(e.list6, e) # AIC = 261

e.coefs6 <- sem.coefs(e.list6,e)


setwd("~/First chapter/Path analysis/Results")
pdf(file = "Buoed_A.pdf")
par(mar=c(1,1,1,1))

PlotPath(e.coefs6
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
         ,Species.name="PRESENCE \n BUOED"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(2, 1),
         cex.estimate = 0.7,
         digits.estimate = 2)
dev.off()


