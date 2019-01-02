rm(list=ls())

#PATH ANALYSIS MECAL Plot with PlotPath3.1 (-LAI)

library(dplyr)
library(tidyr)
library(piecewiseSEM)
library(nlme)
library(lme4)

setwd("C:/Users/Ana/Documents/PhD/First chapter/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Variables.csv", sep = ",", header=TRUE, fill = TRUE)
colnames(f)[6] <- "EspecieObj"
f <- f[which(f$EspecieObj == "MECAL"), ]


f[f == 99.00] <- NA
f <- f[-which(duplicated(f[ , 2:15])), ]
f <- f[which(complete.cases(f$Contatge)), ]
length(which(f$Contatge > 0)) 

f_or <- f[ which(f$Zone == "ORIENTAL"), ] # Occidental y oriental or only oriental?
length(which(f_or$Contatge > 0))
f_oc <- f[ which(f$Zone == "OCCIDENTAL"), ] 
length(which(f_oc$Contatge > 0)) #INCLUDE BOTH

no_na<-na.omit(f)


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


length(which(e$Zone == "OCCIDENTAL" & e$`Picar i herbicidar` == "1")) # TREATMENT: 70 p+h and 124 Control en OCCIDENTAL
length(which(e$Zone == "ORIENTAL" & e$`Picar i herbicidar` == "1"))   #             3 p+h and 308 Control en ORIENTAL
length(which(e$Zone == "OCCIDENTAL" & e$Control == "1")) 
length(which(e$Zone == "ORIENTAL" & e$Control == "1")) 

e <- e[-which(e$Zone == "ORIENTAL"), ] # Delete occidental because only 0 and no use of Zone
table(e$Any)


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

# No random effects

e.list0 <- list( 
  lm( Cover ~ Treatment, data = e),
  lm( Cover_dead ~ Treatment, data = e),
  lm( Height ~ Treatment, data = e),
  lm( Heter ~ Treatment, data = e),
  lm( Diver ~ Treatment, data = e),
  
  lm( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
        Cover + Height + Cover_dead + Heter + Diver, data = e),
  lm( SAI_sd ~ Treatment + Cover + Height, data = e),
  
  glm( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
         SAI_sd + Fallow + crop_diver + par + tbl + area, 
       family = "binomial"(link = "logit"), data = e) )

e.fit0 <- sem.fit(e.list0, e) #438.33
x0 <- rsquared(e.list0) #r2 = 0.48


# Random intercept Year
e.list2 <- list( 
  lmer( Cover ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Cover_dead ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Height ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Heter ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Diver ~ Treatment + (1|Year), data = e, REML = TRUE),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + (1|Year), data = e, REML = TRUE),
  lmer( SAI_sd ~ Treatment + Cover + Height + (1|Year), data = e, REML = TRUE),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) # AIC = 379.35
x2 <- rsquared(e.list2) # Conditional (=marginal) r2 = 0.47

#LOWEST AIC: YEAR -> INCLUDE MISSING PATHS

e.list21 <- list( 
  lmer( Cover ~ Treatment + area + (1|Year), data = e, REML = TRUE),
  lmer( Cover_dead ~ Treatment + area + Cover + Fallow + Heter + (1|Year), data = e, REML = TRUE),
  lmer( Height ~ Treatment + crop_diver + Cover + area + Fallow + Cover_dead + (1|Year), data = e, REML = TRUE),
  lmer( Heter ~ Treatment + crop_diver + (1|Year), data = e, REML = TRUE),
  lmer( Diver ~ Treatment + par + area + Cover + Height + Cover_dead + (1|Year), data = e, REML = TRUE),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + (1|Year), data = e, REML = TRUE),
  lmer( SAI_sd ~ Treatment + Cover + Height + (1|Year), data = e, REML = TRUE),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit21 <- sem.fit(e.list21, e) # AIC = 379.35
x21 <- rsquared(e.list21)
e.coefs21 <- sem.coefs(e.list21,e) 
e.coefs21$lowCI <- e.coefs21$estimate - 2*e.coefs21$std.error
e.coefs21$upCI <- e.coefs21$estimate + 2*e.coefs21$std.error

setwd("C:/Users/ana.sanz/Documents/First chapter/Path analysis/Results2")
pdf(file = "Mecal_SH.pdf")
par(mar=c(1,1,1,1))

PlotPath(e.coefs21
         ,cex.text =0.6
         ,cex.text1 = 0.75
         ,offset.poly = 2
         ,significant = 0.05
         ,xlim=c(-20,70)
         ,ylim=c(-30,60)
         ,col.pos="black"
         ,col.neg="red"
         ,col.non.signifi="grey"
         ,Treatment.name= "SHREDDING +\n HERBICIDE"
         ,Species.name="PRESENCE \n CL"
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


length(which(e$Zone == "OCCIDENTAL" & e$Picar == "1")) # TREATMENT: 21 p and 124 Control en OCCIDENTAL
length(which(e$Zone == "ORIENTAL" & e$Picar == "1"))   #            28 p and 308 Control en ORIENTAL
length(which(e$Zone == "OCCIDENTAL" & e$Control == "1")) 
length(which(e$Zone == "ORIENTAL" & e$Control == "1")) 
table(e$Any)

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
cor(g, use = "complete.obs") #Crop_diver - Tree: 0.66 (Quitar TREE)
#LAI_sd - SAI_sd: 0.57 (quitar LAI)


#PATH ANALYSIS
e <- na.omit(e)

# No random

e.list0 <- list( 
  lm( Cover ~ Treatment, data = e),
  lm( Cover_dead ~ Treatment, data = e),
  lm( Height ~ Treatment, data = e),
  lm( Heter ~ Treatment, data = e),
  lm( Diver ~ Treatment, data = e),
  
  lm( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
        Cover + Height + Cover_dead + Heter + Diver, data = e),
  lm( SAI_sd ~ Treatment + Cover + Height, data = e),
  
  glm( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
         SAI_sd + Fallow + crop_diver + par + tbl + area, 
       family = "binomial"(link = "logit"), data = e) )

e.fit0 <- sem.fit(e.list0, e) # AIC = 560.31
x0 <- rsquared(e.list0) #r2 = 0.33

# Random intercept zone
e.list1 <- list( 
  lmer( Cover ~ Treatment + (1|Zone), data = e, REML = TRUE),
  lmer( Cover_dead ~ Treatment + (1|Zone), data = e, REML = TRUE),
  lmer( Height ~ Treatment + (1|Zone), data = e, REML = TRUE),
  lmer( Heter ~ Treatment + (1|Zone), data = e, REML = TRUE),
  lmer( Diver ~ Treatment + (1|Zone), data = e, REML = TRUE),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + (1|Zone), data = e, REML = TRUE),
  lmer( SAI_sd ~ Treatment + Cover + Height + (1|Zone), data = e, REML = TRUE),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit1 <- sem.fit(e.list1, e) # AIC = 572.87
x1 <- rsquared(e.list1) #Conditional r2 = 0.30

# Random intercept Year
e.list2 <- list( 
  lmer( Cover ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Cover_dead ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Height ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Heter ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Diver ~ Treatment + (1|Year), data = e, REML = TRUE),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + (1|Year), data = e, REML = TRUE),
  lmer( SAI_sd ~ Treatment + Cover + Height + (1|Year), data = e, REML = TRUE),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) # AIC = 552.06
x2 <- rsquared(e.list2) # Cond r2 = 0.33

#LOWEST AIC AND HIGHEST CONDITIONAL R2: YEAR
e.list21 <- list( 
  lmer( Cover ~ Treatment + par + area + Fallow + (1|Year), data = e, REML = TRUE),
  lmer( Cover_dead ~ Treatment + Fallow + par + area + Cover + tbl + (1|Year), data = e, REML = TRUE),
  lmer( Height ~ Treatment + Fallow + tbl + Cover + (1|Year), data = e, REML = TRUE),
  lmer( Heter ~ Treatment + crop_diver + tbl + Cover + (1|Year), data = e, REML = TRUE),
  lmer( Diver ~ Treatment +  par + area + Cover + Cover_dead + (1|Year), data = e, REML = TRUE),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + (1|Year), data = e, REML = TRUE),
  lmer( SAI_sd ~ Treatment + Cover + Height +  par + area + Cover_dead + Diver + (1|Year), data = e, REML = TRUE),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit21 <- sem.fit(e.list21, e) # AIC = 211.34
e.coefs21 <- sem.coefs(e.list21,e)
x21 <- rsquared(e.list21) # Marginal R2 = 0.31
e.coefs21$lowCI <- e.coefs21$estimate - 2*e.coefs21$std.error
e.coefs21$upCI <- e.coefs21$estimate + 2*e.coefs21$std.error

setwd("~/First chapter/Path analysis/Results2")
pdf(file = "Mecal_S.pdf")
par(mar=c(1,1,1,1))

PlotPath(e.coefs21
         ,cex.text =0.6
         ,cex.text1 = 0.75
         ,offset.poly = 2
         ,significant = 0.05
         ,xlim=c(-20,70)
         ,ylim=c(-30,60)
         ,col.pos="black"
         ,col.neg="red"
         ,col.non.signifi="grey"
         ,Treatment.name= "SHREDDING"
         ,Species.name="PRESENCE \n CL"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(3, 1),
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


length(which(e$Zone == "OCCIDENTAL" & e$Llaurar == "1")) # TREATMENT: 76 p and 124 Control en OCCIDENTAL
length(which(e$Zone == "ORIENTAL" & e$Llaurar == "1"))   #            95 p and 308 Control en ORIENTAL
table(e$Any)

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

# No random

e.list0 <- list( 
  lm( Cover ~ Treatment, data = e),
  lm( Cover_dead ~ Treatment, data = e),
  lm( Height ~ Treatment, data = e),
  lm( Heter ~ Treatment, data = e),
  lm( Diver ~ Treatment, data = e),
  
  lm( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
        Cover + Height + Cover_dead + Heter + Diver, data = e),
  lm( SAI_sd ~ Treatment + Cover + Height, data = e),
  
  glm( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
         SAI_sd + Fallow + crop_diver + par + tbl + area, 
       family = "binomial"(link = "logit"), data = e) )

e.fit0 <- sem.fit(e.list0, e) # AIC = 606.75
x0 <- rsquared(e.list0) # R2 = 0.26


# Random intercept zone

e.list1 <- list( 
  lmer( Cover ~ Treatment + (1|Zone), data = e, REML = TRUE),
  lmer( Cover_dead ~ Treatment + (1|Zone), data = e, REML = TRUE),
  lmer( Height ~ Treatment + (1|Zone), data = e, REML = TRUE),
  lmer( Heter ~ Treatment + (1|Zone), data = e, REML = TRUE),
  lmer( Diver ~ Treatment + (1|Zone), data = e, REML = TRUE),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + (1|Zone), data = e, REML = TRUE),
  lmer( SAI_sd ~ Treatment + Cover + Height + (1|Zone), data = e, REML = TRUE),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Zone), 
         family = "binomial"(link = "logit"), data = e) )

e.fit1 <- sem.fit(e.list1, e) # AIC = 579.78
x1 <- rsquared(e.list1) # Conditional R2 = 0.25


# Random intercept Year
e.list2 <- list( 
  lmer( Cover ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Cover_dead ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Height ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Heter ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Diver ~ Treatment + (1|Year), data = e, REML = TRUE),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + (1|Year), data = e, REML = TRUE),
  lmer( SAI_sd ~ Treatment + Cover + Height + (1|Year), data = e, REML = TRUE),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) # AIC = 588.85
x2 <- rsquared(e.list2) # Conditional R2 = 0.25


# Random intercept Zone + Year
e.list3 <- list( 
  lmer( Cover ~ Treatment + (1|Year) + (1|Zone), data = e, REML = TRUE),
  lmer( Cover_dead ~ Treatment + (1|Year) + (1|Zone), data = e, REML = TRUE),
  lmer( Height ~ Treatment + (1|Year) + (1|Zone), data = e, REML = TRUE),
  lmer( Heter ~ Treatment + (1|Year) + (1|Zone), data = e, REML = TRUE),
  lmer( Diver ~ Treatment + (1|Year) + (1|Zone), data = e, REML = TRUE),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + (1|Year) + (1|Zone), data = e, REML = TRUE),
  lmer( SAI_sd ~ Treatment + Cover + Height + (1|Year) + (1|Zone), data = e, REML = TRUE),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit3 <- sem.fit(e.list3, e) # AIC = 563.19
x3 <- rsquared(e.list3) # Conditional R2 = 0.30


# LOWEST AIC and HIGHEST R2: YEAR + ZONE

e.list31 <- list( 
  lmer( Cover ~ Treatment + par + area + (1|Year) + (1|Zone), data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Height ~ Treatment + Fallow + Cover + (1|Year) + (1|Zone), data = e),
  lmer( Heter ~ Treatment + crop_diver + par + Cover_dead + Height + (1|Year) + (1|Zone), data = e),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + Height + (1|Year) + (1|Zone), data = e),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + (1|Year) + (1|Zone), data = e),
  lmer( SAI_sd ~ Treatment + Cover + Height + par + area + Diver + (1|Year) + (1|Zone), data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit31 <- sem.fit(e.list31, e) # AIC = 227
x31 <- rsquared(e.list31) # Marginal R2 = 0.24

e.coefs31 <- sem.coefs(e.list31,e)
e.coefs31$lowCI <- e.coefs31$estimate - 2*e.coefs31$std.error
e.coefs31$upCI <- e.coefs31$estimate + 2*e.coefs31$std.error


setwd("~/First chapter/Path analysis/Results2")
pdf(file = "Mecal_T.pdf")
par(mar=c(1,1,1,1))

PlotPath(e.coefs31
         ,cex.text =0.6
         ,cex.text1 = 0.75
         ,offset.poly = 2
         ,significant = 0.05
         ,xlim=c(-20,70)
         ,ylim=c(-30,60)
         ,col.pos="black"
         ,col.neg="red"
         ,col.non.signifi="grey"
         ,Treatment.name= "TILLAGE"
         ,Species.name="PRESENCE \n CL"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(3, 1),
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


length(which(e$Zone == "OCCIDENTAL" & e$Alfals == "1")) # TREATMENT: 0 a and 124 Control en OCCIDENTAL
length(which(e$Zone == "ORIENTAL" & e$Alfals == "1"))   #            140 a and 308 Control en ORIENTAL
table(e$Any)

e <- e[-which(e$Zone == "OCCIDENTAL"), ] # Delete occidental because only 0 and no use of Zone


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

# No random

e.list0 <- list( 
  lm( Cover ~ Treatment, data = e),
  lm( Cover_dead ~ Treatment, data = e),
  lm( Height ~ Treatment, data = e),
  lm( Heter ~ Treatment, data = e),
  lm( Diver ~ Treatment, data = e),
  
  lm( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
        Cover + Height + Cover_dead + Heter + Diver, data = e),
  lm( SAI_sd ~ Treatment + Cover + Height, data = e),
  
  glm( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
         SAI_sd + Fallow + crop_diver + par + tbl + area, 
       family = "binomial"(link = "logit"), data = e) )

e.fit0 <- sem.fit(e.list0, e) # AIC = 606.8
x0 <- rsquared(e.list0) # R2 = 0.29


# Random intercept Year
e.list1 <- list( 
  lmer( Cover ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Cover_dead ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Height ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Heter ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Diver ~ Treatment + (1|Year), data = e, REML = TRUE),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + (1|Year), data = e, REML = TRUE),
  lmer( SAI_sd ~ Treatment + Cover + Height + (1|Year), data = e, REML = TRUE),
  
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit1 <- sem.fit(e.list1, e) # AIC = 653.73
x1 <- rsquared(e.list1) # Conditional R2 = 0.31


# LOWEST AIC AND BEST R2: NO RANDOM EFFECTS

e.list01 <- list( 
  lm( Cover ~ Treatment + par + area, data = e),
  lm( Cover_dead ~ Treatment + Fallow + par + tbl, data = e),
  lm( Height ~ Treatment + Fallow + tbl + Cover + Cover_dead, data = e),
  lm( Heter ~ Treatment + tbl + Cover_dead + Height, data = e),
  lm( Diver ~ Treatment + par + area + Cover + Cover_dead, data = e),
  
  lm( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
        Cover + Height + Cover_dead + Heter + Diver + area + Diver, data = e),
  lm( SAI_sd ~ Treatment + Cover + Height + par + area + Cover_dead + Diver, data = e),
  
  glm( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
         SAI_sd + Fallow + crop_diver + par + tbl + area, 
       family = "binomial"(link = "logit"), data = e) )

e.fit01 <- sem.fit(e.list01, e) 
x01 <- rsquared(e.list01) # Conditional R2 = 0.29


e.coefs01 <- sem.coefs(e.list01,e)
e.coefs01$lowCI <- e.coefs01$estimate - 2*e.coefs01$std.error
e.coefs01$upCI <- e.coefs01$estimate + 2*e.coefs01$std.error

setwd("~/First chapter/Path analysis/Results2")
pdf(file = "Mecal_A.pdf")
par(mar=c(1,1,1,1))

PlotPath(e.coefs01
         ,cex.text =0.6
         ,cex.text1 = 0.75
         ,offset.poly = 2
         ,significant = 0.05
         ,xlim=c(-20,70)
         ,ylim=c(-30,60)
         ,col.pos="black"
         ,col.neg="red"
         ,col.non.signifi="grey"
         ,Treatment.name= "ALFALFA"
         ,Species.name="PRESENCE \n CL"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(3, 1),
         cex.estimate = 0.7,
         digits.estimate = 2)
dev.off()
