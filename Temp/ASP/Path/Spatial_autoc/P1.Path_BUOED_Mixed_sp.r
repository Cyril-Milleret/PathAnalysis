rm(list=ls())

# PATH ANALYSIS BUOED. 
# Try path dirty with gaussian autocorrelation just to see if it runs
# - Plot with PlotPath3.1 (-LAI)

library(dplyr)
library(tidyr)
library(piecewiseSEM)
library(nlme)
library(MASS) # For correlation structures in glmm
library(aod)
#library(lme4)

setwd("C:/Users/Ana/Documents/PhD/First chapter/Datos/Datos barbechos arrendados/Variables")

sp <- read.csv("Data_path_submission2_sp.csv", sep = ",", header=TRUE, fill = TRUE)
sp <- sp[which(sp$Species == "SC"), ]


################################### PICAR Y HERBICIDAR #################################################################### 


e <- sp[ which(sp$agri_practice %in% c("C", "S+H")), ] #Select treatment

e <- e %>% 
  unnest(agri_practice) %>% 
  mutate(new = 1) %>% 
  spread(agri_practice, new, fill = 0) #Create dummy variable for treatment

length(which(e$Zone == "OCCIDENTAL" & e$`S+H` == "1")) # TREATMENT: 58 p+h and 88 Control en OCCIDENTAL
length(which(e$Zone == "ORIENTAL" & e$`S+H` == "1"))   #             2 p+h and 224 Control en ORIENTAL
length(which(e$Zone == "OCCIDENTAL" & e$C == "1")) 
length(which(e$Zone == "ORIENTAL" & e$C == "1")) 

e <- e[-which(e$Zone == "ORIENTAL"), ] # Delete oriental because only 0 and no use of Zone


colnames(e)[8] <- "Pres"
colnames(e)[12] <- "Diver"
colnames(e)[13] <- "Heter"
colnames(e)[16] <- "par"
colnames(e)[17] <- "Fallow"
colnames(e)[21] <- "crop_diver"
colnames(e)[25] <- "Treatment"

e$Cover<-scale(e$Cover)
e$Height<-scale(e$Height)
e$biom<-scale(e$biom)
e$Cover_dead<-scale(e$Cover_dead)
e$Heter<-scale(e$Heter)
e$Diver<-scale(e$Diver)
e$tbl<-scale(e$tbl)
e$par<-scale(e$par)
e$Fallow<-scale(e$Fallow)
e$crop_diver<-scale(e$crop_diver)
e$area <- scale(e$area)


#Check correlations
g <- e[ ,c(9:23)]
cor(g, use = "complete.obs") 
#LAI_sd - SAI_sd: 0.57 (quitar LAI)

# Check spatial autocorrelation between fields
x1 <- lme( Cover ~ 1, random = ~ 1 | dummy, data = e) #Spatial autocorrelation of cover
plot(Variogram(x1, resType="normalized"))
plot(Variogram(x1, resType="normalized"), form = ~ Lon_x + Lat_y)

x11 <- lme( Cover ~ 1, random = ~ 1 | dummy, correlation = corGaus(form = ~ Lon_x + Lat_y | dummy), data = e) #Spatial autocorrelation of cover
plot(Variogram(x11, resType="normalized"))
plot(Variogram(x11, resType="normalized"), form = ~ Lon_x + Lat_y)

x2 <- lme( Cover ~ Treatment, random = ~ 1 | dummy, data = e) #Spatial autocorrelation of cover
plot(Variogram(x2, resType="normalized"))

x22 <- lme( Cover ~ Treatment, random = ~ 1 | dummy, correlation = corGaus(form = ~ Lon_x + Lat_y | dummy), data = e) #Spatial autocorrelation of cover
plot(Variogram(x22, resType="normalized"))


x3 <- glmmPQL( Pres ~ 1,
              random = ~ 1 | dummy, 
              family = "binomial", data = e) 
plot(Variogram(x3, resType="normalized"))

x33 <- glmmPQL( Pres ~ 1,
               random = ~ 1 | dummy, correlation = corGaus(form = ~ Lon_x + Lat_y | dummy),
               family = "binomial", data = e) 
plot(Variogram(x33, resType="normalized"))

x4 <- glmmPQL( Pres ~ Treatment,
               random = ~ 1 | dummy,
               family = "binomial", data = e) 
plot(Variogram(x4, resType="normalized"))



#PATH ANALYSIS

# Random intercept Year
e.list2 <- psem( 
  lme( Cover ~ Treatment, random =  ~ 1|Year, correlation = corGaus(form = ~ Lon_x + Lat_y),
       data = e, method = 'REML'),
  lme( Cover_dead ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  lme( Height ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  lme( Heter ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  lme( Diver ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  
  lme( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver, 
       correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  lme( SAI_sd ~ Treatment + Cover + Height, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  
  glmmPQL( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + Fallow + crop_diver + par + tbl + area, correlation = corGaus(form = ~ Lon_x + Lat_y),
           random = ~ 1 | Year,
           family = "binomial",data = e))

e.fit2 <- summary(e.list2)
x2 <- rsquared(e.list2) 


# Fill with missing paths
e.list21 <- psem( 
  lme( Cover ~ Treatment + par + tbl + area + Fallow, random =  ~ 1|Year, correlation = corGaus(form = ~ Lon_x + Lat_y),
       data = e, method = 'REML'),
  lme( Cover_dead ~ Treatment + Fallow + par + area + Cover + tbl, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  lme( Height ~ Treatment + tbl + Cover + crop_diver, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  lme( Heter ~ Treatment + crop_diver + tbl + Cover_dead + Height + Cover, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  lme( Diver ~ Treatment + par + area + Cover + Cover_dead + Height + tbl, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  
  lme( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
         Cover + Height + Cover_dead + Heter + Diver, 
       correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  lme( SAI_sd ~ Treatment + Cover + Height + tbl + area + par, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  
  glmmPQL( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + 
             SAI_sd + Fallow + crop_diver + par + tbl + area, correlation = corGaus(form = ~ Lon_x + Lat_y),
           random = ~ 1 | Year,
           family = "binomial",data = e))


e.fit21 <- summary(e.list21)

# conditional R2, which describes the proportion of variance explained by both 
#the fixed and random factors

e.coefs21 <- coefs(e.list21) 
e.coefs21$lowCI <- e.coefs21$Estimate - 2*e.coefs21$Std.Error
e.coefs21$upCI <- e.coefs21$Estimate + 2*e.coefs21$Std.Error


setwd("C:/Users/Ana/Documents/PhD/First chapter/Path analysis/Results_sp")

pdf(file = "Buoed_SH_Y_sp.pdf")
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
         ,Treatment.name= "SHREDDING +\nHERBICIDE"
         ,Species.name="PRESENCE \n SC"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(3, 1),
         cex.estimate = 0.6,
         digits.estimate = 2)

dev.off()


####################################### PICAR #############################################

e <- sp[ which(sp$agri_practice %in% c("C", "S")), ] #Select treatment

e <- e %>% 
  unnest(agri_practice) %>% 
  mutate(new = 1) %>% 
  spread(agri_practice, new, fill = 0) #Create dummy variable for treatment

length(which(e$Zone == "OCCIDENTAL" & e$`S` == "1")) # TREATMENT: 16 p and 88 Control en OCCIDENTAL
length(which(e$Zone == "ORIENTAL" & e$`S` == "1"))   #             15 p+h and 224 Control en ORIENTAL
length(which(e$Zone == "OCCIDENTAL" & e$C == "1")) 
length(which(e$Zone == "ORIENTAL" & e$C == "1")) 


colnames(e)[8] <- "Pres"
colnames(e)[12] <- "Diver"
colnames(e)[13] <- "Heter"
colnames(e)[16] <- "par"
colnames(e)[17] <- "Fallow"
colnames(e)[21] <- "crop_diver"
colnames(e)[25] <- "Treatment"

e$Cover<-scale(e$Cover)
e$Height<-scale(e$Height)
e$biom<-scale(e$biom)
e$Cover_dead<-scale(e$Cover_dead)
e$Heter<-scale(e$Heter)
e$Diver<-scale(e$Diver)
e$tbl<-scale(e$tbl)
e$par<-scale(e$par)
e$Fallow<-scale(e$Fallow)
e$crop_diver<-scale(e$crop_diver)
e$area <- scale(e$area)

#Check correlations
g <- e[ ,c(9:23)]
cor(g, use = "complete.obs") 
#LAI_sd - SAI_sd: 0.57 (quitar LAI)


#PATH ANALYSIS

# Random intercept Year

e.list2 <- psem( 
  lme( Cover ~ Treatment, random =  ~ 1|Year, correlation = corGaus(form = ~ Lon_x + Lat_y),
       data = e, method = 'REML'),
  lme( Cover_dead ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  lme( Height ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  lme( Heter ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  lme( Diver ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  
  lme( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
         Cover + Height + Cover_dead + Heter + Diver, 
       correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  lme( SAI_sd ~ Treatment + Cover + Height, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Year, data = e, method = 'REML'),
  
  glmmPQL( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + 
             SAI_sd + Fallow + crop_diver + par + tbl + area, correlation = corGaus(form = ~ Lon_x + Lat_y),
           random = ~ 1 | Year,
           family = "binomial",data = e))

e.list1 <- psem( 
  lme( Cover ~ Treatment, random =  ~ 1|Zone, correlation = corGaus(form = ~ Lon_x + Lat_y),
       data = e, method = 'REML'),
  lme( Cover_dead ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Zone, data = e, method = 'REML'),
  lme( Height ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Zone, data = e, method = 'REML'),
  lme( Heter ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Zone, data = e, method = 'REML'),
  lme( Diver ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Zone, data = e, method = 'REML'),
  
  lme( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
         Cover + Height + Cover_dead + Heter + Diver, 
       correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Zone, data = e, method = 'REML'),
  lme( SAI_sd ~ Treatment + Cover + Height, correlation = corGaus(form = ~ Lon_x + Lat_y), random = ~ 1 | Zone, data = e, method = 'REML'),
  
  glmmPQL( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + 
             SAI_sd + Fallow + crop_diver + par + tbl + area, correlation = corGaus(form = ~ Lon_x + Lat_y),
           random = ~ 1 | Zone,
           family = "binomial",data = e))

# Random intercept Zone + Year

e.list1 <- psem( 
  lme( Cover ~ Treatment, random =  list(Year = ~ 1, Zone = ~ 1), correlation = corGaus(form = ~ Lon_x + Lat_y),
       data = e, method = 'REML'),
  lme( Cover_dead ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y),  random = list(const = pdBlocked(pdIdent(~ Year - 1), 
                                                                                                               pdIdent(~ Zone - 1))), data = e, method = 'REML'),
  lme( Height ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random =  list(Year = ~1, Zone = ~1), data = e, method = 'REML'),
  lme( Heter ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random =  list(Year = ~1, Zone = ~1), data = e, method = 'REML'),
  lme( Diver ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random =  list(Year = 1, Zone = ~1), data = e, method = 'REML'),
  
  lme( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
         Cover + Height + Cover_dead + Heter + Diver, 
       correlation = corGaus(form = ~ Lon_x + Lat_y), random =  list(Year = ~1, Zone = ~1), data = e, method = 'REML'),
  lme( SAI_sd ~ Treatment + Cover + Height, correlation = corGaus(form = ~ Lon_x + Lat_y), random =  list(Year = ~1, Zone = ~1), data = e, method = 'REML'),
  
  glmmPQL( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + 
             SAI_sd + Fallow + crop_diver + par + tbl + area, correlation = corGaus(form = ~ Lon_x + Lat_y),
           random =  list(Year = ~1, Zone = ~1),
           family = "binomial",data = e))

e.fit1 <- summary(e.list2)
x2 <- rsquared(e.list2) 

const = factor(rep(1, length(e$Year)))

lme( Cover_dead ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y),  random = list(const = pdBlocked(pdIdent(~ Year - 1), 
                                                                                                             pdIdent(~ Zone - 1))), data = e, method = 'REML')
# HERE I DONT MANAGE TO RUN THIS WITH TWO RANDOM EFFECTS
# Random intercept Zone + Year
const = factor(rep(1, length(e$Year)))

e.list1 <- psem( 
  lme( Cover ~ Treatment, random =  list(Year = ~ 1, Zone = ~ 1), correlation = corGaus(form = ~ Lon_x + Lat_y),
       data = e, method = 'REML'),
  lme( Cover_dead ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random =  list(Year = ~1, Zone = ~1), data = e, method = 'REML'),
  lme( Height ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random =  list(Year = ~1, Zone = ~1), data = e, method = 'REML'),
  lme( Heter ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random =  list(Year = ~1, Zone = ~1), data = e, method = 'REML'),
  lme( Diver ~ Treatment, correlation = corGaus(form = ~ Lon_x + Lat_y), random =  list(Year = 1, Zone = ~1), data = e, method = 'REML'),
  
  lme( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
         Cover + Height + Cover_dead + Heter + Diver, 
       correlation = corGaus(form = ~ Lon_x + Lat_y), random =  list(Year = ~1, Zone = ~1), data = e, method = 'REML'),
  lme( SAI_sd ~ Treatment + Cover + Height, correlation = corGaus(form = ~ Lon_x + Lat_y), random =  list(Year = ~1, Zone = ~1), data = e, method = 'REML'),
  
  glmmPQL( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + 
             SAI_sd + Fallow + crop_diver + par + tbl + area, correlation = corGaus(form = ~ Lon_x + Lat_y),
           random =  list(Year = ~1, Zone = ~1),
           family = "binomial",data = e))

e.fit1 <- summary(e.list2)
x2 <- rsquared(e.list2) 



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

e.fit3 <- sem.fit(e.list3, e) # AIC = 567.39
x3 <- rsquared(e.list3) # Conditional r2 = 0.18


# BEST MODEL FIT (R2 + AIC) : YEAR + ZONE

e.list7 <- list(  #YEAR + ZONE 
  lmer( Cover ~ Treatment + par + area + (1|Year) + (1|Zone), data = e, REML = TRUE),
  lmer( Cover_dead ~ Treatment + Fallow + par + area + Cover + (1|Year) + (1|Zone), data = e, REML = TRUE),
  lmer( Height ~ Treatment + Fallow + Cover + (1|Year) + (1|Zone), data = e, REML = TRUE),
  lmer( Heter ~ Treatment + crop_diver + tbl + Cover + Cover_dead + Height + (1|Year) + (1|Zone), data = e, REML = TRUE),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + (1|Year) + (1|Zone), data = e, REML = TRUE),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + (1|Year) + (1|Zone), data = e, REML = TRUE),
  lmer( SAI_sd ~ Treatment + Cover + Height + (1|Year) + (1|Zone), data = e, REML = TRUE),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit7 <- sem.fit(e.list7, e) # AIC = 249
e.coefs7 <- sem.coefs(e.list7,e)
x7 <- rsquared(e.list7) # Conditional r2 = 0.18; Marginal r2 = 0.13
e.coefs7$lowCI <- e.coefs7$estimate - 2*e.coefs7$std.error
e.coefs7$upCI <- e.coefs7$estimate + 2*e.coefs7$std.error


setwd("~/First chapter/Path analysis/Results2")
#setwd("~/PhD/First chapter/Path analysis/Results2")
pdf(file = "Buoed_S_YZ.pdf")
par(mar=c(1,1,1,1))

PlotPath(e.coefs7
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
         ,Species.name="PRESENCE \n SC"
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
x0 <- rsquared(e.list0) # r2 = 0.10

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
x1 <- rsquared(e.list1) # Marginal = Conditional r2 = 0.07

# Random intercept Year
e.list2 <- list( 
  lmer( Cover ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Cover_dead ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Height ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Heter ~ Treatment + (1|Year), data = e, REML = TRUE),
  lmer( Diver ~ Treatment + (1|Year), data = e, REML = TRUE),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year), data = e, REML = TRUE),
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Year), data = e, REML = TRUE),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + (1|Year), data = e, REML = TRUE),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year), 
         family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) # AIC = 686.24
x2 <- rsquared(e.list2) # Marginal = Conditional r2 = 0.10


# Random intercept Zone + Year
e.list3 <- list( 
  lmer( Cover ~ Treatment + (1|Year) + (1|Zone), data = e, REML = TRUE),
  lmer( Cover_dead ~ Treatment + (1|Year) + (1|Zone), data = e, REML = TRUE),
  lmer( Height ~ Treatment + (1|Year) + (1|Zone), data = e, REML = TRUE),
  lmer( Heter ~ Treatment + (1|Year) + (1|Zone), data = e, REML = TRUE),
  lmer( Diver ~ Treatment + (1|Year) + (1|Zone), data = e, REML = TRUE),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + LAI_sd + (1|Year) + (1|Zone), data = e, REML = TRUE),
  lmer( SAI_sd ~ Treatment + LAI_sd + Cover + Height + (1|Year) + (1|Zone), data = e, REML = TRUE),
  lmer( LAI_sd ~ Treatment + SAI_sd + Cover + Height + (1|Year) + (1|Zone), data = e, REML = TRUE),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + LAI_sd + Fallow + crop_diver + par + tbl + area + (1|Year) + (1|Zone), 
         family = "binomial"(link = "logit"), data = e))

e.fit3 <- sem.fit(e.list3, e) # AIC = 648.54
x3 <- rsquared(e.list3) #Conditional r2 = 0.07

# BEST R2: YEAR + ZONE

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

e.fit5 <- sem.fit(e.list5, e) # AIC = 249.21

x5 <- rsquared(e.list5) #Conditional r2 = 0.07

e.coefs5 <- sem.coefs(e.list5,e)

#setwd("~/Phd/First chapter/Path analysis/Results2")
setwd("~/First chapter/Path analysis/Results2")

pdf(file = "Buoed_T_YZ.pdf")
par(mar=c(1,1,1,1))

PlotPath(e.coefs5
         ,cex.text =0.6
         ,cex.text1 = 0.75
         ,offset.poly = 2
         ,significant = 0.05
         ,xlim=c(-20,70)
         ,ylim=c(-30,60)
         ,Treatment.name= "TILLAGE"
         ,Species.name="PRESENCE \n SC"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(3, 1),
         cex.estimate = 0.6,
         digits.estimate = 2)
dev.off()

#LOWEST AIC = ZONE

e.list11 <- list( 
  lmer( Cover ~ Treatment + par + area + (1|Zone), data = e, REML = TRUE),
  lmer( Cover_dead ~ Treatment + Fallow + par +  tbl + Cover + (1|Zone), data = e, REML = TRUE),
  lmer( Height ~ Treatment + Fallow +  tbl + Cover + (1|Zone), data = e, REML = TRUE),
  lmer( Heter ~ Treatment + crop_diver +  tbl + Cover_dead + Height + (1|Zone), data = e, REML = TRUE),
  lmer( Diver ~ Treatment + par + area + Cover + Cover_dead + Height +(1|Zone), data = e, REML = TRUE),
  
  lmer( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
          Cover + Height + Cover_dead + Heter + Diver + area + (1|Zone), data = e, REML = TRUE),
  lmer( SAI_sd ~ Treatment + Cover + Height + crop_diver + par + area + Diver + (1|Zone), data = e, REML = TRUE),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
           SAI_sd + Fallow + crop_diver + par + tbl + area + (1|Zone), 
         family = "binomial"(link = "logit"), data = e) )

e.fit11 <- sem.fit(e.list11, e) # AIC = 193.73
x11 <- rsquared(e.list11) #Conditional r2 = 0.07

e.coefs11 <- sem.coefs(e.list11,e)
e.coefs11$lowCI <- e.coefs11$estimate - 2*e.coefs11$std.error
e.coefs11$upCI <- e.coefs11$estimate + 2*e.coefs11$std.error

#setwd("~/Phd/First chapter/Path analysis/Results2")
setwd("~/First chapter/Path analysis/Results2")

pdf(file = "Buoed_T_Z.pdf")
par(mar=c(1,1,1,1))

PlotPath(e.coefs11
         ,cex.text =0.6
         ,cex.text1 = 0.75
         ,offset.poly = 2
         ,significant = 0.05
         ,xlim=c(-20,70)
         ,ylim=c(-30,60)
         ,Treatment.name= "TILLAGE"
         ,Species.name="PRESENCE \n SC"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(3, 1),
         cex.estimate = 0.6,
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
x0 <- rsquared(e.list0) #r2 = 0.18

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
x1 <- rsquared(e.list1) # Conditional r2 = 0.15 (= Marginal)


# LOWEST AIC: NO RANDOM EFFECTS

e.list01 <- list( 
  lm( Cover ~ Treatment + par + area, data = e),
  lm( Cover_dead ~ Treatment + Fallow + par + tbl, data = e),
  lm( Height ~ Treatment + Fallow + tbl + Cover + Cover_dead, data = e),
  lm( Heter ~ Treatment + tbl + Cover_dead + Height, data = e),
  lm( Diver ~ Treatment + par + area + Cover + Cover_dead, data = e),
  
  lm( biom ~ Treatment + Fallow + crop_diver + par + tbl + 
        Cover + Height + Cover_dead + Heter + Diver + area, data = e),
  lm( SAI_sd ~ Treatment + Cover + Height + par + area + Cover_dead + Diver + biom, data = e),
  
  glm( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + 
         SAI_sd + Fallow + crop_diver + par + tbl + area, 
       family = "binomial"(link = "logit"), data = e) )

e.fit01 <- sem.fit(e.list01, e) # AIC = 184
e.coefs01 <- sem.coefs(e.list01,e)
x01 <- rsquared(e.list01) # r2 = 0.19 
e.coefs01$lowCI <- e.coefs01$estimate - 2*e.coefs01$std.error
e.coefs01$upCI <- e.coefs01$estimate + 2*e.coefs01$std.error


setwd("~/First chapter/Path analysis/Results2")


pdf(file = "Buoed_A.pdf")
par(mar=c(1,1,1,1))

PlotPath(e.coefs01
         ,cex.text =0.6
         ,cex.text1 = 0.75
         ,offset.poly = 2
         ,xlim=c(-20,70)
         ,ylim=c(-30,60)
         ,Treatment.name= "ALFALFA"
         ,Species.name="PRESENCE \n SC"
         ,cex.category = 0.6
         ,plot.axis=FALSE
         ,estimate.box.width=c(3, 1),
         cex.estimate = 0.6,
         digits.estimate = 2)
dev.off()


