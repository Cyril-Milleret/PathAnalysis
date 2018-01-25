
rm(list=ls())

##PATH ANALYSIS MECAL

library(dplyr)
library(tidyr)
library(piecewiseSEM)
library(nlme)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Variables.csv", sep = ",", header=TRUE, fill = TRUE)

f <- f[which(f$EspecieObj == "MECAL"), ]
f[f == 99.00] <- NA
f <- f[-which(duplicated(f[ , 2:15])), ]
f <- f[which(complete.cases(f$Contatge)), ]


################################### PICAR Y HERBICIDAR #################################################################### 

e <- f[ , which(colnames(f) %in% c("CF_A", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom", "SAI_sd","LAI_sd", "TBL_500", "PAR_500","Fallow_500", 
                                   "Irri_500","Zone", "Tractament", "Any", "Codi_Finca"))]

e <- e[ ,c(3:18,1,2)]


e <-e[-which(duplicated(e[ , 1])), ]

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


colnames(e)[2] <- "Pres"
colnames(e)[3] <- "Cover"
colnames(e)[4] <- "Cover_dead"
colnames(e)[5] <- "Height"
colnames(e)[6] <- "Diver"
colnames(e)[7] <- "Heter"
colnames(e)[8] <- "tbl"
colnames(e)[9] <- "par"
colnames(e)[10] <- "Fallow"
colnames(e)[11] <- "Irrig"
colnames(e)[16] <- "Field"
colnames(e)[17] <- "Year"
colnames(e)[19] <- "Treatment"

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

e$Pres[e$Pres > 1] <- 1 # Binomial response

#Check correlations
g <- e[ ,c(3:10, 12:14)]
cor(g, use = "complete.obs")




#PATH ANALYSIS
e <- na.omit(e)

# Random intercept zone
e.list1 <- list( 
  lmer( Cover ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Zone), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Zone), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit1 <- sem.fit(e.list1, e) # AIC = 957.8

# Random intercept Year
e.list2 <- list( 
  lmer( Cover ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) # AIC = 934.72


# Random intercept Zone + Year
e.list3 <- list( 
  lmer( Cover ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Zone) + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))
e.fit3 <- sem.fit(e.list3, e) # AIC = 933.29

#DEFINE BASIS SET WITH ONLY YEAR (BEST)

e.list4 <- list( 
  lmer( Cover ~ Treatment + par + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + Cover + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + Fallow + Cover + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + tbl + Height + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + par + Cover + Cover_dead + Height + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + par + tbl + Cover + Height + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + Fallow + par + tbl + Cover + Height + Diver + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit4 <- sem.fit(e.list4, e) # AIC = 934.72
e.coefs4 <- sem.coefs(e.list4,e)

setwd("~/Path analysis/Path_species")
pdf(file = "Mecal_PH.pdf")
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
         ,Treatment.name= "P + H"
         ,Species.name="PRESENCE \n MECAL"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(2, 1),
         cex.estimate = 0.7,
         digits.estimate = 2)
dev.off()

####################################### PICAR ##############################

e <- f[ , which(colnames(f) %in% c("CF_A", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom", "SAI_sd","LAI_sd", "TBL_500", "PAR_500","Fallow_500", 
                                   "Irri_500","Zone", "Tractament", "Any", "Codi_Finca"))]

e <- e[ ,c(3:18,1,2)]


e <-e[-which(duplicated(e[ , 1])), ]

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


colnames(e)[2] <- "Pres"
colnames(e)[3] <- "Cover"
colnames(e)[4] <- "Cover_dead"
colnames(e)[5] <- "Height"
colnames(e)[6] <- "Diver"
colnames(e)[7] <- "Heter"
colnames(e)[8] <- "tbl"
colnames(e)[9] <- "par"
colnames(e)[10] <- "Fallow"
colnames(e)[11] <- "Irrig"
colnames(e)[16] <- "Field"
colnames(e)[17] <- "Year"
colnames(e)[19] <- "Treatment"

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

e$Pres[e$Pres > 1] <- 1 # Binomial response


#PATH ANALYSIS
e <- na.omit(e)

# Random intercept zone
e.list1 <- list( 
  lmer( Cover ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Zone), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Zone), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit1 <- sem.fit(e.list1, e) # AIC = 910

# Random intercept Year
e.list2 <- list( 
  lmer( Cover ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) # AIC = 920


# Random intercept Zone + Year
e.list3 <- list( 
  lmer( Cover ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Zone) + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))
e.fit3 <- sem.fit(e.list3, e) # AIC = 907

#DEFINE BASIS SET RANDOM ZONE (best)

e.list4 <- list( 
  lmer( Cover ~ Treatment + par + (1|Zone), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + tbl + Cover + (1|Zone), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + Fallow + Cover + (1|Zone), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + Cover + Cover_dead + Height + (1|Zone), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + par + Cover + Cover_dead + (1|Zone), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Zone), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + par + Cover + Height + Diver + LAI_sd + (1|Zone), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + par + Cover + Height + Diver + (1|Zone), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Zone), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit4 <- sem.fit(e.list4, e)
e.coefs4 <- sem.coefs(e.list4,e)

setwd("~/Path analysis/Path_species")
pdf(file = "Mecal_P.pdf")
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
         ,Treatment.name= "PICAR"
         ,Species.name="PRESENCE \n MECAL"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(2, 1),
         cex.estimate = 0.7,
         digits.estimate = 2)
dev.off()

################################### LABRAR ###############################################

e <- f[ , which(colnames(f) %in% c("CF_A", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom", "SAI_sd","LAI_sd", "TBL_500", "PAR_500","Fallow_500", 
                                   "Irri_500","Zone", "Tractament", "Any", "Codi_Finca"))]

e <- e[ ,c(3:18,1,2)]


e <-e[-which(duplicated(e[ , 1])), ]

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


colnames(e)[2] <- "Pres"
colnames(e)[3] <- "Cover"
colnames(e)[4] <- "Cover_dead"
colnames(e)[5] <- "Height"
colnames(e)[6] <- "Diver"
colnames(e)[7] <- "Heter"
colnames(e)[8] <- "tbl"
colnames(e)[9] <- "par"
colnames(e)[10] <- "Fallow"
colnames(e)[11] <- "Irrig"
colnames(e)[16] <- "Field"
colnames(e)[17] <- "Year"
colnames(e)[19] <- "Treatment"

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

e$Pres[e$Pres > 1] <- 1 # Binomial response

#PATH ANALYSIS
e <- na.omit(e)

# Random intercept zone
e.list1 <- list( 
  lmer( Cover ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Zone), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Zone), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit1 <- sem.fit(e.list1, e) # AIC = 917

# Random intercept Year
e.list2 <- list( 
  lmer( Cover ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) # AIC = 904


# Random intercept Zone + Year
e.list3 <- list( 
  lmer( Cover ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Zone) + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))
e.fit3 <- sem.fit(e.list3, e) # AIC = 901

#Define basis set with Zone + Year

e.list4 <- list( 
  lmer( Cover ~ Treatment + par + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + Fallow + Cover + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + Cover + Height + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + Cover + par + Cover_dead + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + par + Cover + Cover_dead + Height + Diver + LAI_sd + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + par + Cover + Height + Diver + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Zone) + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))
e.fit4 <- sem.fit(e.list4, e) 

e.coefs4 <- sem.coefs(e.list4,e)

setwd("~/Path analysis/Path_species")
pdf(file = "Mecal_L.pdf")
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
         ,Treatment.name= "LABRAR"
         ,Species.name="PRESENCE \n MECAL"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(2, 1),
         cex.estimate = 0.7,
         digits.estimate = 2)
dev.off()

################################### ALFALFA ###############################################

e <- f[ , which(colnames(f) %in% c("CF_A", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom", "SAI_sd","LAI_sd", "TBL_500", "PAR_500","Fallow_500", 
                                   "Irri_500","Zone", "Tractament", "Any", "Codi_Finca"))]

e <- e[ ,c(3:18,1,2)]


e <-e[-which(duplicated(e[ , 1])), ]

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


colnames(e)[2] <- "Pres"
colnames(e)[3] <- "Cover"
colnames(e)[4] <- "Cover_dead"
colnames(e)[5] <- "Height"
colnames(e)[6] <- "Diver"
colnames(e)[7] <- "Heter"
colnames(e)[8] <- "tbl"
colnames(e)[9] <- "par"
colnames(e)[10] <- "Fallow"
colnames(e)[11] <- "Irrig"
colnames(e)[16] <- "Field"
colnames(e)[17] <- "Year"
colnames(e)[19] <- "Treatment"

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

e$Pres[e$Pres > 1] <- 1 # Binomial response

#PATH ANALYSIS
e <- na.omit(e)

# Random intercept zone
e.list1 <- list( 
  lmer( Cover ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Zone), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + (1|Zone), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Zone), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit1 <- sem.fit(e.list1, e) # AIC = 980

# Random intercept Year
e.list2 <- list( 
  lmer( Cover ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) # AIC = 974


# Random intercept Zone + Year
e.list3 <- list( 
  lmer( Cover ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Zone) + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))
e.fit3 <- sem.fit(e.list3, e) # AIC = 974


#DEFINE BASIS SET WITH ZONE + YEAR AS WITH BUOED

e.list4 <- list( 
  lmer( Cover ~ Treatment + Fallow + par + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + tbl + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + Fallow + Cover + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + Cover + Cover_dead + Height + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + par + Cover + Cover_dead + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + par + Cover + Cover_dead + Height + Diver + LAI_sd + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + par + Cover + Height + Diver + (1|Zone) + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Zone) + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit4 <- sem.fit(e.list4, e) # AIC = 974

e.coefs4 <- sem.coefs(e.list4,e)

setwd("~/Path analysis/Path_species")
pdf(file = "Mecal_A.pdf")
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
         ,Species.name="PRESENCE \n MECAL"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(2, 1),
         cex.estimate = 0.7,
         digits.estimate = 2)
dev.off()
