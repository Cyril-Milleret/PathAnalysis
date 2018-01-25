

##PATH ANALYSIS MECAL

library(dplyr)
library(tidyr)
library(piecewiseSEM)
library(nlme)

setwd("C:/Users/Ana/Documents/PHD/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Variables.csv", sep = ",", header=TRUE, fill = TRUE)

f <- f[which(f$EspecieObj == "MECAL"), ]
f[f == 99.00] <- NA
f <- f[-which(duplicated(f[ , 2:15])), ]
f <- f[which(complete.cases(f$Contatge)), ]

############################### PICAR ######################################

#PREPARE DATA: TREATMENT = Picar i Herbicidar join with Picar; All variables (LandscaPe, Food, Vegetation) + Zone 

e <- f[ , which(colnames(f) %in% c("CF_A", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom","SAI_sd","LAI_sd", "TBL_200", "PAR_200","Fallow_200", 
                                   "Zone", "Tractament"))]

e <-e [-which(duplicated(e[ , 1])), ]

e$Tractament[which(e$Tractament == "Picar")] <- "Picar i herbicidar" #Join Picar y Picar i herbicidar
e <- e[ which(e$Tractament %in% c("Control", "Picar i herbicidar")), ] #Select treatment


e <- e %>% 
  unnest(Tractament) %>% 
  mutate(new = 1) %>% 
  spread(Tractament, new, fill = 0) #Create dummy variable for treatment

e$`Picar i herbicidar` <- as.character(e$`Picar i herbicidar`)
length(which(e$Zone == "OCCIDENTAL" & e$`Picar i herbicidar` == "1"))
length(which(e$Zone == "ORIENTAL" & e$`Picar i herbicidar` == "1"))
length(which(e$Zone == "OCCIDENTAL")) # 178 #Possibility to add ZONE?SI
length(which(e$Zone == "ORIENTAL")) #302

e$Zone <- as.character(e$Zone)
e$Zone[which(e$Zone == "OCCIDENTAL")] <- 0
e$Zone[which(e$Zone == "ORIENTAL")] <- 1


colnames(e)[2] <- "Pres"
colnames(e)[3] <- "Cover"
colnames(e)[4] <- "Cover_dead"
colnames(e)[5] <- "Height"
colnames(e)[6] <- "Diver"
colnames(e)[7] <- "Heter"
colnames(e)[8] <- "tbl"
colnames(e)[9] <- "par"
colnames(e)[10] <- "Fallow"
colnames(e)[16] <- "Picar"

e$Cover<-scale(e$Cover)
e$Height<-scale(e$Height)
e$biom<-scale(e$biom)
e$Cover_dead<-scale(e$Cover_dead)
e$Heter<-scale(e$Heter)
e$Diver<-scale(e$Diver)
e$tbl<-scale(e$tbl)
e$par<-scale(e$par)
e$Fallow<-scale(e$Fallow)

e$Pres[e$Pres > 1] <- 1 # Binomial response

#PATH ANALYSIS

#1. Create list of structured equations
e.list <- list( 
  glm( Cover ~ Picar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Cover_dead ~ Picar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Height ~ Picar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Heter ~ Picar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Diver ~ Picar + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( biom ~ Picar + Fallow + par + tbl + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( SAI_sd ~ Picar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( LAI_sd ~ Picar + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
       + Fallow + par + tbl + Zone, na.action = na.omit, family = "binomial", data = e))


#2. Check basis set with possible independence claims
e.basis.set <- sem.basis.set(e.list)

#3. Conduct d-sep test and evaluate the fit
e.fit <- sem.fit(e.list, e) 

#Add Picar in Presence and effect of tbl and par in food and veg
e.list2 <- list( 
  glm( Cover ~ Picar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Cover_dead ~ Picar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Height ~ Picar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Heter ~ Picar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Diver ~ Picar + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( biom ~ Picar + Fallow + par + tbl + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( SAI_sd ~ Picar + par + tbl + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( LAI_sd ~ Picar + par + tbl + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( Pres ~ Picar + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
       + Fallow + par + tbl + Zone, na.action = na.omit, family = "binomial", data = e))

e.fit2 <- sem.fit(e.list2, e) 

#Add rest
e.list3 <- list( 
  glm( Cover ~ Picar + Fallow + par + tbl + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Cover_dead ~ Picar + tbl + Cover + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Height ~ Cover + Cover_dead + Picar + Fallow + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Heter ~ Picar + Zone + Fallow, na.action = na.omit, family = "gaussian", data = e),
  glm( Diver ~ Cover + Cover_dead + Height + Heter + Picar + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( biom ~ Picar + Cover + Cover_dead + Height + Fallow + par + tbl + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( SAI_sd ~ Picar + Cover + Height + Diver + par + tbl + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( LAI_sd ~ Picar + Cover + Height + Diver + par + tbl + Heter + Cover_dead + biom + SAI_sd + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( Pres ~ Picar + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
       + Fallow + par + tbl + Zone, na.action = na.omit, family = "binomial", data = e))


 e.fit3<- sem.fit(e.list3, e) 


#4. Extract coefficients
e.coefs <- sem.coefs(e.list3)

#Remove non logical ones and it doesnt work????????
e.list4 <- list( 
  glm( Cover ~ Picar + Fallow + par + tbl, na.action = na.omit, family = "gaussian", data = e),
  glm( Cover_dead ~ Picar , na.action = na.omit, family = "gaussian", data = e),
  glm( Height ~ Picar + Fallow, na.action = na.omit, family = "gaussian", data = e),
  glm( Heter ~ Picar, na.action = na.omit, family = "gaussian", data = e),
  glm( Diver ~ Cover + Cover_dead + Height + Picar, na.action = na.omit, family = "gaussian", data = e),
  
  glm( biom ~ Picar + Cover + Cover_dead + Fallow + par + tbl, na.action = na.omit, family = "gaussian", data = e),
  glm( SAI_sd ~ Picar + Cover + Height + Diver + par + tbl, na.action = na.omit, family = "gaussian", data = e),
  glm( LAI_sd ~ Picar + Cover + Height + Diver + par + tbl + Heter + biom, na.action = na.omit, family = "gaussian", data = e),
  
  glm( Pres ~ Picar + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
       + Fallow + par + tbl, na.action = na.omit, family = "binomial", data = e))


e.fit4 <- sem.fit(e.list4, e) 


################################### LABRAR ###############################################

e <- f[ , which(colnames(f) %in% c("CF_A", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom","SAI_sd","LAI_sd", "TBL_200", "PAR_200","Fallow_200", 
                                   "Zone", "Tractament"))]

e <-e [-which(duplicated(e[ , 1])), ]

e$Tractament[which(e$Tractament == "Curronar")] <- "Llaurar" #Join Picar y Picar i herbicidar
e <- e[ which(e$Tractament %in% c("Control", "Llaurar")), ] #Select treatment


e <- e %>% 
  unnest(Tractament) %>% 
  mutate(new = 1) %>% 
  spread(Tractament, new, fill = 0) #Create dummy variable for treatment

e$Llaurar <- as.character(e$Llaurar)
length(which(e$Zone == "OCCIDENTAL" & e$Llaurar == "1"))
length(which(e$Zone == "ORIENTAL" & e$Llaurar == "1"))
length(which(e$Zone == "OCCIDENTAL")) # 178 #Possibility to add ZONE?SI
length(which(e$Zone == "ORIENTAL")) #302

e$Zone <- as.character(e$Zone)
e$Zone[which(e$Zone == "OCCIDENTAL")] <- 0
e$Zone[which(e$Zone == "ORIENTAL")] <- 1


colnames(e)[2] <- "Pres"
colnames(e)[3] <- "Cover"
colnames(e)[4] <- "Cover_dead"
colnames(e)[5] <- "Height"
colnames(e)[6] <- "Diver"
colnames(e)[7] <- "Heter"
colnames(e)[8] <- "tbl"
colnames(e)[9] <- "par"
colnames(e)[10] <- "Fallow"
colnames(e)[16] <- "Labrar"

e$Cover<-scale(e$Cover)
e$Height<-scale(e$Height)
e$biom<-scale(e$biom)
e$Cover_dead<-scale(e$Cover_dead)
e$Heter<-scale(e$Heter)
e$Diver<-scale(e$Diver)
e$tbl<-scale(e$tbl)
e$par<-scale(e$par)
e$Fallow<-scale(e$Fallow)

e$Pres[e$Pres > 1] <- 1 # Binomial response

#PATH ANALYSIS

e.list <- list( 
  glm( Cover ~ Labrar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Cover_dead ~ Labrar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Height ~ Labrar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Heter ~ Labrar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Diver ~ Labrar + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( biom ~ Labrar + Fallow + par + tbl + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( SAI_sd ~ Labrar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( LAI_sd ~ Labrar + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
       + Fallow + par + tbl + Zone, na.action = na.omit, family = "binomial", data = e))

e.fit <- sem.fit(e.list, e) 


e.list2 <- list( 
  glm( Cover ~ Labrar + par + Fallow + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Cover_dead ~ Labrar + tbl + Cover + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Height ~ Labrar + Fallow + Cover + Cover_dead + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Heter ~ Labrar + Fallow + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Diver ~ Labrar + par + Cover + Cover_dead + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( biom ~ Labrar + Fallow + par + tbl + Cover + Cover_dead + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( SAI_sd ~ Labrar + par + Cover + Height + Diver + biom + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( LAI_sd ~ Labrar + par + Cover + Height + Diver + biom + SAI_sd + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
       + Fallow + par + tbl + Zone, na.action = na.omit, family = "binomial", data = e))


e.fit2 <- sem.fit(e.list2, e) 

e.coefs <- sem.coefs(e.list2)

################################### ALFALFA ###############################################

e <- f[ , which(colnames(f) %in% c("CF_A", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom","SAI_sd","LAI_sd", "TBL_200", "PAR_200","Fallow_200", 
                                   "Zone", "Tractament"))]

e <-e [-which(duplicated(e[ , 1])), ]

e <- e[ which(e$Tractament %in% c("Control", "Alfals")), ] #Select treatment


e <- e %>% 
  unnest(Tractament) %>% 
  mutate(new = 1) %>% 
  spread(Tractament, new, fill = 0) #Create dummy variable for treatment

e$Llaurar <- as.character(e$Llaurar)
length(which(e$Zone == "OCCIDENTAL" & e$Llaurar == "1"))
length(which(e$Zone == "ORIENTAL" & e$Llaurar == "1"))
length(which(e$Zone == "OCCIDENTAL")) # 178 #Possibility to add ZONE?SI
length(which(e$Zone == "ORIENTAL")) #302

e$Zone <- as.character(e$Zone)
e$Zone[which(e$Zone == "OCCIDENTAL")] <- 0
e$Zone[which(e$Zone == "ORIENTAL")] <- 1


colnames(e)[2] <- "Pres"
colnames(e)[3] <- "Cover"
colnames(e)[4] <- "Cover_dead"
colnames(e)[5] <- "Height"
colnames(e)[6] <- "Diver"
colnames(e)[7] <- "Heter"
colnames(e)[8] <- "tbl"
colnames(e)[9] <- "par"
colnames(e)[10] <- "Fallow"
colnames(e)[15] <- "Alfalfa"

e$Cover<-scale(e$Cover)
e$Height<-scale(e$Height)
e$biom<-scale(e$biom)
e$Cover_dead<-scale(e$Cover_dead)
e$Heter<-scale(e$Heter)
e$Diver<-scale(e$Diver)
e$tbl<-scale(e$tbl)
e$par<-scale(e$par)
e$Fallow<-scale(e$Fallow)

e$Pres[e$Pres > 1] <- 1 # Binomial response

#PATH ANALYSIS

e.list <- list( 
  glm( Cover ~ Alfalfa + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Cover_dead ~ Alfalfa + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Height ~ Alfalfa + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Heter ~ Alfalfa + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Diver ~ Alfalfa + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( biom ~ Alfalfa + Fallow + par + tbl + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( SAI_sd ~ Alfalfa + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( LAI_sd ~ Alfalfa + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
       + Fallow + par + tbl + Zone, na.action = na.omit, family = "binomial", data = e))

e.fit <- sem.fit(e.list, e) 


e.list2 <- list( 
  glm( Cover ~ Alfalfa + Fallow + par + tbl + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Cover_dead ~ Alfalfa + tbl + Cover + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Height ~ Alfalfa + par + Cover + Cover_dead + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Heter ~ Alfalfa + Fallow + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Diver ~ Alfalfa + par + Cover + Cover_dead + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( biom ~ Alfalfa + Fallow + par + tbl + Cover + Cover_dead + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( SAI_sd ~ Alfalfa + par + Cover + Height + Diver + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( LAI_sd ~ Alfalfa + par + Cover + Height + Diver + biom + SAI_sd + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( Pres ~ Alfalfa + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
       + Fallow + par + tbl + Zone, na.action = na.omit, family = "binomial", data = e))

e.fit2 <- sem.fit(e.list2, e) 

e.coefs <- sem.coefs(e.list2)
