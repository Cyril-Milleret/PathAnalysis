
#PATH ANALYSIS TERAX

library(dplyr)
library(tidyr)
library(piecewiseSEM)
library(nlme)
library(semPlot)

setwd("C:/Users/Ana/Documents/PHD/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Variables.csv", sep = ",", header=TRUE, fill = TRUE)

f <- f[which(f$EspecieObj == "TERAX_M"), ]
f <- f[ which(f$Zone == "ORIENTAL"), ]
f[f == 99.00] <- NA
f <- f[-which(duplicated(f[ , 2:15])), ]
f <- f[which(complete.cases(f$Contatge)), ]



################################### PICAR #################################################################### 

e <- f[ , which(colnames(f) %in% c("CF_A", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom","SAI_sd","LAI_sd", "TBL_200", "PAR_200","Fallow_200", "Tractament"))]

e <-e [-which(duplicated(e[ , 1])), ]
K <- e[which(complete.cases(e)), ]

e$Tractament[which(e$Tractament == "Picar")] <- "Picar i herbicidar" #Join Picar y Picar i herbicidar
e <- e[ which(e$Tractament %in% c("Control", "Picar i herbicidar")), ] #Select treatment


e <- e %>% 
  unnest(Tractament) %>% 
  mutate(new = 1) %>% 
  spread(Tractament, new, fill = 0) #Create dummy variable for treatment


colnames(e)[2] <- "Pres"
colnames(e)[3] <- "Cover"
colnames(e)[4] <- "Cover_dead"
colnames(e)[5] <- "Height"
colnames(e)[6] <- "Diver"
colnames(e)[7] <- "Heter"
colnames(e)[8] <- "tbl"
colnames(e)[9] <- "par"
colnames(e)[10] <- "Fallow"
colnames(e)[15] <- "Picar"

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
  glm( Cover ~ Picar, na.action = na.omit, family = "gaussian", data = e),
  glm( Cover_dead ~ Picar, na.action = na.omit, family = "gaussian", data = e),
  glm( Height ~ Picar, na.action = na.omit, family = "gaussian", data = e),
  glm( Heter ~ Picar, na.action = na.omit, family = "gaussian", data = e),
  glm( Diver ~ Picar, na.action = na.omit, family = "gaussian", data = e),
  
  glm( biom ~ Picar + Fallow + par + tbl, na.action = na.omit, family = "gaussian", data = e),
  glm( SAI_sd ~ Picar, na.action = na.omit, family = "gaussian", data = e),
  glm( LAI_sd ~ Picar, na.action = na.omit, family = "gaussian", data = e),
  
  glm( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
       + Fallow + par + tbl, na.action = na.omit, family = "binomial", data = e))

e.fit <- sem.fit(e.list, e) 



e.list2 <- list( 
  glm( Cover ~ Picar + Fallow + par + Cover, na.action = na.omit, family = "gaussian", data = e),
  glm( Cover_dead ~ Picar + tbl + Cover, na.action = na.omit, family = "gaussian", data = e),
  glm( Height ~ Picar + Fallow + Cover + Cover_dead + par + tbl, na.action = na.omit, family = "gaussian", data = e),
  glm( Heter ~ Picar + Fallow, na.action = na.omit, family = "gaussian", data = e),
  glm( Diver ~ Picar + par + Cover + Cover_dead + tbl, na.action = na.omit, family = "gaussian", data = e),
  
  glm( biom ~ Picar + Fallow + par + tbl + Cover + Cover_dead, na.action = na.omit, family = "gaussian", data = e),
  glm( SAI_sd ~ Picar + par + Cover + Cover_dead + Height + Diver + tbl, na.action = na.omit, family = "gaussian", data = e),
  glm( LAI_sd ~ Picar + par + Cover + Cover_dead + Height + Diver + biom + SAI_sd, na.action = na.omit, family = "gaussian", data = e),
  
  glm( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
       + Fallow + par + tbl, na.action = na.omit, family = "binomial", data = e))

e.fit2 <- sem.fit(e.list2, e)

e.coefs <- sem.coefs(e.list2)

################################### LABRAR ###############################################

e <- f[ , which(colnames(f) %in% c("CF_A", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom","SAI_sd","LAI_sd", "TBL_200", "PAR_200","Fallow_200", "Tractament"))]

e <-e [-which(duplicated(e[ , 1])), ]

e$Tractament[which(e$Tractament == "Curronar")] <- "Llaurar" #Join Picar y Picar i herbicidar
e <- e[ which(e$Tractament %in% c("Control", "Llaurar")), ] #Select treatment


e <- e %>% 
  unnest(Tractament) %>% 
  mutate(new = 1) %>% 
  spread(Tractament, new, fill = 0) #Create dummy variable for treatment


colnames(e)[2] <- "Pres"
colnames(e)[3] <- "Cover"
colnames(e)[4] <- "Cover_dead"
colnames(e)[5] <- "Height"
colnames(e)[6] <- "Diver"
colnames(e)[7] <- "Heter"
colnames(e)[8] <- "tbl"
colnames(e)[9] <- "par"
colnames(e)[10] <- "Fallow"
colnames(e)[15] <- "Labrar"

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

#PATH ANALYSIS

e.list <- list( 
  glm( Cover ~ Labrar, na.action = na.omit, family = "gaussian", data = e),
  glm( Cover_dead ~ Labrar, na.action = na.omit, family = "gaussian", data = e),
  glm( Height ~ Labrar, na.action = na.omit, family = "gaussian", data = e),
  glm( Heter ~ Labrar, na.action = na.omit, family = "gaussian", data = e),
  glm( Diver ~ Labrar, na.action = na.omit, family = "gaussian", data = e),
  
  glm( biom ~ Labrar + Fallow + par + tbl, na.action = na.omit, family = "gaussian", data = e),
  glm( SAI_sd ~ Labrar, na.action = na.omit, family = "gaussian", data = e),
  glm( LAI_sd ~ Labrar, na.action = na.omit, family = "gaussian", data = e),
  
  glm( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
       + Fallow + par + tbl, na.action = na.omit, family = "binomial", data = e))

e.fit <- sem.fit(e.list, e)



e.list2 <- list( 
  glm( Cover ~ Labrar + Fallow + par, na.action = na.omit, family = "gaussian", data = e),
  glm( Cover_dead ~ Labrar + tbl + Cover + par, na.action = na.omit, family = "gaussian", data = e),
  glm( Height ~ Labrar + Fallow + Cover + Cover_dead + par, na.action = na.omit, family = "gaussian", data = e),
  glm( Heter ~ Labrar + Fallow, na.action = na.omit, family = "gaussian", data = e),
  glm( Diver ~ Labrar + par + Cover + Cover_dead + tbl, na.action = na.omit, family = "gaussian", data = e),
  
  glm( biom ~ Labrar + Fallow + par + tbl + Cover + Cover_dead, na.action = na.omit, family = "gaussian", data = e),
  glm( SAI_sd ~ Labrar + par + Cover + Cover_dead + Height + Diver + tbl, na.action = na.omit, family = "gaussian", data = e),
  glm( LAI_sd ~ Labrar + par + Cover + Cover_dead + Height + Diver + biom + SAI_sd, na.action = na.omit, family = "gaussian", data = e),
  
  glm( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
       + Fallow + par + tbl, na.action = na.omit, family = "binomial", data = e))

e.fit2 <- sem.fit(e.list2, e)  

e.coefs <- sem.coefs(e.list2)


################################### ALFALFA ###############################################

e <- f[ , which(colnames(f) %in% c("CF_A", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom","SAI_sd","LAI_sd", "TBL_200", "PAR_200","Fallow_200", "Tractament"))]

e <-e [-which(duplicated(e[ , 1])), ]

e <- e[ which(e$Tractament %in% c("Control", "Alfals")), ] #Select treatment


e <- e %>% 
  unnest(Tractament) %>% 
  mutate(new = 1) %>% 
  spread(Tractament, new, fill = 0) #Create dummy variable for treatment



colnames(e)[2] <- "Pres"
colnames(e)[3] <- "Cover"
colnames(e)[4] <- "Cover_dead"
colnames(e)[5] <- "Height"
colnames(e)[6] <- "Diver"
colnames(e)[7] <- "Heter"
colnames(e)[8] <- "tbl"
colnames(e)[9] <- "par"
colnames(e)[10] <- "Fallow"
colnames(e)[14] <- "Alfalfa"

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
  glm( Cover ~ Alfalfa, na.action = na.omit, family = "gaussian", data = e),
  glm( Cover_dead ~ Alfalfa, na.action = na.omit, family = "gaussian", data = e),
  glm( Height ~ Alfalfa, na.action = na.omit, family = "gaussian", data = e),
  glm( Heter ~ Alfalfa, na.action = na.omit, family = "gaussian", data = e),
  glm( Diver ~ Alfalfa, na.action = na.omit, family = "gaussian", data = e),
  
  glm( biom ~ Alfalfa + Fallow + par + tbl, na.action = na.omit, family = "gaussian", data = e),
  glm( SAI_sd ~ Alfalfa, na.action = na.omit, family = "gaussian", data = e),
  glm( LAI_sd ~ Alfalfa, na.action = na.omit, family = "gaussian", data = e),
  
  glm( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
       + Fallow + par + tbl, na.action = na.omit, family = "binomial", data = e))

e.fit <- sem.fit(e.list, e)


e.list2 <- list( 
  glm( Cover ~ Alfalfa + Fallow + par + tbl, na.action = na.omit, family = "gaussian", data = e),
  glm( Cover_dead ~ Alfalfa + tbl + Cover + par, na.action = na.omit, family = "gaussian", data = e),
  glm( Height ~ Alfalfa + Fallow + par + tbl + Cover + Cover_dead, na.action = na.omit, family = "gaussian", data = e),
  glm( Heter ~ Alfalfa + Fallow, na.action = na.omit, family = "gaussian", data = e),
  glm( Diver ~ Alfalfa + par + Cover + Cover_dead + tbl, na.action = na.omit, family = "gaussian", data = e),
  
  glm( biom ~ Alfalfa + Fallow + par + tbl + Cover, na.action = na.omit, family = "gaussian", data = e),
  glm( SAI_sd ~ Alfalfa + par + Cover + Cover_dead + Height + Diver + tbl, na.action = na.omit, family = "gaussian", data = e),
  glm( LAI_sd ~ Alfalfa + par + Cover + Cover_dead + Height + Diver + biom + SAI_sd, na.action = na.omit, family = "gaussian", data = e),
  
  glm( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
       + Fallow + par + tbl, na.action = na.omit, family = "binomial", data = e))

e.fit2 <- sem.fit(e.list2, e)

e.coefs <- sem.coefs(e.list2)


