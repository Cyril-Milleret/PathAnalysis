
#PATH ANALYSIS ALRUF

library(dplyr)
library(tidyr)
library(piecewiseSEM)
library(nlme)
library(semPlot)

setwd("C:/Users/Ana/Documents/PHD/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Variables.csv", sep = ",", header=TRUE, fill = TRUE)

f <- f[which(f$EspecieObj == "ALRUF"), ]
f[f == 99.00] <- NA
f <- f[-which(duplicated(f[ , 2:15])), ]
f <- f[which(complete.cases(f$Contatge)), ]

################################### PICAR #################################################################### 

e <- f[ , which(colnames(f) %in% c("CF_A", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom","SAI_sd","LAI_sd", "TBL_200", "PAR_200","Fallow_200", 
                                   "Zone", "Tractament"))]

e <-e [-which(duplicated(e[ , 1])), ]
K <- e[which(complete.cases(e)), ]

e$Tractament[which(e$Tractament == "Picar")] <- "Picar i herbicidar" #Join Picar y Picar i herbicidar
e <- e[ which(e$Tractament %in% c("Control", "Picar i herbicidar")), ] #Select treatment
length( which( e$Control == 1 & e$Contatge == 1)) #Sólo dos campos con individuos en Picar...no merece la pena

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

length( which( e$Llaurar == 1 & e$Contatge == 1)) #Sólo 4 campos de labrar

###########################################################################################

# Muy pocos datos. Mirar de nuevo con datos del 2017
