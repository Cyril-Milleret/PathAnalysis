
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


# Random intercept Year
e.list1 <- list( 
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

e.fit1 <- sem.fit(e.list1, e) 

# DEFINE BASIS SET WITH RANDOM YEAR

e.list2 <- list( 
  lmer( Cover ~ Treatment + par + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + Cover + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + Cover_dead + Height + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + par + Cover + Cover_dead + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + par + Cover + Cover_dead + Height + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + par + Cover + Height + Diver + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) 
e.coefs2 <- sem.coefs(e.list2,e)

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

# Random intercept Year
e.list1 <- list( 
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

e.fit1 <- sem.fit(e.list1, e) 

# DEFINE BASIS SET WITH RANDOM YEAR

e.list2 <- list( 
  lmer( Cover ~ Treatment + par + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + Cover + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + Cover_dead + Height + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + par + Cover + Cover_dead + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + par + Cover + Cover_dead + Height + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + par + Cover + Height + Diver + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) 
e.coefs2 <- sem.coefs(e.list2,e)

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

# Random intercept Year
e.list1 <- list( 
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

e.fit1 <- sem.fit(e.list1, e) 

# DEFINE BASIS SET WITH RANDOM YEAR

e.list2 <- list( 
  lmer( Cover ~ Treatment + Fallow + par + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + Cover + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + Cover_dead + Height + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + par + Cover + Cover_dead + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + par + Cover + Cover_dead + Height + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + par + Cover + Height + Diver + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) 
e.coefs2 <- sem.coefs(e.list2,e)

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


# Random intercept Year
e.list1 <- list( 
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

e.fit1 <- sem.fit(e.list1, e) 

# DEFINE BASIS SET WITH RANDOM YEAR

e.list2 <- list( 
  lmer( Cover ~ Treatment + Fallow + par + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + Fallow + par + tbl + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + Fallow + Cover + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + Cover_dead + Height + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + par + Cover + Cover_dead + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + par + Cover + Cover_dead + Height + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + par + Cover + Height + Diver + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) 
e.coefs2 <- sem.coefs(e.list2,e)


