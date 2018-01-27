

#PATH ANALYSIS BUOED

library(dplyr)
library(tidyr)
library(piecewiseSEM)
library(nlme)
library(semPlot)

setwd("C:/Users/Ana/Documents/PHD/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Variables.csv", sep = ",", header=TRUE, fill = TRUE)

f <- f[which(f$EspecieObj == "BUOED"), ]
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

#Check correlations
g <- e[ ,c(3:10, 12:14)]
cor(g, use = "complete.obs")


#PATH ANALYSIS

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


e.basis.set <- sem.basis.set(e.list)

e.fit <- sem.fit(e.list, e) 

# Include missing paths

e.list2 <- list( 
  glm( Cover ~ Picar + Fallow + par + tbl + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Cover_dead ~ Picar + tbl + Cover + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Height ~ Picar + Fallow + Cover + Cover_dead + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Heter ~ Picar + Fallow + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Diver ~ Picar + par + Cover + Cover_dead + Height + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( biom ~ Picar + Fallow + par + tbl + Cover + Cover_dead + Height + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( SAI_sd ~ Picar + par + Cover + Height + Diver + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( LAI_sd ~ Picar + par + Cover + Height + Diver + biom + SAI_sd + Cover_dead + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
       + Fallow + par + tbl + Zone, na.action = na.omit, family = "binomial", data = e))

e.fit2 <- sem.fit(e.list2, e) 

e.coefs <- sem.coefs(e.list2)

#PLAY

e.list9 <- list( 
  glm( Cover ~ Picar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Cover_dead ~ Picar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Height ~ Picar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Heter ~ Picar + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Diver ~ Picar + Cover + Cover_dead + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( biom ~ Picar + Fallow + par + tbl + Cover + Cover_dead + Height + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( SAI_sd ~ Picar + par + Cover + Height + Diver + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( LAI_sd ~ Picar + par + Cover + Height + Diver + biom + SAI_sd + Cover_dead + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
       + Fallow + par + tbl + Zone, na.action = na.omit, family = "binomial", data = e))

e.fit9 <- sem.fit(e.list9, e) 

e.coefs <- sem.coefs(e.list9)




# Plot
x <- semPlotModel(sem.lavaan(e.list2, e)) #Model in laavan

semPaths(x, layout = "tree2", rotation = 2)

#3. Create semPlotModel class with estimates from piecewiseSEM
Pars <- data.frame (
  label = x@Pars$label,
  lhs = x@Pars$lhs,
  edge = x@Pars$edge,
  rhs = x@Pars$rhs,
  est = c(e.coefs$estimate, rep(0, times = 24)), # From piecewise
  std = c(e.coefs$std.error, rep(0, times = 24)), # From piecewise
  group = x@Pars$group,
  fixed = FALSE,
  par = x@Pars$par)

h <- new( "semPlotModel",
          Pars = Pars, # Contains the good estimates
          Vars = x@Vars, 
          Thresholds = x@Thresholds,
          Computed = x@Computed,
          ObsCovs = x@ObsCovs,
          ImpCovs = x@ImpCovs,
          Original = x@Original)


semPaths(h, what = "est", whatLabels = "est", style = "ram", residuals = FALSE, # No circular arrow
         layout = "tree2", rotation = 2, nCharNodes = 0, curvature = 5,
         sizeMan = 8, sizeMan2 = 5,                                             
         shapeMan = "rectangle", freeStyle = c("darkgrey",1),
         edge.label.cex = 1, edge.width = 0.4, levels = c(100,5,6,7))



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

#Include paths with sense

e.list2 <- list( 
  glm( Cover ~ Labrar + par + Fallow + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Cover_dead ~ Labrar + tbl + Cover + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Height ~ Labrar + Fallow + Cover + Cover_dead + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Heter ~ Labrar + Fallow + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( Diver ~ Labrar + par + Cover + Cover_dead + Zone, na.action = na.omit, family = "gaussian", data = e),
  
  glm( biom ~ Labrar + Fallow + par + tbl + Cover + Cover_dead + Height + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( SAI_sd ~ Labrar + Cover + Height + Diver + biom + par + Zone, na.action = na.omit, family = "gaussian", data = e),
  glm( LAI_sd ~ Labrar + Cover + Height + Diver + biom + SAI_sd + Cover_dead + Zone, na.action = na.omit, family = "gaussian", data = e),
  
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
  
  glm( Pres ~ Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
       + Fallow + par + tbl + Zone, na.action = na.omit, family = "binomial", data = e))

e.fit2 <- sem.fit(e.list2, e)
e.coefs <- sem.coefs(e.list2)


##############################################################
############# PLOT EXAMPLE ##################################

# 1. Define the model in piecewiseSEM
elist <- list(                           
  lm(Height ~ Picar, data = e),
  lm(biom ~ Picar, data = e),
  lm(Pres ~ Height + biom, data = e))
e.fit <- sem.fit(elist, e) 
e.coefs <- sem.coefs(elist) 

#2. Define the model same in lavaan
lav<- sem.lavaan(elist,e)
x <- semPlotModel(lav)
semPaths(x, layout = "tree2", rotation = 2)

#3. Create semPlotModel class with estimates from piecewiseSEM
Pars <- data.frame (
  label = x@Pars$label,
  lhs = x@Pars$lhs,
  edge = x@Pars$edge,
  rhs = x@Pars$rhs,
  est = e.coefs$estimate, # From piecewise
  std = e.coefs$std.error, # From piecewise
  group = x@Pars$group,
  fixed = FALSE,
  par = x@Pars$par)

h <- new( "semPlotModel",
          Pars = Pars, # Contains the good estimates
          Vars = x@Vars, 
          Thresholds = x@Thresholds,
          Computed = x@Computed,
          ObsCovs = x@ObsCovs,
          ImpCovs = x@ImpCovs,
          Original = x@Original)


semPaths(h, what = "est", whatLabels = "est", style = "ram", residuals = FALSE, # No circular arrow
         layout = "tree2", intStyle = "single", rotation = 2, nCharNodes = 0,
         sizeMan = 8, sizeMan2 = 5,                                             # Wigth and height of variable boxes
         shapeMan = "rectangle", freeStyle = c("darkgrey",1),
         edge.label.cex = 1, edge.width = 0.4)




