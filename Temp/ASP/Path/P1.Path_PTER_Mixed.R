
#PATH ANALYSIS PTEROCLIDOS

library(dplyr)
library(tidyr)
library(piecewiseSEM)
library(nlme)
library(semPlot)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Variables.csv", sep = ",", header=TRUE, fill = TRUE)

f <- f[which(f$EspecieObj == "PTALC"), ]
f <- f[ which(f$Zone == "OCCIDENTAL"), ]
f[f == 99.00] <- NA
f[f == 198.00] <- NA
f <- f[-which(duplicated(f[ , 2:15])), ]
f <- f[which(complete.cases(f$Cont_pt)), ]


################################### PICAR Y HERBICIDAR #################################################################### 

e <- f[ , which(colnames(f) %in% c("CF_A", "Cont_pt", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
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

#Check correlations
g <- e[ ,c(3:10, 12:14)]
cor(g, use = "complete.obs")


#PATH ANALYSIS
e <- na.omit(e)

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
  lmer( Cover ~ Treatment + Irrig + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + Fallow + Cover + Height + Heter + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + Fallow + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + Height + par + Cover + Cover_dead + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + Fallow + tbl + Height + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + Fallow + Cover_dead + Height + Diver + par + Cover + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) 
e.coefs2 <- sem.coefs(e.list2,e)

setwd("~/Path analysis/Path_species")
pdf(file = "Pter_PH.pdf")
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
         ,Treatment.name= "P + H"
         ,Species.name="PRESENCE \n PTEROCLID"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(2, 1),
         cex.estimate = 0.7,
         digits.estimate = 2)
dev.off()

####################################### PICAR #############################################

e <- f[ , which(colnames(f) %in% c("CF_A", "Cont_pt", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
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

#DEFINE BASIS SET WITH RANDOM YEAR

e.list2 <- list( 
  lmer( Cover ~ Treatment + Irrig + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + Fallow + Irrig + Cover + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + Fallow + Cover + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + Cover + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + Cover + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + Height + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + Cover + Cover_dead + Height + Diver + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e)
e.coefs2 <- sem.coefs(e.list2,e)

setwd("~/Path analysis/Path_species")
pdf(file = "Pter_P.pdf")
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
         ,Treatment.name= "PICAR"
         ,Species.name="PRESENCE \n PTEROC"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(2, 1),
         cex.estimate = 0.7,
         digits.estimate = 2)
dev.off()

################################### LABRAR ###############################################

e <- f[ , which(colnames(f) %in% c("CF_A", "Cont_pt", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
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
  lmer( Cover ~ Treatment + Irrig + (1|Year), na.action = na.omit, data = e),
  lmer( Cover_dead ~ Treatment + Fallow + Cover + (1|Year), na.action = na.omit, data = e),
  lmer( Height ~ Treatment + Fallow + Cover + (1|Year), na.action = na.omit, data = e),
  lmer( Heter ~ Treatment + Cover + (1|Year), na.action = na.omit, data = e),
  lmer( Diver ~ Treatment + Irrig + Cover + (1|Year), na.action = na.omit, data = e),
  
  lmer( biom ~ Treatment + Fallow + Irrig + par + tbl + Cover + Height + Cover_dead 
        + Heter + Diver + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( SAI_sd ~ Treatment + Height + LAI_sd + (1|Year), na.action = na.omit, data = e),
  lmer( LAI_sd ~ Treatment + Fallow + Cover + Cover_dead + Height + Diver + (1|Year), na.action = na.omit, data = e),
  
  glmer( Pres ~ Treatment + Cover + Height + Cover_dead + Heter + Diver + biom + SAI_sd + LAI_sd
         + Fallow + Irrig + par + tbl + (1|Year), na.action = na.omit, family = "binomial"(link = "logit"), data = e))

e.fit2 <- sem.fit(e.list2, e) 

e.coefs2 <- sem.coefs(e.list2,e)

setwd("~/Path analysis/Path_species")
pdf(file = "Pter_L.pdf")
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
         ,Treatment.name= "LABRAR"
         ,Species.name="PRESENCE \n PTEROC"
         ,cex.category = 0.5
         ,plot.axis=FALSE
         ,estimate.box.width=c(2, 1),
         cex.estimate = 0.7,
         digits.estimate = 2)
dev.off()

