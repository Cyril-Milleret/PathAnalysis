#  SIMULATE  AND ANALYZE DATA DATA FOR A SIMPLE PATH ANALYSIS ####
rm(list=ls())
#load library
library(jagsUI)
library(sp)

setwd("C:/My_documents/ana")## set working directory


## Define parameters 
N <- 1000  # sample size 
beta0 <- 0 #intercept 
beta1 <- 2 # effect of climate on vegetation
beta2 <- -2 # effect of altitude on vegetation
sdError <- 5 # sd error
error <- rnorm(N, mean=0, sd=sdError)

## Simualte some variables 
climate <- rnorm(N, mean=0, sd=1)
altitude <- rnorm(N, mean=0, sd=1)

## simulate the direct effect of climate + altitude on vegetation quality
vegetation.quality <- beta0 + beta1*climate + beta2*altitude + error


## direct effect of climate + indirect effect of climate and altitude (through vegetation quality) on body mass 
beta0.1 <- 0 #intercept 
beta1.1 <- 5 # direct effect of climate on body mass
beta2.1 <- -5 # effect of vegetation quality on body mass.
error1 <- rnorm(N, mean=0, sd=10)


bodymass <- beta0.1 + beta1.1*climate + beta2.1 * vegetation.quality + error1 



### now the corresponding jags model 
sink("path.jags")
cat("model {
    ##------------------------------------------------------------------------------------------------------------------
    ##-----------------------------------------##
    ##----------    PRIORS           ----------##
    ##-----------------------------------------##
    tau.vegetation.quality <- 1/(sd.vegetation.quality * sd.vegetation.quality)
    sd.vegetation.quality ~ dunif(0,100)
    
    tau.bodymass <- 1/(sd.tau.bodymass * sd.tau.bodymass)
    sd.tau.bodymass ~ dunif(0,100)    
    
    beta0 ~ dunif(-10,10) 
    beta1 ~ dunif(-10,10) 
    beta2 ~ dunif(-10,10) 
    
    beta0.1 ~ dunif(-10,10) 
    beta1.1 ~ dunif(-10,10) 
    beta2.1 ~ dunif(-10,10) 
    
    ##------------------------------------------------------------------------------------------------------------------
    ##-----------------------------------------##
    ##----------    PATH MODEL       ----------##
    ##-----------------------------------------##
    for( i in 1:N){
    
    # Direct effect of climate and altitude on vegetation quality
    vegetation.quality[i] ~ dnorm(mu.vegetation.quality[i] ,tau.vegetation.quality)
    mu.vegetation.quality[i] <- beta0 + beta1 * climate[i] + beta2*altitude[i]
    
    # Direct effect of climate and indirect effect of  climate and altitude through vegetation quality on body mass
    bodymass[i]~ dnorm(mu.bodymass[i] ,tau.bodymass)
    mu.bodymass[i] <- beta0.1 + beta1.1 * climate[i] + beta2.1 * mu.vegetation.quality[i]
    
    }#i
    }",fill = TRUE)
sink()

## data for jags
  my.jags.input <- list(   climate = climate
                         , altitude = altitude
                         , vegetation.quality = vegetation.quality
                         , bodymass = bodymass
                         , N = N )

# parameters for to monitor 
params <- c( "beta0", "beta1", "beta2"
             ,"beta0.1", "beta1.1", "beta2.1")

## run the model 
my.jags.output <- jags( data = my.jags.input
                        , inits = NULL
                        , parameters.to.save = params
                        , model.file = "path.jags"
                        , n.chains = 3
                        , n.adapt = 500
                        , n.iter = 1000
                        , n.burnin = 0
                        , n.thin = 2
                        , parallel = TRUE
                        , DIC = FALSE
                        , bugs.format = TRUE)
##plot chains
plot(my.jags.output$samples)

## compare to simulated values  values 
unlist(my.jags.output$mean)
c( beta0, beta1, beta2,
   beta0.1, beta1.1, beta2.1)



