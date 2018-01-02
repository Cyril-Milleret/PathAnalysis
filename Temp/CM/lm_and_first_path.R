###  SIMULATE  AND ANALYZE DATA DATA FOR A SIMPLE PATH ANALYSIS
rm(list=ls())
#load library
library(jagsUI)


## Define parameters 
N <- 1000     # sample size 
beta0 <- 0
beta1 <- 2
beta2 <- -2
sdError <- 2
error <- rnorm(N, mean=0, sd=sdError)

## Create some variables 
x1 <- rnorm(N, mean=0, sd=1)
x2 <- rnorm(N, mean=0, sd=1)

## apply the linear model 
y <- beta0 + beta1*x1 + beta2*x2 + error





### ==== 1.JAGS MODEL DEFINITION ====
setwd("C:/My_documents/ana")
sink("path.jags")
cat("model {
##------------------------------------------------------------------------------------------------------------------
##-----------------------------------------##
##----------    PRIORS           ----------##
##-----------------------------------------##

tau <- 1/(sdeps*sdeps)
sdeps ~ dunif(0,100)
    
beta0 ~ dunif(-10,10) 
beta1 ~ dunif(-10,10) 
beta2 ~ dunif(-10,10) 

# LINEAR MODEL 

for( i in 1:N){
  Y[i] ~ dnorm(muY[i] ,tau)
  muY[i] <- beta0 + beta1 * x1[i] + beta2*x2[i]
}#i
    }",fill = TRUE)
sink()

## data for jags
my.jags.input <- list(   x1 = x1
                       , x2 = x2
                       ,  Y = y
                       , N=length(x1) )

# parameters for to monitor 
params <- c("beta0", "beta1", "beta2")

## run the model 
my.jags.output <- jags( data = my.jags.input
                        , inits = NULL
                        , parameters.to.save = params
                        , model.file = "path.jags"
                        , n.chains = 3
                        , n.adapt = 500
                        , n.iter = 1000
                        , n.burnin = 0
                        , n.thin = 3
                        , parallel = TRUE, DIC = FALSE, bugs.format = TRUE)
##
plot(my.jags.output$samples)

## try the linear model with lm 
mod <- lm(y ~ x1 + x2 )
summary(mod)

## both are very close ###
coef(mod)
my.jags.output$mean




##### NOW LET'S SIMULATE DATA TO MIMIC A PATH ANALYSIS 

beta0.1 <- 0
beta1.1 <- 5
beta2.1 <- -5
error1 <- rnorm(N, mean=0, sd=10)


y1 <- beta0.1 + beta1.1*x1 + beta2.1 * y + error1 ## this the response 



### now the corresponding jags model 
setwd("C:/My_documents/ana")
sink("path1.jags")
cat("model {
    ##------------------------------------------------------------------------------------------------------------------
    ##-----------------------------------------##
    ##----------    PRIORS           ----------##
    ##-----------------------------------------##
    tau <- 1/(sdeps * sdeps)
    sdeps ~ dunif(0,100)
   
    tau.1 <- 1/(sdeps.1 * sdeps.1)
    sdeps.1 ~ dunif(0,100)    

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

      Y[i] ~ dnorm(muY[i] ,tau)
      muY[i] <- beta0 + beta1 * x1[i] + beta2*x2[i]

      Y1[i]~ dnorm(muY1[i] ,tau.1)
      muY1[i] <- beta0.1 + beta1.1 * x1[i] + beta2.1 * muY[i]

      }#i
    }",fill = TRUE)
sink()

## data for jags
my.jags.input <- list(   x1 = x1
                       , x2 = x2
                       ,  Y = y
                       , Y1 = y1
                       , N = length(x1) )

# parameters for to monitor 
params <- c( "beta0", "beta1", "beta2"
            ,"beta0.1", "beta1.1", "beta2.1")

## run the model 
my.jags.output <- jags( data = my.jags.input
                        , inits = NULL
                        , parameters.to.save = params
                        , model.file = "path1.jags"
                        , n.chains = 3
                        , n.adapt = 500
                        , n.iter = 1000
                        , n.burnin = 0
                        , n.thin = 3
                        , parallel = TRUE
                        , DIC = FALSE
                        , bugs.format = TRUE)
##
plot(my.jags.output$samples)
## compare to true values 
unlist(my.jags.output$mean)
c( beta0, beta1, beta2,
   beta0.1, beta1.1, beta2.1)









###########################

#####
x3 <- rnorm(1000, mean=0, sd=1)

y1 <- beta0.1 + beta1.1* y.pred + beta2.1* x3 + error1


mod1 <- lm(y1 ~ x3 + y.pred )

summary(mod1)



#######