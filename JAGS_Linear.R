# Model test of linear regression from JAGS.pdf
# Written and tested by Jill Arriola - Dec. 2020
# Linear Model as used in the Linear_JAGS_model.txt file
#
# A basic linear regression model:
# y = mx + b
# Where:
# y = predicted value
# b = y-intercept
# m = slope of the line
# x = independent variable
#
# But here:
# y = b + mx
# is represented as:
# mu = alpha + beta*x[i]
#
# From JAGS.pdf
# model {
#  for (i in 1:N) {
#   y[i] ~ dnorm(mu[i], tau)
#   mu[i] <- alpha + beta * (x[i])
#   }
# alpha ~ dnorm(0.0, 1.0E-4)
# beta ~ dnorm(0.0, 1.0E-4)
# sigma <- 1.0/sqrt(tau)
# tau ~ dgamma(1.0E-3, 1.0E-3)
# }


# Load R2jags and Lattice 
library(R2jags)
library(lattice)

setwd('C:/Users/Jill Arriola/OneDrive - The Pennsylvania State University/Documents')

# Set parameters (taken from example in JAGS.pdf and bayesmetab.R)
# x is "cats"
# y is "hairballs"
line_data <- list("cats" = c(1, 2, 3, 4, 5),
                  "hairballs" = c(1, 3, 3, 3, 5),
                  "N" = 5)
inits <- list(list("alpha" = 3, "beta"= 0, "tau" = 1.5), #3,0,1.5
             list("alpha" = 0, "beta" = 1, "tau" = 0.375)) #0,1,0.375
n.iter <- 2000
n.chains <- 2 # Must match # of nodes to run... still don't know what that means, but 2 works in this example.
n.burnin <- n.iter/2
n.thin <- 10 # Got from bayesmetab.R
params <- c("alpha","beta","sigma")

model_test <- jags(data = line_data, 
                         inits = inits, 
                         parameters.to.save = params, 
                         model.file = "Linear_JAGS_model.txt",
                         n.chains = n.chains,
                         n.iter = n.iter,
                         n.burnin = n.burnin,
                         n.thin = n.thin, 
                         DIC = TRUE,
                         jags.seed = 123, 
                         digits=5)
  
# to see output
print(model_test)
model_test_results <- write.csv(model_test$BUGSoutput$summary, file = "model_test_results.csv")
  
# a really bad plot of results
plot(model_test)
  
# better plots to see convergence
# need to hit enter in Console to continue
traceplot(model_test)
          
# use as.mcmc to convert rjags object into mcmc.list
jagsfit.mcmc <- as.mcmc(model_test)
  
# Use the plotting methods from coda - just like results from bayesmetab.R
xyplot(jagsfit.mcmc)
densityplot(jagsfit.mcmc)



## Check outputs of linear regression using function lm
## Outputs y-intercept and slope as the name of the x variable ("cats" in this example)
linear <- lm(hairballs~cats, data = line_data)
plot(line_data$cats, line_data$hairballs)
summary(linear)

linear_results <- write.csv(as.data.frame(summary(linear)$coef), file="linear_results.csv")
