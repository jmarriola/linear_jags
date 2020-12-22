# R code: rjags test
# Bayesian Data Analysis
library(rjags)
library(R2jags)
library(lattice)
# An example model file is given in: /usr/local/lib/R/library/R2jags/model
# the plotted graphic is shown in your current directory by default, named Rplots.pdf
# load the file (Copy it to your R working directory if necessary) model.file <- system.file(package="R2jags", "model", "schools.txt")
# Let's take a look:
model.file <- system.file(package="R2jags", "model", "schools.txt")
file.show(model.file)
# data
J <- 8.0
y <- c(28.4,7.9,-2.8,6.8,-0.6,0.6,18.0,12.2)
sd <- c(14.9,10.2,16.3,11.0,9.4,11.4,10.4,17.6)
jags.data <- list("y","sd","J")
jags.params <- c("mu","sigma","theta")
jags.inits <- function(){ list("mu"=rnorm(1),"sigma"=runif(1),"theta"=rnorm(J))
}
#=============#
# using jags #
#=============#
jagsfit <- jags(data=jags.data, inits=jags.inits, jags.params,
                n.iter=5000, model.file=model.file)
# display the output
print(jagsfit)
plot(jagsfit)

# traceplot
traceplot(jagsfit)

# or to use some plots in coda
# use as.mcmc to convert rjags object into mcmc.list
jagsfit.mcmc <- as.mcmc(jagsfit)
## now we can use the plotting methods from coda
xyplot(jagsfit.mcmc)
densityplot(jagsfit.mcmc)

# if the model does not converge, update it!
jagsfit.upd <- update(jagsfit, n.iter=1000)
print(jagsfit.upd)
plot(jagsfit.upd)

# or auto update it until it converges! see ?autojags for details
jagsfit.upd <- autojags(jagsfit)
# to get DIC or specify DIC=TRUE in jags() or do the following
dic.samples(jagsfit.upd$model, n.iter=1000, type="pD")

# attach jags object into search path see "attach.bugs" for details
attach.jags(jagsfit.upd)

# this will show a 3-way array of the bugs.sim object, for example: mu

# detach jags object into search path see "attach.bugs" for details
detach.jags()
# to pick up the last save session
# for example, load("RWorkspace.Rdata")
recompile(jagsfit)
jagsfit.upd <- update(jagsfit)