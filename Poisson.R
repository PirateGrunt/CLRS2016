library(ggplot2)
library(rstan)
library(dplyr)

numClaims <- 500

Current <- rpois(numClaims, 50)

shape <- 8
rate <- 1/10
pltGamma <- ggplot(data.frame(x = c(0, 100)), aes(x))
pltGamma <- pltGamma + stat_function(fun = dgamma
                                     , args = list(shape = shape, rate = rate)
                                     , geom = "line")
pltGamma

fitPois1 <- stan(file = './stan/Poisson1.stan'
                 , data = list(numClaims, Current, shape = shape, rate = rate)
                 , iter = 1000
                 , seed = 1234)

lambda <- extract(fitPois1, 'lambda') %>% unlist()

pltLambda <- ggplot(as.data.frame(lambda), aes(lambda)) + geom_density(fill = "grey")
pltLambda

mean(lambda1)

#=====================================
# Same again, but with an exposure (i.e. offset) term
#=====================================
set.seed(1234)
Prior <- as.integer(rlnorm(numClaims, meanlog = 8, sdlog = 1.3))
links <- 1.5
Current <- rpois(numClaims, as.integer(Prior * links))
sampleLinks <- Current / Prior

numNewPrior <- 50
newPrior <- as.integer(rlnorm(numNewPrior, meanlog = 8, sdlog = 1.3))

summary(sampleLinks)

pltSampleLinks <- ggplot(data.frame(sampleLinks), aes(sampleLinks)) + geom_density(fill = "grey")
pltSampleLinks

sum(Current) / sum(Prior)

betaShape <- 2
betaRate <- 2
pltBetaPrior <- ggplot(data.frame(x = c(0, 5)), aes(x))
pltBetaPrior <- pltBetaPrior + stat_function(fun = dgamma
                                             , args = list(shape = betaShape, rate = betaRate)
                                             , geom = "line")
pltBetaPrior

fitPois2 <- stan(file = './stan/Poisson2.stan'
                 , data = list(numClaims, Current, Prior, shape = betaShape, rate = betaRate, NewPrior = newPrior, numNewPrior)
                 , iter = 1000
                 , seed = 1234)

linkRatio <- extract(fitPois2, 'linkRatio') %>% unlist()

pltLinkRatio <- ggplot(as.data.frame(linkRatio), aes(linkRatio)) + geom_density(fill = "grey")
pltLinkRatio

summary(linkRatio)

lstPrediction <- extract(fitPois2, "newCurrent")
mojo <- lstPrediction$newCurrent

newPrior[1] * mean(linkRatio)

pltNewCurrent1 <- ggplot(data.frame(x = mojo[, 1]), aes(x)) + geom_density(fill = "grey")
pltNewCurrent1
