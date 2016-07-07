library(ggplot2)
library(rstan)
library(dplyr)
load("GandL_Simulate.rda")

loaded <- ls()

loaded <- loaded[loaded != "dfLag2"]

rm(list = loaded)

linkRatioShape <- 30
linkRatioRate <- 20
pltLinkRatioPrior <- ggplot(data.frame(x = c(0, 5)), aes(x))
pltLinkRatioPrior <- pltLinkRatioPrior + stat_function(fun = dgamma
                                             , args = list(shape = linkRatioShape, rate = linkRatioRate)
                                             , geom = "line")
pltLinkRatioPrior <- pltLinkRatioPrior + xlim(1, 2.2)
pltLinkRatioPrior

sampleIndex <- sample(seq.int(nrow(dfLag2)), size = 20)
Prior <- dfLag2$Prior[sampleIndex]
Current <- dfLag2$ClaimInt[sampleIndex]
BadCredit <- ifelse(dfLag2$Credit[sampleIndex] == "Bad", 1, 0)
sampleLinks <- Current / Prior
summary(sampleLinks)

# dfNewPrior <- dfLower %>% 
#   filter(Lag == 2)
# 
# numNewPrior <- nrow(dfNewPrior)
# newPrior <- dfNewPrior$PriorInt
# 
# modelMat <- model.matrix(ClaimInt ~ Credit, data=dfLag2)

fitPois <- stan(file = './stan/Claims.stan'
                 , data = list(numClaims = length(Prior)
                               , Current
                               , Prior
                               , BadCredit
                               , shape = linkRatioShape
                               , rate = linkRatioRate
                               , NewPrior = rep(1000, 2)
                               , numNewClaims = 2
                               , NewBadCredit = c(0,1))
                 , iter = 1000
                 , seed = 1234)

linkRatio <- extract(fitPois, 'linkRatio') %>% unlist()
logLink <- extract(fitPois, 'logLink') %>% unlist()
credit <- extract(fitPois, 'credit') %>% unlist()
badCreditLinkRatio <- exp(logLink + credit)

summary(linkRatio)
summary(exp(credit))
summary(badCreditLinkRatio)

pltLinkRatioPost <- ggplot(as.data.frame(linkRatio), aes(linkRatio)) + geom_density(fill = "grey")
pltLinkRatioPost <- pltLinkRatioPost + xlim(1, 2.2)
pltLinkRatioPost

pltBadCreditPost <- ggplot(as.data.frame(badCreditLinkRatio), aes(badCreditLinkRatio)) + geom_density(fill = "grey")
pltBadCreditPost <- pltBadCreditPost + xlim(1, 2.2)
pltBadCreditPost

lstPrediction <- extract(fitPois, "newCurrent")
dfPredictedClaims <- lstPrediction$newCurrent %>% as.data.frame()
names(dfPredictedClaims) <- c("Good Credit", "Bad Credit")
dfPredictedClaims <- dfPredictedClaims %>% 
  tidyr::gather(Credit, Prediction)
 
pltClaimPrediction <- ggplot(dfPredictedClaims, aes(Prediction, fill = Credit)) + geom_density()
pltClaimPrediction <- pltClaimPrediction + scale_x_continuous(labels = scales::dollar)
pltClaimPrediction <- pltClaimPrediction + ggtitle("Lag 2 predictions for a single claim worth $1,000")
pltClaimPrediction

save(file = "Claims.rda"
     , pltLinkRatioPrior
     , pltLinkRatioPost
     , pltBadCreditPost
     , pltClaimPrediction)