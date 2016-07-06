library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(ChainLadder)

load("GandL_Simulate.rda")

#===========================
# Poisson GLM prediction
#===========================
dfClaims$Prediction <- ifelse(dfClaims$Upper, NA, 0)
# dfClaims <- dfClaims %>% 
#   arrange(ClaimNumber, Lag)

fits <- vector("list", length(badLink))

numLinks <- length(badLink)

dfClaimsTri <- dfClaims[] %>% 
  group_by(PolicyYear, Lag) %>% 
  summarise(Claims = sum(ClaimValue)) %>% 
  tidyr::spread(Lag, Claims)

for (i in seq_along(badLink)){
  
  sampleData <- with(dfClaims, which(Lag == lags[i + 1] 
                                     & PolicyYear %in% policyYears[1:(10-i)]))
  
  fits[[i]] <- glm(ClaimInt ~ Credit
                   , family = poisson(link="log")
                   , offset = log(PriorInt)
                   , data = dfClaims[sampleData, ])
  
  predictionData <- with(dfClaims, which(Lag == lags[i+1] 
                                         & PolicyYear %in% policyYears[(10-i+1):10]))
  
  dfClaims$Prediction[predictionData] <- predict(fits[[i]]
                                                 , newdata = dfClaims[predictionData, ]
                                                 , type = "response")
  newPredict <- predictionData + 1
  newPredict <- subset(newPredict, newPredict <= nrow(dfClaims))
  dfClaims$Prior[newPredict] <- dfClaims$Prediction[predictionData[1:length(newPredict)]]
}
rm(sampleData, predictionData, i, newPredict)

dfClaims$AbsoluteError <- with(dfClaims, ClaimInt - Prediction)
dfClaims$RelativeError <- with(dfClaims, AbsoluteError / ClaimInt)

dfTriangle <- dfClaims %>% 
  group_by(PolicyYear, Lag) %>% 
  summarise(ActualLoss = sum(ClaimInt)
            , Predicted = sum(Prediction)
            , AbsoluteError = sum(AbsoluteError)
            , RelativeError = sum(AbsoluteError) / sum(ClaimInt))

GetTriangle <- function(df, whichCol){
  df <- select_(df, "PolicyYear", "Lag", whichCol) %>% 
    tidyr::spread_("Lag", whichCol)
  df
}

#=======================================
# Chain ladder prediction
#=======================================
triMack <- mutate(dfTriangle, ActualLoss = ifelse(is.na(Predicted), ActualLoss, NA)) %>% 
  GetTriangle("ActualLoss")
row.names(triMack) <- triMack$PolicyYear
triMack <- MackChainLadder(triMack[, -1])
triMack

clPredict <- as.data.frame(triMack$FullTriangle) %>% 
  mutate(PolicyYear = policyYears[origin]) %>% 
  rename(Predicted = value, Lag = dev) %>% 
  select(-origin)

dfCL <- select(dfTriangle, -Predicted, -RelativeError) %>% 
  merge(clPredict) %>% 
  mutate(Predicted = ifelse(is.na(AbsoluteError), NA, Predicted)
         , AbsoluteError = ActualLoss - Predicted
         , RelativeError = AbsoluteError / ActualLoss)

#=======================================
# Chain ladder prediction - bifurcated
#=======================================
triGood <- filter(dfClaims, Credit == "Good", is.na(Prediction)) %>% 
  group_by(PolicyYear, Lag) %>% 
  summarise(ActualLoss = sum(ClaimInt)) %>% 
  GetTriangle("ActualLoss")

row.names(triGood) <- triGood$PolicyYear
triGood <- MackChainLadder(triGood[, -1])

clGood <- as.data.frame(triGood$FullTriangle) %>% 
  mutate(PolicyYear = policyYears[origin]) %>% 
  rename(Predicted = value, Lag = dev) %>% 
  select(-origin)

triBad <- filter(dfClaims, Credit == "Bad", is.na(Prediction)) %>% 
  group_by(PolicyYear, Lag) %>% 
  summarise(ActualLoss = sum(ClaimInt)) %>% 
  GetTriangle("ActualLoss")

row.names(triBad) <- triBad$PolicyYear
triBad <- MackChainLadder(triBad[, -1])

clBad <- as.data.frame(triBad$FullTriangle) %>% 
  mutate(PolicyYear = policyYears[origin]) %>% 
  rename(Predicted = value, Lag = dev) %>% 
  select(-origin)

cl_Bif <- rbind(clGood, clBad) %>% 
  group_by(Lag, PolicyYear) %>% 
  summarise(Predicted = sum(Predicted))

dfCL_Bif <- select(dfTriangle, -Predicted, -RelativeError) %>% 
  merge(cl_Bif) %>% 
  mutate(Predicted = ifelse(is.na(AbsoluteError), NA, Predicted)
         , AbsoluteError = ActualLoss - Predicted
         , RelativeError = AbsoluteError / ActualLoss)

View(GetTriangle(dfTriangle, "Predicted"))
View(GetTriangle(dfTriangle, "AbsoluteError"))     
View(GetTriangle(dfTriangle, "RelativeError"))
View(GetTriangle(dfTriangle, "ActualLoss"))

View(GetTriangle(dfCL, "AbsoluteError"))
View(GetTriangle(dfCL, "RelativeError"))
View(GetTriangle(dfCL_Bif, "RelativeError"))
View(GetTriangle(dfCL_Bif, "AbsoluteError"))

sum(dfTriangle$AbsoluteError, na.rm=TRUE)
sum(dfCL$AbsoluteError, na.rm=TRUE)

# curve(LinkSD, from = .9, to = 3, main = "Standard deviation by link ratio"
#       , xlab = "Link ratio", ylab = "Standard deviation")

save(file = "GandL_Fit.rda"
     , pltBadCredit)
