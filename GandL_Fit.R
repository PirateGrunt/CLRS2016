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
  
  fits[[i]] <- glm(ClaimValue ~ Credit
                   , family = poisson(link="log")
                   , offset = log(Prior)
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
  filter(!is.na(Prediction)) %>% 
  group_by(PolicyYear, Lag) %>% 
  summarise(ActualLoss = sum(ClaimInt)
            , PredictedGLM = sum(Prediction)
            , AbsoluteErrorGLM = sum(AbsoluteError)
            , RelativeErrorGLM = sum(AbsoluteError) / sum(ClaimInt))

GetTriangle <- function(df, whichCol){
  df <- select_(df, "PolicyYear", "Lag", whichCol) %>% 
    tidyr::spread_("Lag", whichCol)
  df
}

dfGLMSummary <- dfTriangle %>% 
  group_by(PolicyYear) %>% 
  summarise(ActualLoss = sum(ActualLoss)
            , PredictedGLM = sum(PredictedGLM)
            , AbsoluteErrorGLM = sum(AbsoluteErrorGLM))

#=======================================
# Chain ladder prediction
#=======================================
triMack <- dfUpper %>%
  group_by(PolicyYear, Lag) %>% 
  summarise(ActualLoss = sum(ClaimValue)) %>% 
  GetTriangle("ActualLoss")
row.names(triMack) <- triMack$PolicyYear
triMack <- MackChainLadder(triMack[, -1])
triMack

clPredict <- as.data.frame(triMack$FullTriangle) %>% 
  mutate(PolicyYear = policyYears[origin]) %>% 
  rename(Predicted = value, Lag = dev) %>% 
  select(-origin)

dfCL <- clPredict %>% 
  merge(dfTriangle) %>% 
  rename(PredictedCL = Predicted) %>% 
  mutate(AbsoluteErrorCL = ActualLoss - PredictedCL
         , RelativeErrorCL = AbsoluteErrorCL / ActualLoss)

dfCompareCL <- dfCL %>% 
  select(PolicyYear, Lag, AbsoluteErrorGLM, AbsoluteErrorCL) %>% 
  tidyr::gather(Method, AbsoluteError, -PolicyYear, -Lag)

dfCompareCL_Lag <- dfCompareCL %>% 
  group_by(Lag, Method) %>% 
  summarise(AbsoluteError = sum(AbsoluteError))

pltCompareCL_Lag <- ggplot(dfCompareCL_Lag, aes(as.factor(Lag), AbsoluteError, fill = Method)) + geom_bar(position = "dodge", stat = "identity")
pltCompareCL_Lag <- pltCompareCL_Lag + scale_y_continuous(labels = scales::comma)
pltCompareCL_Lag <- pltCompareCL_Lag + xlab("Lag")
pltCompareCL_Lag

dfCompareCL_PY <- dfCompareCL %>% 
  group_by(PolicyYear, Method) %>% 
  summarise(AbsoluteError = sum(AbsoluteError))

pltCompareCL_PY <- ggplot(dfCompareCL_PY, aes(as.factor(PolicyYear), AbsoluteError, fill = Method)) + geom_bar(position = "dodge", stat = "identity")
pltCompareCL_PY <- pltCompareCL_PY + scale_y_continuous(labels = scales::comma)
pltCompareCL_PY <- pltCompareCL_PY + xlab("Policy Year")
pltCompareCL_PY

#=======================================
# Chain ladder prediction - bifurcated
#=======================================
triGood <- filter(dfUpper, Credit == "Good") %>% 
  group_by(PolicyYear, Lag) %>% 
  summarise(ActualLoss = sum(ClaimValue)) %>% 
  GetTriangle("ActualLoss")
row.names(triGood) <- triGood$PolicyYear
triGood <- MackChainLadder(triGood[, -1], alpha = 2)

clGood <- as.data.frame(triGood$FullTriangle) %>% 
  mutate(PolicyYear = policyYears[origin]) %>% 
  rename(Predicted = value, Lag = dev) %>% 
  select(-origin)

triBad <- filter(dfUpper, Credit == "Bad") %>% 
  group_by(PolicyYear, Lag) %>% 
  summarise(ActualLoss = sum(ClaimValue)) %>% 
  GetTriangle("ActualLoss")
row.names(triBad) <- triBad$PolicyYear
triBad <- MackChainLadder(triBad[, -1], alpha = 2)

clBad <- as.data.frame(triBad$FullTriangle) %>% 
  mutate(PolicyYear = policyYears[origin]) %>% 
  rename(Predicted = value, Lag = dev) %>% 
  select(-origin)

cl_Bif <- rbind(clGood, clBad) %>% 
  group_by(Lag, PolicyYear) %>% 
  summarise(Predicted = sum(Predicted))

dfCL_Bif <- cl_Bif %>% 
  merge(dfTriangle) %>% 
  rename(PredictedCL = Predicted) %>% 
  mutate(AbsoluteErrorCL = ActualLoss - PredictedCL
         , RelativeErrorCL = AbsoluteErrorCL / ActualLoss)

dfCompareCL_Bif <- dfCL_Bif %>% 
  select(PolicyYear, Lag, AbsoluteErrorGLM, AbsoluteErrorCL) %>% 
  tidyr::gather(Method, AbsoluteError, -PolicyYear, -Lag)

dfCompareBifLag <- dfCompareCL_Bif %>% 
  group_by(Lag, Method) %>%
  summarise(AbsoluteError = sum(AbsoluteError))

pltCompareCL_Bif_Lag <- ggplot(dfCompareBifLag, aes(as.factor(Lag), AbsoluteError, fill = Method)) + geom_bar(position = "dodge", stat = "identity")
pltCompareCL_Bif_Lag <- pltCompareCL_Bif_Lag + scale_y_continuous(labels = scales::comma)
pltCompareCL_Bif_Lag <- pltCompareCL_Bif_Lag + xlab("Lag")
pltCompareCL_Bif_Lag

dfCompareBifPY <- dfCompareCL_Bif %>% 
  group_by(PolicyYear, Method) %>%
  summarise(AbsoluteError = sum(AbsoluteError))

pltCompareCL_Bif_PY <- ggplot(dfCompareBifPY, aes(as.factor(PolicyYear), AbsoluteError, fill = Method)) + geom_bar(position = "dodge", stat = "identity")
pltCompareCL_Bif_PY <- pltCompareCL_Bif_PY + scale_y_continuous(labels = scales::comma)
pltCompareCL_Bif_PY <- pltCompareCL_Bif_PY + xlab("Policy Year")
pltCompareCL_Bif_PY

save(file = "GandL_Fit.rda"
     , pltBadCredit
     , pltCompareCL_PY
     , pltCompareCL_Lag
     , pltCompareCL_Bif_PY
     , pltCompareCL_Bif_Lag)
