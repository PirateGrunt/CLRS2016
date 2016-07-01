library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(ChainLadder)

# source("GandL_Simulate.R")
load("GandL_Simulation.rda")

#===========================
# Poisson GLM prediction
#===========================
upperTriangle <- with(dfClaims, (EvaluationMonth / 12 + AccidentYear-1) <= max(ay))
lowerTriangle <- !upperTriangle
dfClaims$Prediction <- ifelse(upperTriangle, NA, 0)
dfClaims <- dfClaims %>% 
  arrange(ClaimNumber, EvaluationMonth)

fits <- list()
for (i in 2:numLinks){
  sampleData <- with(dfClaims, which(EvaluationMonth == months[i] 
                                     & AccidentYear %in% ay[1:(10-i+1)]))
  fits[[i-1]] <- glm(Current ~ Credit
                     , family = poisson(link="log")
                     , offset = log(Prior)
                     , data = dfClaims[sampleData, ])
  
  predictionData <- with(dfClaims, which(EvaluationMonth == months[i] 
                                         & AccidentYear %in% ay[(10-i+2):10]))
  
  dfClaims$Prediction[predictionData] <- predict(fits[[i-1]]
                                             , newdata = dfClaims[predictionData, ]
                                             , type = "response")
  newPredict <- predictionData + 1
  newPredict <- subset(newPredict, newPredict <= (numClaims * numLinks))
  dfClaims$Prior[newPredict] <- dfClaims$Prediction[predictionData[1:length(newPredict)]]
}
rm(sampleData, predictionData, i, newPredict)

dfClaims$AbsoluteError <- with(dfClaims, Current - Prediction)
dfClaims$RelativeError <- with(dfClaims, AbsoluteError / Current)

dfTriangle <- dfClaims %>% 
  group_by(AccidentYear, EvaluationMonth) %>% 
  summarise(ActualLoss = sum(Current)
            , Predicted = sum(Prediction)
            , AbsoluteError = sum(AbsoluteError)
            , RelativeError = sum(AbsoluteError) / sum(Current))

GetTriangle <- function(df, whichCol){
  df <- select_(df, "AccidentYear", "EvaluationMonth", whichCol) %>% 
    tidyr::spread_("EvaluationMonth", whichCol)
  df
}

#=======================================
# Chain ladder prediction
#=======================================
triMack <- mutate(dfTriangle, ActualLoss = ifelse(is.na(Predicted), ActualLoss, NA)) %>% 
  GetTriangle("ActualLoss")
row.names(triMack) <- triMack$AccidentYear
triMack <- MackChainLadder(triMack[, -1])
triMack

clPredict <- as.data.frame(triMack$FullTriangle) %>% 
  mutate(AccidentYear = ay[origin]) %>% 
  rename(Predicted = value, EvaluationMonth = dev) %>% 
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
  group_by(AccidentYear, EvaluationMonth) %>% 
  summarise(ActualLoss = sum(Current)) %>% 
  GetTriangle("ActualLoss")

row.names(triGood) <- triGood$AccidentYear
triGood <- MackChainLadder(triGood[, -1])

clGood <- as.data.frame(triGood$FullTriangle) %>% 
  mutate(AccidentYear = ay[origin]) %>% 
  rename(Predicted = value, EvaluationMonth = dev) %>% 
  select(-origin)

triBad <- filter(dfClaims, Credit == "Bad", is.na(Prediction)) %>% 
  group_by(AccidentYear, EvaluationMonth) %>% 
  summarise(ActualLoss = sum(Current)) %>% 
  GetTriangle("ActualLoss")

row.names(triBad) <- triBad$AccidentYear
triBad <- MackChainLadder(triBad[, -1])

clBad <- as.data.frame(triBad$FullTriangle) %>% 
  mutate(AccidentYear = ay[origin]) %>% 
  rename(Predicted = value, EvaluationMonth = dev) %>% 
  select(-origin)

cl_Bif <- rbind(clGood, clBad) %>% 
  group_by(EvaluationMonth, AccidentYear) %>% 
  summarise(Predicted = sum(Predicted))

dfCL_Bif <- select(dfTriangle, -Predicted, -RelativeError) %>% 
  merge(cl_Bif) %>% 
  mutate(Predicted = ifelse(is.na(AbsoluteError), NA, Predicted)
         , AbsoluteError = ActualLoss - Predicted
         , RelativeError = AbsoluteError / ActualLoss)

#=======================================
# Explore the results
#=======================================
pltBadCredit <- ggplot(data=dfClaims, aes(x=as.factor(AccidentYear), fill=Credit)) + geom_bar()
pltBadCredit <- pltBadCredit + labs(x = "Accident Year", y = "Claims", title = "Portion of bad credit claims by AY")
pltBadCredit <- pltBadCredit + scale_y_continuous(labels = comma)
pltBadCredit

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

curve(LinkSD, from = .9, to = 3, main = "Standard deviation by link ratio"
      , xlab = "Link ratio", ylab = "Standard deviation")

save(pltBadCredit
     , file = "GandL.rda")
