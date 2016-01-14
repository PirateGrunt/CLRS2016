library(ggplot2, tidyr)
library(dplyr)
library(scales)

#===============================================
# Global functions
#===============================================
LinkSD <- function(x){
  x <- (x - 1) * .1
  x
}

#===============================================
# Basic parameters
#===============================================
ay <- 1990:1999
claimsPerYear <- 500
numClaims <- claimsPerYear * length(ay)

#===============================================
# Link ratio data frame
#===============================================
goodLink <- c(1, 1.8, 1.17, 1.13, 1.08, 1.05, 1.03, 1.02, 1.015, 1.008)
badLink <- c(1, 2, 1.2125, 1.1625, 1.1, 1.0625, 1.0375, 1.025, 1.01875, 1.01)
prod(goodLink)
prod(badLink)
numLinks <- length(goodLink)
months <- seq(12, by=12, length.out=length(goodLink))

dfLinkRatio <- data.frame(Credit = c(rep("Good", 10), rep("Bad", 10))
                          , LinkRatio = c(goodLink, badLink)
                          , EvaluationMonth = rep(months, 2))
rm(goodLink, badLink)

#===============================================
# Credit data frame
#===============================================
dfCredit <- data.frame(AccidentYear = ay
                       , Bad = c(.3, .35, .4, .45, .5, .55, .6, .65, .7, .75))
dfCredit$BadClaims <- dfCredit$Bad * claimsPerYear

#===============================================
# Claims data frame
#===============================================
dfClaims <- data.frame(AccidentYear = rep(ay, claimsPerYear)
                       , Credit = rep("Good", numClaims)
                       , stringsAsFactors = FALSE)
dfClaims$ClaimNumber <- 1:(nrow(dfClaims))

# Establish which claims are associated with bad credit
set.seed(123)
for (i in seq_along(ay)){
  whichAy <- which(dfClaims$AccidentYear == ay[i])
  badClaims <- dfCredit$BadClaims[dfCredit$AccidentYear == ay[i]]
  badClaims <- sample(whichAy, badClaims)
  dfClaims$Credit[badClaims] <- "Bad"
}
rm(badClaims, whichAy)

# Setup matrix for current and prior claim vals. We'll melt this later.
claimVals <- matrix(nrow = numClaims, ncol = numLinks)
priorVals <- matrix(nrow = numClaims, ncol = numLinks)

claimVals[, 1] <- rlnorm(numClaims, meanlog = 8, sdlog = 1.3)
priorVals[, 1] <- claimVals[, 1]
for (i in 2:numLinks){
  iGoodLink <- dfLinkRatio$EvaluationMonth == months[i] & dfLinkRatio$Credit == "Good"
  iGoodLink <- dfLinkRatio$LinkRatio[iGoodLink]
  iBadLink <- dfLinkRatio$EvaluationMonth == months[i] & dfLinkRatio$Credit == "Bad"
  iBadLink <- dfLinkRatio$LinkRatio[iBadLink]
  link <- ifelse(dfClaims$Credit == "Good", iGoodLink, iBadLink)
  link <- link * rnorm(numClaims, mean=1, sd = LinkSD(link))
  link <- pmax(link, 1)
  claimVals[, i] <- round(claimVals[, i-1] * link)
  priorVals[, i] <- claimVals[, i-1]
}

dfClaims <- cbind(dfClaims, claimVals, priorVals)
rm(claimVals, priorVals, iBadLink, iGoodLink, link)

# Name the columns and melt.
evalCols <- c(paste0("C", months), paste0("P", months))
colnames(dfClaims)[4:23] <- evalCols

dfClaims <- tidyr::gather(dfClaims, "EvaluationMonth", "Loss", which(names(dfClaims) %in% evalCols))
dfClaims$Prior <- ifelse(grepl("C", dfClaims$EvaluationMonth), "Current", "Prior")
dfClaims$EvaluationMonth <- as.integer(gsub("[CP]", "", dfClaims$EvaluationMonth))
dfClaims$Loss[dfClaims$EvaluationMonth == months[1] & dfClaims$Prior == "Prior"] <- NA

dfClaims <- tidyr::spread(dfClaims, Prior, Loss)

rm(evalCols)

#===========================
# Poisson GLM prediction
#===========================
upperTriangle <- with(dfClaims, (EvaluationMonth / 12 + AccidentYear-1) <= max(ay))
lowerTriangle <- !upperTriangle
dfClaims$Prediction <- ifelse(upperTriangle, NA, 0)
dfClaims <- arrange(dfClaims, ClaimNumber, EvaluationMonth)

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

dfTriangle <- group_by(dfClaims, AccidentYear, EvaluationMonth) %>% 
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
library(ChainLadder)
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
# Explore the results
#=======================================
plt <- ggplot(data=dfClaims, aes(x=as.factor(AccidentYear), fill=Credit)) + geom_bar()
plt <- plt + labs(x = "Accident Year", y = "Claims", title = "Portion of bad credit claims by AY")
plt <- plt + scale_y_continuous(labels = comma)
plt

View(GetTriangle(dfTriangle, "Predicted"))
View(GetTriangle(dfTriangle, "AbsoluteError"))     
View(GetTriangle(dfTriangle, "RelativeError"))
View(GetTriangle(dfTriangle, "ActualLoss"))

View(GetTriangle(dfCL, "AbsoluteError"))
View(GetTriangle(dfCL, "RelativeError"))

sum(dfTriangle$AbsoluteError, na.rm=TRUE)
sum(dfCL$AbsoluteError, na.rm=TRUE)

curve(LinkSD, from = .9, to = 3, main = "Standard deviation by link ratio"
      , xlab = "Link ratio", ylab = "Standard deviation")
