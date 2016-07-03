# library(ggplot2)
# library(tidyr)
library(dplyr)
# library(scales)

#===============================================
# Install package
#===============================================
packageVersion("imagine")

if (!require("imagine")){
  devtools::install_github("PirateGrunt/imagine")
  library("imagine")
}

#===============================================
# Global functions
#===============================================
LinkSD <- function(x){
  x <- (x - 1) * .1
  x
}

PctOfTotalToGrowth <- function(x){
  growth <- x / dplyr::lag(x)
  growth[1] <- 1
  growth <- growth[-1]
  
  growth
}

#===============================================
# Basic parameters
#===============================================
policyYears <- 1990:1999
# claimsPerYear <- 500
# numClaims <- claimsPerYear * length(ay)
NumPolicies <- 500

badPctOfTotal <- c(.3, .35, .4, .45, .5, .55, .6, .65, .7, .75)
goodPctOfTotal <- 1 - badPctOfTotal
badPolicies <- NumPolicies * badPctOfTotal

badGrowth <- PctOfTotalToGrowth(badPctOfTotal)
goodAttrition <- PctOfTotalToGrowth(goodPctOfTotal)

150 * cumprod(badGrowth)
350 * cumprod(goodAttrition)

set.seed(1234)
dfBadCredit <- SimulatePolicies(NumPolicies * badPctOfTotal[1]
                                , policyYears
                                , Renewal = rep(1, 9)
                                , Growth = badGrowth - 1) %>% 
  mutate(Credit = "Bad")

dfGoodCredit <- SimulatePolicies(NumPolicies * goodPctOfTotal[1]
                                 , policyYears
                                 , Renewal = goodAttrition
                                 , Growth = rep(0, 9)) %>% 
  mutate(Credit = "Good")

dfAll <- dplyr::bind_rows(dfBadCredit, dfGoodCredit)

mojo <- dfAll %>% 
  mutate(PY = lubridate::year(PolicyEffectiveDate)) %>% 
  group_by(PY, Credit) %>% 
  summarise(PY_Count = n()) %>% 
  tidyr::spread(Credit, PY_Count)

sum(500 * badPctOfTotal)
sum(500 * goodPctOfTotal)

dfBadCreditClaims <- dfBadCredit %>% 
  ClaimsByLag(Frequency = FixedVal(1)
              , Lags = )

#===============================================
# Link ratio data frame
#===============================================
goodLink <- c(1, 1.8, 1.1700, 1.1300, 1.08, 1.0500, 1.0300, 1.020, 1.01500, 1.008)
badLink  <- c(1, 2.0, 1.2125, 1.1625, 1.10, 1.0625, 1.0375, 1.025, 1.01875, 1.010)
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
set.seed(1234)
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

upperTriangle <- with(dfClaims, (EvaluationMonth / 12 + AccidentYear-1) <= max(ay))
lowerTriangle <- !upperTriangle

dfUpper <- dfClaims[upperTriangle, ]
dfLower <- dfClaims[lowerTriangle, ]

save(dfClaims
     , upperTriangle
     , lowerTriangle
     , dfUpper
     , dfLower
     , ay
     , file = "GandL_Simulation.rda")