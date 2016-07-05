library(methods)
library(ggplot2)
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

# 350 * cumprod(goodAttrition)

set.seed(1234)
dfBadCredit <- SimulatePolicies(NumPolicies * badPctOfTotal[1]
                                , policyYears
                                , Renewal = rep(1, 9)
                                , Growth = badGrowth - 1
                                , AdditionalColumns = list(Credit = "Bad"))

if (nrow(dfBadCredit) != sum(NumPolicies * sum(badPctOfTotal))) {
  warning("Row total for bad credit data frame doesn't match expectation.")
}

dfGoodCredit <- SimulatePolicies(NumPolicies * goodPctOfTotal[1]
                                 , policyYears
                                 , Renewal = goodAttrition
                                 , Growth = rep(0, 9)
                                 , StartID = max(dfBadCredit$PolicyID) + 1
                                 , AdditionalColumns = list(Credit = "Good"))

if (nrow(dfGoodCredit) != sum(NumPolicies * sum(goodPctOfTotal))) {
  warning("Row total for good credit data frame doesn't match expectation.")
}

dfPolicy <- dplyr::bind_rows(dfBadCredit, dfGoodCredit)

dfDupPolicyID <- dfPolicy %>% 
  select(PolicyID, Credit) %>% 
  unique() %>% 
  group_by(PolicyID) %>% 
  summarise(NumRec = n()) %>% 
  filter(NumRec > 1)

if (nrow(dfDupPolicyID) > 0) stop("Duplicate PolicyIDs")

badLink  <- c(1, 2.0, 1.2125, 1.1625, 1.10, 1.0625, 1.0375, 1.025, 1.01875, 1.010)
goodLink <- c(1, 1.8, 1.1700, 1.1300, 1.08, 1.0500, 1.0300, 1.020, 1.01500, 1.008)

if (length(badLink) != length(goodLink)){
  warning("Length of link ratio vectors is not identical.")
}

lstBadCreditLinks <- vector("list", length(policyYears) - 1)
lstGoodCreditLinks <- vector("list", length(policyYears) - 1)

for (i in seq_along(lstBadCreditLinks)){
  lstBadCreditLinks[[i]] <- NormalHelper(badLink[i]
                                         , LinkSD(badLink[i])
                                         , lowerBound = .3
                                         , upperBound = 4)
  
  lstGoodCreditLinks[[i]] <- NormalHelper(goodLink[i]
                                          , LinkSD(goodLink[i])
                                          , lowerBound = .3
                                          , upperBound = 4)
}

dfBadClaims <- dfBadCredit %>% 
  ClaimsByLag(Frequency = FixedVal(1)
              , Severity = LognormalHelper(8, 1.3)
              , Links = lstBadCreditLinks
              , Lags = seq_along(badLink))

expectedClaims <- nrow(dfBadCredit) * length(badLink)
foundClaims <- nrow(dfBadClaims)
if (expectedClaims != foundClaims) {
  msg <- paste0("Number of bad claims does not equal expected."
               , "\nExpected ", expectedClaims, " claims."
               , "\nFound ", foundClaims, " claims.")
  warning(msg)
}

# dfBadTri <- dfBadClaims %>% 
#   tidyr::spread(Lag, ClaimValue)

dfGoodClaims <- dfGoodCredit %>% 
  ClaimsByLag(Frequency = FixedVal(1)
              , Severity = LognormalHelper(8, 1.3)
              , Links = lstGoodCreditLinks
              , Lags = seq_along(goodLink))

expectedClaims <- nrow(dfGoodCredit) * length(goodLink)
foundClaims <- nrow(dfGoodClaims)
if (expectedClaims != foundClaims) {
  msg <- paste0("Number of good claims does not equal expected."
               , "\nExpected ", expectedClaims, " claims."
               , "\nFound ", foundClaims, " claims.")
  warning(msg)
}

dfClaims <- dplyr::bind_rows(dfBadClaims, dfGoodClaims) %>% 
  dplyr::inner_join(dfPolicy, by = c("PolicyID", "PolicyEffectiveDate")) %>% 
  mutate(EvalYear = lubridate::year(PolicyEffectiveDate) + Lag - 1)

expectedClaims <- NumPolicies * length(badLink) * length(policyYears)
foundClaims <- nrow(dfClaims)
if (expectedClaims != foundClaims) {
  msg <- paste0("Number of total claims does not equal expected."
                , "\nExpected ", expectedClaims, " claims."
                , "\nFound ", foundClaims, " claims.")
  warning(msg)
}


upperTriangle <- with(dfClaims, EvalYear <= max(policyYears))
lowerTriangle <- !upperTriangle

dfUpper <- dfClaims[upperTriangle, ]
dfLower <- dfClaims[lowerTriangle, ]

save(dfClaims
     , dfPolicy
     , dfUpper
     , dfLower
     , upperTriangle
     , lowerTriangle
     , policyYears
     , file = "GandL_Simulation.rda")