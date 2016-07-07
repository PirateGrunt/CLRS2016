library(methods)
library(ggplot2)
# library(tidyr)
library(dplyr)
library(scales)

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

expectedRows <- sum(NumPolicies * sum(goodPctOfTotal))
foundRows <- nrow(dfGoodCredit)
if (expectedRows != foundRows) {
  msg <- paste0("Row total for good credit data frame doesn't match expectation."
                , "\nExpected ", expectedRows, " rows."
                , "\nFound ", foundRows, " rows.")
  warning(msg)
}

dfPolicy <- dplyr::bind_rows(dfBadCredit, dfGoodCredit)

dfDupPolicyID <- dfPolicy %>% 
  select(PolicyID, Credit) %>% 
  unique() %>% 
  group_by(PolicyID) %>% 
  summarise(NumRec = n()) %>% 
  filter(NumRec > 1)

if (nrow(dfDupPolicyID) > 0) stop("Duplicate PolicyIDs")

#=============================
# Simulate claims
#=============================
badLink  <- c(2.0, 1.2125, 1.1625, 1.10, 1.0625, 1.0375, 1.025, 1.01875, 1.010)
goodLink <- c(1.6, 1.1700, 1.1300, 1.08, 1.0500, 1.0300, 1.020, 1.01500, 1.008)

if (length(badLink) != length(goodLink)){
  warning("Length of link ratio vectors is not identical.")
}

lags <- seq(1, length(badLink) + 1)

lstBadCreditLinks <- mapply(NormalHelper
                            , mean = badLink
                            , sd = LinkSD(badLink)
                            , lowerBound = 0.3
                            , upperBound = 4
                            , SIMPLIFY = FALSE)

lstGoodCreditLinks <- mapply(NormalHelper
                            , mean = goodLink
                            , sd = LinkSD(goodLink)
                            , lowerBound = 0.3
                            , upperBound = 4
                            , SIMPLIFY = FALSE)
dfBadClaims <- dfBadCredit %>% 
  ClaimsByLag(Frequency = FixedVal(1)
              , Severity = LognormalHelper(8, 1.3)
              , Links = lstBadCreditLinks
              , Lags = lags)

expectedClaims <- nrow(dfBadCredit) * length(lags)
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
              , Lags = lags)

expectedClaims <- nrow(dfGoodCredit) * length(lags)
foundClaims <- nrow(dfGoodClaims)
if (expectedClaims != foundClaims) {
  msg <- paste0("Number of good claims does not equal expected."
               , "\nExpected ", expectedClaims, " claims."
               , "\nFound ", foundClaims, " claims.")
  warning(msg)
}

dfClaims <- dplyr::bind_rows(dfBadClaims, dfGoodClaims) %>% 
  dplyr::inner_join(dfPolicy, by = c("PolicyID", "PolicyEffectiveDate")) %>% 
  mutate(PolicyYear = lubridate::year(PolicyEffectiveDate)
         , EvalYear = PolicyYear + Lag - 1
         , ClaimInt = as.integer(round(ClaimValue))
         , Upper = EvalYear <= max(policyYears)) %>% 
  arrange(PolicyID, ClaimID, Lag) %>% 
  group_by(PolicyID, ClaimID) %>% 
  mutate(Prior = lag(ClaimValue)
         , PriorInt = lag(ClaimInt)) %>% 
  ungroup()

expectedClaims <- NumPolicies * length(lags) * length(policyYears)
foundClaims <- nrow(dfClaims)
if (expectedClaims != foundClaims) {
  msg <- paste0("Number of total claims does not equal expected."
                , "\nExpected ", expectedClaims, " claims."
                , "\nFound ", foundClaims, " claims.")
  warning(msg)
}

dfUpper <- dfClaims %>% filter(Upper)
dfLower <- dfClaims %>% filter(!Upper)
dfLag2 <- dfUpper %>% 
  filter(Lag == 2)

AggPY <- function(df){
  df <- df %>% 
    summarise(ClaimValue = sum(ClaimValue)
              , ClaimInt = sum(ClaimInt)
              , Prior = sum(Prior)
              , PriorInt = sum(PriorInt)) %>% 
    ungroup()
  
  df
}

dfByPY <- dfClaims %>% 
  group_by(PolicyYear, Lag) %>% 
  AggPY()

dfUpperPY <- dfUpper %>% 
  group_by(PolicyYear, Lag) %>% 
  AggPY()

dfLowerPY <- dfLower %>% 
  group_by(PolicyYear, Lag) %>% 
  AggPY()

dfByPY_Split <- dfClaims %>% 
  group_by(PolicyYear, Lag, Credit) %>% 
  AggPY()

dfUpperPY_Split <- dfUpper %>% 
  group_by(PolicyYear, Lag, Credit) %>% 
  AggPY()

dfLowerPY_Split <- dfLower %>% 
  group_by(PolicyYear, Lag, Credit) %>% 
  AggPY()

#=======================================
# Explore the data
#=======================================
pltBadCredit <- ggplot(data = dfClaims, aes(x=as.factor(PolicyYear), fill=Credit)) + geom_bar()
pltBadCredit <- pltBadCredit + labs(x = "Policy Year", y = "Claims", title = "Portion of bad credit claims by Policy Year")
pltBadCredit <- pltBadCredit + scale_y_continuous(labels = comma)
pltBadCredit

pltAllClaims <- ggplot(data = subset(dfUpper, Lag != 1), aes(PriorInt, ClaimInt)) + geom_point() + facet_wrap(~ Lag)
pltAllClaims <- pltAllClaims + scale_x_log10(labels = scales::comma) + scale_y_log10(labels = scales::comma)
pltAllClaims

pltLag2 <- ggplot(data = dfLag2, aes(Prior, ClaimValue)) + geom_point()
pltLag2 <- pltLag2 + scale_x_log10(labels = scales::comma) + scale_y_log10(labels = scales::comma)
pltLag2 <- pltLag2 + labs(x = "Prior Cumulative", y = "Current Cumulative", title = "Individual Claims Link Ratios")
pltLag2

pltLag2Color <- ggplot(data = dfLag2, aes(Prior, ClaimValue, color = Credit)) + geom_point(alpha = 0.3)
pltLag2Color <- pltLag2Color + scale_x_log10(labels = scales::comma) + scale_y_log10(labels = scales::comma)
pltLag2Color <- pltLag2Color + labs(x = "Prior Cumulative", y = "Current Cumulative", title = "Individual Claims Link Ratios")
pltLag2Color

pltPY <- ggplot(filter(dfUpperPY, Lag == 2), aes(Prior, ClaimValue)) + geom_point()
pltPY <- pltPY + scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)
pltPY

pltPY_Split <- ggplot(filter(dfUpperPY_Split, Lag == 2), aes(Prior, ClaimValue, color = Credit)) + geom_point()
pltPY_Split <- pltPY_Split + scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)
pltPY_Split

save(file = "GandL_Simulate.rda"
     , policyYears
     , goodLink
     , badLink
     , lags
     , dfClaims
     , dfPolicy
     , dfUpper
     , dfLower
     , dfLag2
     , dfByPY
     , dfByPY_Split
     , dfUpperPY
     , dfLowerPY
     , dfUpperPY_Split
     , dfLowerPY_Split
     , pltBadCredit
     , pltLag2
     , pltLag2Color)