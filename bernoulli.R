library(rstan)
library(dplyr)

sampleN <- 10
y <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
predN <- 5

fit1 <- stan(file = 'bernoulli.stan'
            , data = list(sampleN, y, predN, betaA = 1, betaB = 1)
            , iter = 1000
            , seed = 1234)

y_pred1 <- extract(fit1, 'y_pred') %>% unlist()
theta1 <- extract(fit1, 'theta') %>% unlist()

pltTheta1 <- ggplot(as.data.frame(theta1), aes(theta1)) + geom_density(fill = "grey")
pltTheta1 <- pltTheta1 + geom_vline(color = "red", xintercept = 0.5)
pltTheta1

pltPred1 <- ggplot(as.data.frame(y_pred1), aes(y_pred1)) + geom_bar()
pltPred1

fit2 <- stan(file = 'bernoulli.stan'
             , data = list(sampleN, y, predN, betaA = 10, betaB = 10)
             , iter = 1000
             , seed = 1234)

y_pred2 <- extract(fit2, 'y_pred') %>% unlist()
theta2 <- extract(fit2, 'theta') %>% unlist()

pltTheta2 <- ggplot(as.data.frame(theta2), aes(theta2)) + geom_density(fill = "grey")
pltTheta2 <- pltTheta2 + geom_vline(color = "red", xintercept = 0.5)
pltTheta2

pltPred2 <- ggplot(as.data.frame(y_pred2), aes(y_pred2)) + geom_bar()
pltPred2

dfCompare <- data.frame(Fit = c(rep("1", length(theta1)), rep("2", length(theta2)))
                        , Theta = c(theta1, theta2)
                        , Prediction = c(y_pred1, y_pred2))

pltCompareTheta <- ggplot(dfCompare, aes(Theta, fill = Fit)) + geom_density()
pltCompareTheta <- pltCompareTheta + geom_vline(color = "red", xintercept = 0.5)
pltCompareTheta

pltComparePred <- ggplot(dfCompare, aes(Prediction, fill = Fit)) + geom_bar(position = "dodge")
pltComparePred

save(file = "bernoulli.rda"
     , pltTheta1
     , pltTheta2
     , pltPred1
     , pltPred2
     , pltCompareTheta
     , pltComparePred
     , theta1
     , theta2
     , y_pred1
     , y_pred2)
