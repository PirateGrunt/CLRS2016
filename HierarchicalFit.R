library(ggplot2)
library(dplyr)
library(scales)
library(nlme)

load("GandL_Simulate.rda")

dfUpperLag2 <- filter(dfUpperPY_Split, Lag == 2)
fitAgg <- lme(data = dfUpperLag2, fixed = ClaimValue ~ 0 + Prior, random = ~ 0 + Prior | Credit)
dfUpperLag2$Blended <- predict(fitAgg)

fitLM <- lm(ClaimValue ~ 0 + Prior:Credit, data = dfUpperLag2)
dfUpperLag2$Individual <- predict(fitLM)

pltBlended <- ggplot(dfUpperLag2, aes(Prior, ClaimValue, color = Credit)) + geom_point(aes(y = ClaimValue))
pltBlended <- pltBlended + geom_line(aes(y = Blended), linetype = "dotted")
pltBlended <- pltBlended + scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)
pltBlended <- pltBlended + ggtitle("Hierarchical Fit")
pltBlended

save(file = "HierarchicalFit.rda"
     , pltBlended)
