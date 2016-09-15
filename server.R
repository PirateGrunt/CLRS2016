library(ggplot2)
library(dplyr)
library(tidyr)

summarizeDF <- function(df){
  
  df <- df %>% 
    group_by(Actuary, MCMC) %>% 
    summarise(Count = n())

  }

shinyServer(function(input, output) {
  
  nRec <- 500
  
  df <- data.frame(Actuary = sample(c(TRUE, FALSE), size = nRec, replace = TRUE)
                   , MCMC = sample(c(TRUE, FALSE), size = nRec, replace = TRUE))
  
  output$pltTile <- renderPlot({
   
    df <- summarizeDF(df)
    
    plt <- ggplot(df, aes(Actuary, MCMC)) + geom_tile(aes(fill = Count))
    plt
  
  })
  
})