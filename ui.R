shinyUI(bootstrapPage(
  
    numericInput(inputId = "alpha"
                 , label = "Beta alpha:"
                 , min = 0
                 , value = 1)
  
  , numericInput(inputId = "beta"
                 , label = "Beta beta:"
                 , min = 0
                 , value = 1)
  
  , checkboxInput(inputId = "chkActuary"
                  , label = "I am an actuary"
                  , value = FALSE)
  
  , checkboxInput(inputId = "chkMCMC"
                  , label = "I use MCMC"
                  , value = FALSE)
  
  , selectInput(inputId = "Platform"
                , label = "In my work, I primarily use: "
                , choices = c("Excel", "R", "Other")
                , selected = "Excel")
  
  , submitButton(text = "Submit")
  
  , plotOutput(outputId = "pltTile", height = "300px")
  
))