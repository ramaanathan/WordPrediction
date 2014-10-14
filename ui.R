library(shiny)

# Define UI for miles per gallon application
shinyUI(fluidPage(
  titlePanel("Word Prediction"),


 sidebarLayout(
    sidebarPanel(
      helpText(h3("Predict the next word given a short phrase or a sentence")),
      

      textInput("sentence","Enter a short sentence or phrase","I am testing")
    ),
    
    mainPanel(
      h4("Predicted Words"),
      tableOutput("WordsWithProbs")
      #textOutput("Predictions")
    )
  )



))
