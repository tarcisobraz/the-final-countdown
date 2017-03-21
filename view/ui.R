#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Contagem Regressiva"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput(outputId = "choose_reservoir"),
      sliderInput(inputId = "select_nmonths", label = "Escolha quantos meses de previsão", min = 3, max = 24, value = 3, step = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Previsão", plotOutput("forecastPlot")),
        tabPanel("Decomposição", plotOutput("decompositionPlot"))
      )
    )
  )
  
  
  
  )
)
