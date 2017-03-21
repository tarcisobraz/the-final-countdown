#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

source('./the-final-countdown-lib.R')

reservoir.base.url <- "https://wwws-cloud.lsd.ufcg.edu.br:42160/api/reservatorios/"
reservoirs.info <- read.csv("./reservatorios_info.csv")

# Loads data about the reservoirs
reservoirs <- reservoirs.info$id
names(reservoirs) <- reservoirs.info$reservat
reservoirs <- as.list(reservoirs)


shinyServer(function(input, output) {
  
  
  output$choose_reservoir <- renderUI(
    selectInput(inputId = 'reservoir.id', label = 'Escolha o reservatório', choices = reservoirs)
  )
  
  
  output$forecastPlot <- renderPlot({
    pred <- forecast.lake.volume(input$reservoir.id, nmonths = input$select_nmonths)
    pred$x <- 10^(pred$x)
    pred$mean <- 10^(pred$mean)
    pred$lower <- 10^(pred$lower)
    pred$upper <- 10^(pred$upper)
    autoplot(pred, fcol = 'red') + labs(title = NULL, x = 'Tempo', y = 'Volume (%)', legend = 'Intervalo de confiança') + theme_light() + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
  })
  
  output$decompositionPlot <- renderPlot({
    pred <- forecast.lake.volume(input$reservoir.id, nmonths = input$select_nmonths)
    pred$x <- 10^(pred$x)
    pred$mean <- 10^(pred$mean)
    predd <- decompose(pred$x)
    autoplot(predd, labels = c('sazonal', 'tendência', 'restante'), range.bars = F) + theme_light() + labs(title = NULL, x = 'Tempo', y = 'Volume (%)') + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), strip.text.y = element_text(size = 12))
  })
})
