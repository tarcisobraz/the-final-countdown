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

# Loads data about the lakes
json_lakes <- read_json('data/info.json')
lakes <- list()
for (i in 1:length(json_lakes)) {
  lakes[[unlist(json_lakes[[i]]['reservat'])]] <- as.numeric(unlist(json_lakes[[i]]['id']))
}

# TEMP
load('data/prediction.rda')
pred$x <- 10^(pred$x)
pred$mean <- 10^(pred$mean)

shinyServer(function(input, output) {
  
  
  output$choose_lake <- renderUI(
    selectInput(inputId = 'lakes', label = 'Escolha o reservatório', choices = lakes)
  )
  
  
  output$forecastPlot <- renderPlot({
    autoplot(pred, PI = F, fcol = 'red') + labs(title = NULL, x = 'Tempo', y = 'Volume (%)') + theme_light() + scale_y_continuous(breaks = seq(0, max(pred$x), 5)) + theme(axis.text=element_text(size=12),
                                                                                                                                                                           axis.title=element_text(size=14,face="bold"))
  })
  
  output$decompositionPlot <- renderPlot({
    predd <- decompose(pred$x)
    autoplot(predd, labels = c('sazonal', 'tendência', 'restante'), range.bars = F) + theme_light() + labs(title = NULL, x = 'Tempo', y = 'Volume (%)') + theme(axis.text=element_text(size=12),
                                                                                                                                                               axis.title=element_text(size=14,face="bold"),
                                                                                                                                                               strip.text.y = element_text(size = 12))
  })
})
