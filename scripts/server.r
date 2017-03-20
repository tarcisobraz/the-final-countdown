library(shiny)
library(datasets)
library(forecast)


shinyServer(function(input, output) {
  
  getDataset <- reactive({
    if (input$variable=="boqueirao")
    {
      return(boqueirao)
    }
    else if (input$variable=="gas")
    {
      
      return(read.csv())
    }
    else
    {
      return(wineind)
    }
  })
  
  output$caption <- renderText({
    paste("Volume percentual vs Tempo")
  })
  
  output$dcompPlot <- renderPlot({
    # ds_ts <- ts(getDataset(), frequency=12)
    # data<-ts(getDataset()$volume.percentual,start = c(2012,1), end = c(2017,2), frequency = 12)
    # f <- decompose(data)
    # plot(f)
    data<-ts(getDataset()$volume.percentual,start = c(2012,1), end = c(2017,2), frequency = 12)
    plot(decompose(data))
  })
  
  output$arimaForecastPlot <- renderPlot({
    # fit <- auto.arima(getDataset())
    # plot(forecast(fit, h=input$ahead))
    ARIMAfit <- auto.arima(data, approximation=FALSE,trace=FALSE)
    plot(forecast(ARIMAfit, h=input$ahead))
  })
  
  output$etsForecastPlot <- renderPlot({
    # getDataset()
    # input$ahead
    
    # library(dplyr)
    # library(ggplot2)
    # require ("zoo")
    # 
    # 
    # 
    # volumes.por.mes <- volumes.data %>%
    #   group_by(ano, mes) %>%
    #   summarise(volume = min(volume),
    #             volume.percentual = min(volume.percentual)) %>%
    #   filter(ano >= 2012) %>%
    #   ungroup() %>%
    #   filter(row_number() != n()) %>%
    #   group_by(ano,mes)
    # 
    data<-ts(volumes.por.mes$volume.percentual,start = c(2012,1), end = c(2017,2), frequency = 12)
    # par(mfrow = c(1,2))
    # acf(ts(diff(log10(data))),main="ACF Tractor Sales")
    # pacf(ts(diff(log10(data))),main="PACF Tractor Sales")
    # library(forecast)
    ARIMAfit <- auto.arima(log10(data), approximation=FALSE,trace=FALSE)
    pred <- predict(ARIMAfit, n.ahead = input$ahead)
    pred.df <- data.frame(Y=as.matrix(10^(pred$pred)),date=as.Date(as.yearmon(time(pred$pred))))
    print(pred.df)
    plot(data,type="l",xlim=c(2016,2017 + input$ahead / 12 ),ylim=c(1,15),xlab = "Tempo",ylab = "Volume do Açude")
    lines(10^(pred$pred),col="blue")
    lines(10^(pred$pred+2*pred$se),col="orange")
    lines(10^(pred$pred-2*pred$se),col="orange")
    
  })
  
})
