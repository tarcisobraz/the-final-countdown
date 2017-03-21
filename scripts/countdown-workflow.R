library(jsonlite)
library(dplyr)
library(lubridate)
library(forecast)
library(zoo)
library(imputeTS)

reservoir.base.url <- "https://wwws-cloud.lsd.ufcg.edu.br:42160/api/reservatorios/"

parse.json.volumes <- function(volume.data.filepath) {
  json.volumes <- fromJSON(volume.data.filepath)$volumes
  volume.data <- json.volumes %>%
    mutate(DataInformacao = dmy(DataInformacao),
           Fonte = as.factor(Fonte),
           Volume = as.numeric(Volume),
           VolumePercentual = as.numeric(VolumePercentual))
}

format.volume.data <- function(volume.data) {
    formatted.volume.data <- reservatorio.data.bruto %>%
        rename(data=DataInformacao, fonte=Fonte, volume=Volume, volume.percentual=VolumePercentual) %>%
        select(-fonte) %>%
        mutate(mes = month(data),
               ano = year(data)) %>%
        mutate(data.volume = dmy(paste("1",as.character(mes),as.character(ano),sep="-"))) %>%
        group_by(data.volume) %>%
        summarise(volume = min(volume),
                  volume.percentual = min(volume.percentual)) %>%
        filter(year(data.volume) >= 2012) %>%
        ungroup() %>%
        filter(!is.na(volume.percentual),row_number() != n())
    
    time.series.completa <- data.frame(data.volume=seq(first(formatted.volume.data$data.volume), 
                                                       last(formatted.volume.data$data.volume), 
                                                       by = "month"))
    
    time.series.fixed <- left_join(time.series.completa,formatted.volume.data,by="data.volume") %>%
        # mutate(volume = volume + 1,
        #        volume.percentual = volume.percentual + 1)
        mutate(volume = ifelse(volume == 0,1,volume),
               volume.percentual = ifelse(volume.percentual == 0,1,volume.percentual))
}

prepare.data.for.arima <- function(volume.data) {
  start.time <- volume.data %>% ungroup() %>% filter(row_number() == 1)
  end.time <- volume.data %>% ungroup() %>% filter(row_number() == n())
  
  print(start.time)
  print(end.time)
  
  
  data <- ts(volume.data$volume.percentual,
           start = c(year(start.time$data.volume),month(start.time$data.volume)), 
           end = c(year(end.time$data.volume),month(end.time$data.volume)), 
           frequency = 12)
  data <- na.interpolation(data)
}

fit.arima <- function(volume.data.ts) {
  fit <- auto.arima(log10(volume.data.ts), approximation=FALSE,trace=FALSE)

  return(fit)
}

forecast.arima.model <- function(arima.fit,num.months=12,conf.level=90) {
    pred <- forecast(arima.fit, h=num.months, level=conf.level)
    return(pred)
}

plot.forecast <- function(curve){
    plot(curve$history$volume,type="l",xlim=c(2016,2018),ylim=c(1,15),xlab = "Tempo",ylab = "Volume do Açude")
    lines(curve$pred$mean,col="blue")
    lines(pred$upper,col="orange")
    lines(pred$lower,col="orange")
}

forecast.lake.volume <- function(lake.id,nmonths=12,conf=90) {
    reservatorio.data.url <- paste0(reservoir.base.url,reservatorio.id,"/monitoramento")
    reservatorio.data.filepath <- paste0("../data/",reservatorio.id,".json")
    
    download.file(reservatorio.data.url,destfile = reservatorio.data.filepath)
    reservatorio.data.bruto <- parse.json.volumes(reservatorio.data.filepath)
    reservatorio.data <- format.volume.data(reservatorio.data.bruto)
    ts.reservatorio.data <- prepare.data.for.arima(reservatorio.data)
    fit <- fit.arima(ts.reservatorio.data)
    pred <- forecast.arima.model(fit,nmonths,conf.level = conf)    
}

reservatorios.info <- read.csv("../data/reservatorios_info.csv")

reservatorio.id <- 12172
forecast.lake.volume(12172)

plot(10^(pred$x),type="l",xlim=c(2012,2018),ylim=c(0,100),xlab = "Tempo",ylab = "Volume do Açude")
lines(10^(pred$mean),col="blue")
lines(10^(pred$lower),col="orange")
lines(10^(pred$upper),col="orange")

#Saving Prediction
library(mgcv)
save(pred, file = "prediction.rda")
load(file = "prediction.rda")












#Manual

data<-ts(reservatorio.data$volume.percentual,start = c(2012,12), end = c(2017,2), frequency = 12)

plot(data,ylab="Diferenciação do Volume do Açude")
plot(diff(log10(data)),ylab="Diferenciação do Volume do Açude")

par(mfrow = c(1,2))
acf(ts(diff(log10(data))),main="ACF Tractor Sales")
pacf(ts(diff(log10(data))),main="PACF Tractor Sales")

ARIMAfit <- auto.arima(log10(data), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)  

plot(forecast(ARIMAfit, h=12))

pred <- forecast(ARIMAfit, h=12)
pred.df <- data.frame(Y=as.matrix(10^(pred$lower)),date=as.Date(as.yearmon(time(pred$lower))))
print(pred.df)
plot(data,type="l",xlim=c(2016,2018),ylim=c(1,15),xlab = "Tempo",ylab = "Volume do Açude")
lines(10^(pred$mean),col="blue")
lines(10^(pred$lower+2*pred$se),col="orange")
lines(10^(pred$upper-2*pred$se),col="orange")





write.csv(reservatorio.data,"mae-dagua.csv",row.names = F)
write.csv(reservatorio.data,"coremas.csv",row.names = F)
