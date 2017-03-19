library(jsonlite)
library(dplyr)
library(lubridate)
library(forecast)
library(zoo)

parse.json.volumes <- function(volume.data.filepath) {
  json.volumes <- fromJSON(volume.data.filepath)$volumes
  volume.data <- json.volumes %>%
    mutate(DataInformacao = dmy(DataInformacao),
           Fonte = as.factor(Fonte),
           Volume = as.numeric(Volume),
           VolumePercentual = as.numeric(VolumePercentual))
}

format.volume.data <- function(volume.data) {
  formatted.volume.data <- volume.data %>%
    rename(data=DataInformacao, fonte=Fonte, volume=Volume, volume.percentual=VolumePercentual) %>%
    select(-fonte) %>%
    mutate(mes = month(data),
           ano = year(data)) %>%
    group_by(ano, mes) %>%
    summarise(volume = min(volume),
              volume.percentual = min(volume.percentual)) %>%
    filter(ano >= 2012) %>%
    ungroup() %>%
    filter(!is.na(volume.percentual), row_number() != n()) %>%
    #group_by(ano,mes)
    arrange(ano,mes)
}

prepare.data.for.arima <- function(volume.data) {
  start.time <- volume.data %>% ungroup() %>% filter(row_number() == 1)
  end.time <- volume.data %>% ungroup() %>% filter(row_number() == n())
  
  print(start.time)
  print(end.time)
  
  
  data<-ts(volume.data$volume.percentual,
           start = c(start.time$ano,start.time$mes), 
           end = c(end.time$ano,end.time$mes), 
           frequency = 12)
}

fit.arima <- function(volume.data.ts) {
  ARIMAfit <- auto.arima(log10(volume.data.ts), approximation=FALSE,trace=FALSE)
  summary(ARIMAfit)  
}

forecast.timeseries <- function(volume.data.ts,arima.fit,num.months=12) {
  plot(forecast(ARIMAfit, h=12))
}

reservatorios.info <- read.csv("../data/reservatorios_info.csv")

reservatorio.id <- 12143

reservatorio.data.url <- paste0("https://wwws-cloud.lsd.ufcg.edu.br:42160/api/reservatorios/",reservatorio.id,"/monitoramento")
reservatorio.data.filepath <- paste0("../data/",reservatorio.id,".json")

download.file(reservatorio.data.url,destfile = reservatorio.data.filepath)

reservatorio.data.bruto <- parse.json.volumes(reservatorio.data.filepath)

reservatorio.data <- format.volume.data(reservatorio.data.bruto)

ts.reservatorio.data <- prepare.data.for.arima(reservatorio.data)

ARIMAfit <- fit.arima(ts.reservatorio.data)

forecast.timeseries(ts.reservatorio.data,ARIMAfit,12)

















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
