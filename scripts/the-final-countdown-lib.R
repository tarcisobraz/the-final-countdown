library(jsonlite)
library(dplyr)
library(lubridate)
library(forecast)
library(zoo)
library(imputeTS)

reservoir.base.url <- "https://wwws-cloud.lsd.ufcg.edu.br:42160/api/reservatorios/"

parse.json.volumes <- function(volume.data.filepath) {
    json.volumes <- fromJSON(volume.data.filepath)$volumes
    if(length(json.volumes) == 0) {
      stop("Não há dados disponíveis para este reservatório.")
    }
    volume.data <- json.volumes %>%
        mutate(DataInformacao = dmy(DataInformacao),
               Fonte = as.factor(Fonte),
               Volume = as.numeric(Volume),
               VolumePercentual = as.numeric(VolumePercentual))
}

format.volume.data <- function(volume.data,raw.reservoir.data) {
    formatted.volume.data <- raw.reservoir.data %>%
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
        mutate(volume = ifelse(volume == 0,1,volume),
               volume.percentual = ifelse(volume.percentual == 0,1,volume.percentual))
}

prepare.data.for.arima <- function(volume.data) {
    start.time <- volume.data %>% ungroup() %>% filter(row_number() == 1)
    end.time <- volume.data %>% ungroup() %>% filter(row_number() == n())
    
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

forecast.lake.volume <- function(reservoir_id,nmonths=12,conf=90) {
    reservatorio.data.url <- paste0(reservoir.base.url,reservoir_id,"/monitoramento")
    reservatorio.data.filepath <- paste0("../data/",reservoir_id,".json")
    
    if (!file.exists(reservatorio.data.filepath)) {
      download.file(reservatorio.data.url,destfile = reservatorio.data.filepath)  
    }
    reservatorio.data.bruto <- parse.json.volumes(reservatorio.data.filepath)
    reservatorio.data <- format.volume.data(reservatorio.data.bruto,reservatorio.data.bruto)
    ts.reservatorio.data <- prepare.data.for.arima(reservatorio.data)
    fit <- fit.arima(ts.reservatorio.data)
    pred <- forecast.arima.model(fit,nmonths,conf.level = conf)    
}