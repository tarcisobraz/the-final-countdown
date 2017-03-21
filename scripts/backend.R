source("the-final-countdown-lib.R")

reservoir.base.url <- "https://wwws-cloud.lsd.ufcg.edu.br:42160/api/reservatorios/"
reservatorios.info <- read.csv("../data/reservatorios_info.csv")

reservatorio.id <- 12164
pred <- forecast.lake.volume(reservatorio.id)

plot(10^(pred$x),type="l",xlim=c(2012,2018),ylim=c(0,100),xlab = "Tempo",ylab = "Volume do Açude")
lines(10^(pred$mean),col="blue")
lines(10^(pred$lower),col="orange")
lines(10^(pred$upper),col="orange")
