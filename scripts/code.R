setwd("~/Documentos/Hackfest/dados_snis_tratados")

library(dplyr)
library(lubridate)

boqueirao <- read.csv('volume_boqueirao.csv')
boqueirao <- boqueirao %>%
  rename(data=DataInformacao, fonte=Fonte, volume=Volume, volume.percentual=VolumePercentual) %>%
  select(-fonte) %>%
  mutate(data=dmy(data)) %>%
  mutate(mes = month(data),
         ano = year(data))

write.csv(boqueirao, 'boqueirao_final.csv', row.names = F)

boqueirao_2012 <- boqueirao %>%
  filter(ano >= 2012)

boqueirao_por_mes <- boqueirao %>%
  group_by(ano, mes) %>%
  summarise(volume = median(volume),
            volume.percentual = median(volume.percentual))

boqueirao_por_mes_2012 <- boqueirao_por_mes %>%
  filter(ano >= 2012)
