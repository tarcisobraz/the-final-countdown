library(jsonlite)
library(dplyr)
library(lubridate)
# Give the input file name to the function.5
reservatorios.info <- fromJSON("reservatorios_info.json")

reservatorios.info <- reservatorios.info %>%
  mutate(area = as.numeric(area),
         capacidade = as.numeric(capacidade),
         data_informacao = dmy(data_informacao),
         hectares = as.numeric(hectares),
         perimetro = as.numeric(perimetro),
         volume = as.numeric(volume),
         volume_percentual = as.numeric(volume_percentual))
