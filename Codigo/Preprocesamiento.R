#Paquetes ----
library(readxl)
library(readr) 
library(dplyr) 
library(tidyr)
library(stringr)
library(lubridate)
#Leer dato ---- 
Datos <- read_csv("Datos/matches.csv",locale = locale(encoding = "Latin1"))

datos <- Datos %>% 
  mutate(Date = dmy(Date))

write_csv(datos,"Datos/datos desarollo.csv")
