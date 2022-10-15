#Paquetes ----
library(readxl)
library(readr) 
library(dplyr) 
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)
#Leer dato ----
Datos <- read_csv("Datos/datos sin procesar.csv")

datos <- Datos %>% mutate(año= case_when(
  str_detect(datos$Date,"2008") ~ as.numeric("2008"),
  str_detect(datos$Date,"2009") ~ as.numeric("2009"),
  str_detect(datos$Date,"2010") ~ as.numeric("2010"),
  str_detect(datos$Date,"2011") ~ as.numeric("2011"),
  str_detect(datos$Date,"2012") ~ as.numeric("2012"),
  str_detect(datos$Date,"2013") ~ as.numeric("2013"),
  str_detect(datos$Date,"2014") ~ as.numeric("2014"),
  str_detect(datos$Date,"2015") ~ as.numeric("2015"),
  str_detect(datos$Date,"2016") ~ as.numeric("2016"),
  str_detect(datos$Date,"2017") ~ as.numeric("2017"),
  str_detect(datos$Date,"2018") ~ as.numeric("2018"),
  str_detect(datos$Date,"2019") ~ as.numeric("2019"),
  str_detect(datos$Date,"2020") ~ as.numeric("2020"),
  str_detect(datos$Date,"2021") ~ as.numeric("2021")
))

write_csv(datos,"Datos/datos-desarollo.csv")
