#Paquetes ----
library(readxl)
library(readr) 
library(dplyr) 
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)
#Leer dato ----
Datos <- read_csv("Datos/matches.csv")
#Prueba en Colo-Colo ----
Colo_local <- Datos %>% 
  filter(HT == "Colo Colo") %>% 
  mutate(año = case_when(
    str_detect(Colo_local$Date,"2008") ~ as.numeric("2008"),
    str_detect(Colo_local$Date,"2009") ~ as.numeric("2009"),
    str_detect(Colo_local$Date,"2010") ~ as.numeric("2010"),
    str_detect(Colo_local$Date,"2011") ~ as.numeric("2011"),
    str_detect(Colo_local$Date,"2012") ~ as.numeric("2012"),
    str_detect(Colo_local$Date,"2013") ~ as.numeric("2013"),
    str_detect(Colo_local$Date,"2014") ~ as.numeric("2014"),
    str_detect(Colo_local$Date,"2015") ~ as.numeric("2015"),
    str_detect(Colo_local$Date,"2016") ~ as.numeric("2016"),
    str_detect(Colo_local$Date,"2017") ~ as.numeric("2017"),
    str_detect(Colo_local$Date,"2018") ~ as.numeric("2018"),
    str_detect(Colo_local$Date,"2019") ~ as.numeric("2019"),
    str_detect(Colo_local$Date,"2020") ~ as.numeric("2020"),
    str_detect(Colo_local$Date,"2021") ~ as.numeric("2021")
    ))
  

Colo_v <- Datos %>% 
  filter(AT == "Colo Colo") %>% 
  mutate(año = case_when(
    str_detect(Colo_v$Date,"2008") ~ as.numeric("2008"),
    str_detect(Colo_v$Date,"2009") ~ as.numeric("2009"),
    str_detect(Colo_v$Date,"2010") ~ as.numeric("2010"),
    str_detect(Colo_v$Date,"2011") ~ as.numeric("2011"),
    str_detect(Colo_v$Date,"2012") ~ as.numeric("2012"),
    str_detect(Colo_v$Date,"2013") ~ as.numeric("2013"),
    str_detect(Colo_v$Date,"2014") ~ as.numeric("2014"),
    str_detect(Colo_v$Date,"2015") ~ as.numeric("2015"),
    str_detect(Colo_v$Date,"2016") ~ as.numeric("2016"),
    str_detect(Colo_v$Date,"2017") ~ as.numeric("2017"),
    str_detect(Colo_v$Date,"2018") ~ as.numeric("2018"),
    str_detect(Colo_v$Date,"2019") ~ as.numeric("2019"),
    str_detect(Colo_v$Date,"2020") ~ as.numeric("2020"),
    str_detect(Colo_v$Date,"2021") ~ as.numeric("2021")
  ))

#creo dfuncion que lee y suma los puntos y resultados ----
puntos <- function(dato){
  punL = 0
  punV = 0
  gano = 0
  empa = 0
  perdi = 0
  for (i in 1:(length((dato$HT)))){
  if (dato$HS[i] > dato$AS[i]){
    punL = punL + 3
    gano = gano + 1
  }else if(dato$HS[i] == dato$AS[i]){
    punL = punL + 1
    punV = punV + 1
    empa = empa + 1
  }else {
    punV = punV + 3
    perdi = perdi +1
  }}
  pun = c(punL,empa,punV)
  resultados = c(gano,empa,perdi)
  return(c(pun,resultados))
}

local <- Colo_local %>% 
  filter(año == "2008") %>% 
  puntos()

visita <- Colo_v %>% 
  filter(año == "2008") %>% 
  puntos()

total <- c(local[1] + visita[3],local[2] + visita[2],local[3] + visita[1],local[4] + visita[6],local[5] + visita[5],local[6] + visita[4])

rbind(local,visita) #interpretar bien donde esta cada dato

#GENERALIZO LOS DATOS PARA IR VARIANDO EL EQUIPO----
equipo = "Universidad Catolica" #ESCRIBIR BIEN EL NOMBRE DEL EQUIPO


Local <- Datos %>% 
  filter(HT == equipo) %>% 
  mutate(año = case_when(
    str_detect(Local$Date,"2008") ~ as.numeric("2008"),
    str_detect(Local$Date,"2009") ~ as.numeric("2009"),
    str_detect(Local$Date,"2010") ~ as.numeric("2010"),
    str_detect(Local$Date,"2011") ~ as.numeric("2011"),
    str_detect(Local$Date,"2012") ~ as.numeric("2012"),
    str_detect(Local$Date,"2013") ~ as.numeric("2013"),
    str_detect(Local$Date,"2014") ~ as.numeric("2014"),
    str_detect(Local$Date,"2015") ~ as.numeric("2015"),
    str_detect(Local$Date,"2016") ~ as.numeric("2016"),
    str_detect(Local$Date,"2017") ~ as.numeric("2017"),
    str_detect(Local$Date,"2018") ~ as.numeric("2018"),
    str_detect(Local$Date,"2019") ~ as.numeric("2019"),
    str_detect(Local$Date,"2020") ~ as.numeric("2020"),
    str_detect(Local$Date,"2021") ~ as.numeric("2021")
  ))

Visita <- Datos %>% 
  filter(AT == equipo) %>% 
  mutate(año = case_when(
    str_detect(Visita$Date,"2008") ~ as.numeric("2008"),
    str_detect(Visita$Date,"2009") ~ as.numeric("2009"),
    str_detect(Visita$Date,"2010") ~ as.numeric("2010"),
    str_detect(Visita$Date,"2011") ~ as.numeric("2011"),
    str_detect(Visita$Date,"2012") ~ as.numeric("2012"),
    str_detect(Visita$Date,"2013") ~ as.numeric("2013"),
    str_detect(Visita$Date,"2014") ~ as.numeric("2014"),
    str_detect(Visita$Date,"2015") ~ as.numeric("2015"),
    str_detect(Visita$Date,"2016") ~ as.numeric("2016"),
    str_detect(Visita$Date,"2017") ~ as.numeric("2017"),
    str_detect(Visita$Date,"2018") ~ as.numeric("2018"),
    str_detect(Visita$Date,"2019") ~ as.numeric("2019"),
    str_detect(Visita$Date,"2020") ~ as.numeric("2020"),
    str_detect(Visita$Date,"2021") ~ as.numeric("2021")
  ))


#Escribir en el README----
## Los datos fueron obtenidos de la pagina kaggle: copio la url o [kaggle](url) asi si seleccion kaggle abre la url