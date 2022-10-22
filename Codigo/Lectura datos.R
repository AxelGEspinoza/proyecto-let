#Paquetes ----
library(readxl)
library(readr) 
library(dplyr) 
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)
#Leer dato ----
Datos <- read_csv("Datos/datos-desarrollo.csv")

#Prueba en Colo-Colo ----
Colo_local <- Datos %>% 
  filter(HT == "Colo Colo")

Colo_v <- Datos %>% 
  filter(AT == "Colo Colo") 
  

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

#PRUEBA LA FUNCION CREADA CON LOS DATOS ANTERIORMENTE PROBADOS
local <- Colo_local %>% 
  filter(año == "2009") %>% 
  puntos()

visita <- Colo_v %>% 
  filter(año == "2009") %>% 
  puntos()

rbind(local,visita) #interpretar bien donde esta cada dato

#CREO UNA TABLA DONDE SE PUEDAN APRECIAR LOS DATOS DE PARTIDOS GANADOS, EMPATADOS Y PERDIDOS EN EL AÑO FILTRADO

total <- c(local[1] + visita[3],local[2] + visita[2],local[3] + visita[1],local[4] + visita[6],local[5] + visita[5],local[6] + visita[4])

#local[1] + visita[3] = PUNTOS GANADOS 
#local[2] + visita[2] = PARTIDOS EMPATADOS
#local[3] + visita[1] = PUNTOS PERDIDOS
#local[4] + visita[6] = PARTIDOS GANADOS
#local[6] + visita[4] = PARTIDOS PERDIDOS

rbind(total)


#GENERALIZO LOS DATOS PARA IR VARIANDO EL EQUIPO----

equipo = "Universidad Catolica" #ESCRIBIR BIEN EL NOMBRE DEL EQUIPO
## PREGUNTAR QUE HACER CON LA Ñ Y LOS ACENTOS
anio = "2009"
partidos <- function(equipo,anio){
  Local <- Datos %>% 
    filter(HT == equipo)  
  
  Visita <- Datos %>% 
    filter(AT == equipo)
  
  local <- Local %>% 
    filter(año == anio) %>% 
    puntos()
  
  visita <- Visita %>% 
    filter(año == anio) %>% 
    puntos()
  
  rbind(local,visita)

  
  total <- c(local[1] + visita[3],local[2] + visita[2],local[3] + visita[1],local[4] + visita[6],local[5] + visita[5],local[6] + visita[4])
  
  return(rbind(total))
}

partido <- partidos("Universidad Catolica","2020")
g_09 <- partido[4]
g_10 <- partido[4]
g_11 <- partido[4]
g_12 <- partido[4]
g_13 <- partido[4]
g_14 <- partido[4]
g_15 <- partido[4]
g_16 <- partido[4]
g_17 <- partido[4]
g_18 <- partido[4]
g_19 <- partido[4]
g_20 <- partido[4]

ganados <- rbind(g_09,g_10,g_11,g_12,g_13,g_14,g_15,g_16,g_17,g_18,g_19,g_20)

t_09 <- partido[4]+partido[5]+partido[6]
t_10 <- partido[4]+partido[5]+partido[6]
t_11 <- partido[4]+partido[5]+partido[6]
t_12 <- partido[4]+partido[5]+partido[6]
t_13 <- partido[4]+partido[5]+partido[6]
t_14 <- partido[4]+partido[5]+partido[6]
t_15 <- partido[4]+partido[5]+partido[6]
t_16 <- partido[4]+partido[5]+partido[6]
t_17 <- partido[4]+partido[5]+partido[6]
t_18 <- partido[4]+partido[5]+partido[6]
t_19 <- partido[4]+partido[5]+partido[6]
t_20 <- partido[4]+partido[5]+partido[6]

#media de partidos por temporada
total <- cbind(t_09,t_10,t_11,t_12,t_13,t_14,t_15,t_16,t_17,t_18,t_19,t_20)
años <- c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

plot(x=años,y=ganados)

ggplot() + geom_point(aes(x=años,y=ganados)) + scale_y_continuous(limits = c(0,30))
