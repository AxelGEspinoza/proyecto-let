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

equipo = "Universidad Catolica" 
anio = "2020"
partido <- partidos(equipo,anio)
for (i in 1:1){
  if(anio == "2009"){
    g_09 <- partido[4]
    e_09 <- partido[5]
    p_09 <- partido[6]
    t_09 <- partido[4]+partido[5]+partido[6]
  }else if(anio == "2010"){
    g_10 <- partido[4]
    t_10 <- partido[4]+partido[5]+partido[6]
    e_10 <- partido[5]
    p_10 <- partido[6]
  }else if(anio ==2011){
    g_11 <- partido[4]
    t_11 <- partido[4]+partido[5]+partido[6]
    e_11 <- partido[5]
    p_11 <- partido[6]
  }else if(anio== 2012){
    g_12 <- partido[4]
    e_12 <- partido[5]
    p_12 <- partido[6]
    t_12 <- partido[4]+partido[5]+partido[6]
  }else if(anio == 2013){
    g_13 <- partido[4]
    e_13 <- partido[5]
    p_13 <- partido[6]
    t_13 <- partido[4]+partido[5]+partido[6]
  }else if(anio == 2014){
    g_14 <- partido[4]
    e_14 <- partido[5]
    p_14 <- partido[6]
    t_14 <- partido[4]+partido[5]+partido[6]
  }else if(anio == 2015){
    g_15 <- partido[4]
    e_15 <- partido[5]
    p_15 <- partido[6]
    t_15 <- partido[4]+partido[5]+partido[6]
  }else if(anio == 2016){
    g_16 <- partido[4]
    e_16 <- partido[5]
    p_16 <- partido[6]
    t_16 <- partido[4]+partido[5]+partido[6]
  }else if(anio == 2017){
    g_17 <- partido[4]
    e_17 <- partido[5]
    p_17 <- partido[6]
    t_17 <- partido[4]+partido[5]+partido[6]
  }else if(anio == 2018){
    g_18 <- partido[4]
    e_18 <- partido[5]
    p_18 <- partido[6]
    t_18 <- partido[4]+partido[5]+partido[6]
  }else if(anio == 2019){
    g_19 <- partido[4]
    e_19 <- partido[5]
    p_19 <- partido[6]
    t_19 <- partido[4]+partido[5]+partido[6]
  }else {
    g_20 <- partido[4]
    e_20 <- partido[5]
    p_20 <- partido[6]
    t_20 <- partido[4]+partido[5]+partido[6]
  }
}

ganados <- rbind(g_09,g_10,g_11,g_12,g_13,g_14,g_15,g_16,g_17,g_18,g_19,g_20)
empatados <- rbind(e_09,e_10,e_11,e_12,e_13,e_14,e_15,e_16,e_17,e_18,e_19,e_20)
perdidos <- rbind(p_09,p_10,p_11,p_12,p_13,p_14,p_15,p_16,p_17,p_18,p_19,p_20)
#media de partidos por temporada

años <- c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)


ggplot() + geom_point(aes(x=años,y=ganados)) + scale_y_continuous(limits = c(0,30)) + scale_x_continuous(breaks = seq(2009,2020,1))+labs(title = "Partidos Ganados UC", subtitle = "Entre los años 2009 y 2020", y = "Partidos Ganados", x = "Años",caption = "Promedio de Partidos por año: 34") + theme_bw()

##CREAR TABLA CON GT
