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
  filter(año == "2008") %>% 
  puntos()

visita <- Colo_v %>% 
  filter(año == "2008") %>% 
  puntos()

rbind(local,visita) #interpretar bien donde esta cada dato

#CREO UNA TABLA DONDE SE PUEDAN APRECIAR LOS DATOS DE PARTIDOS GANADOS, EMPATADOS Y PERDIDOS EN EL AÑO FILTRADO

total <- c(local[1] + visita[3],local[2] + visita[2],local[3] + visita[1],local[4] + visita[6],local[5] + visita[5],local[6] + visita[4])

#local[1] + visita[3] = PARTIDOS GANADOS 
#local[2] + visita[2] = PARTIDOS EMPATADOS
#local[3] + visita[1] = PARTIDOS PERDIDOS
#local[4] + visita[6] = PUNTOS GANADOS
#local[6] + visita[4] = PUNTOS PERDIDOS

rbind(total)


#GENERALIZO LOS DATOS PARA IR VARIANDO EL EQUIPO----

equipo = "Universidad Catolica" #ESCRIBIR BIEN EL NOMBRE DEL EQUIPO
## PREGUNTAR QUE HACER CON LA Ñ Y LOS ACENTOS

Local <- Datos %>% 
  filter(HT == equipo)  
  
Visita <- Datos %>% 
  filter(AT == equipo)
 
local <- Local %>% 
  filter(año == "2008") %>% 
  puntos()

visita <- Visita %>% 
  filter(año == "2008") %>% 
  puntos()

rbind(local,visita)


total <- c(local[1] + visita[3],local[2] + visita[2],local[3] + visita[1],local[4] + visita[6],local[5] + visita[5],local[6] + visita[4])

rbind(total)



#Escribir en el README----
## Los datos fueron obtenidos de la pagina kaggle: copio la url o [kaggle](url) asi si seleccion kaggle abre la url