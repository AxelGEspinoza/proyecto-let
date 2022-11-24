#Paquetes ----
library(readr)
library(dplyr) 
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)
library(gt)
library(gtsummary)
library(broom)
library(lubridate)
#Leer dato ----
Datos <- read_csv("Datos/datos-desarrollo.csv")

#creo dfuncion que lee y suma los puntos y resultados ----
puntos <- function(dato){
  gano = 0
  empa = 0
  perdi = 0
  for (i in 1:(length((dato$HT)))){
  if (dato$HS[i] > dato$AS[i]){
    gano = gano + 1
  }else if(dato$HS[i] == dato$AS[i]){
    empa = empa + 1
  }else {
    perdi = perdi +1
  }}
  resultados = c(gano,empa,perdi)
  return(resultados)
}


#GENERALIZO LOS DATOS PARA IR VARIANDO EL EQUIPO----
partidos <- function(equipo,anio){
  Local <- Datos %>% 
    filter(HT == equipo)  
  
  Visita <- Datos %>% 
    filter(AT == equipo)
  
  local <- Local %>% 
    filter(year(Date) == anio) %>% 
    puntos()
  
  visita <- Visita %>% 
    filter(year(Date) == anio) %>% 
    puntos()
  
  total <- c(local[1] + visita[3],local[2] + visita[2],local[3] + visita[1])
  
  return(rbind(total))
}

localia <- function(equipo,anio){
  Local <- Datos %>% 
    filter(HT == equipo)  
  
  Visita <- Datos %>% 
    filter(AT == equipo)
  
  local <- Local %>% 
    filter(year(Date) == anio) %>% 
    puntos()
  
  visita <- Visita %>% 
    filter(year(Date) == anio) %>% 
    puntos()
  return(rbind(local,visita))
}
equipo = "Colo Colo" 

for (i in 2009:2020){
  anio=i
  partido <- partidos(equipo,anio)
  lovi <- localia(equipo,anio)
  if(anio == "2009"){
    g_09 <- partido[1]
    e_09 <- partido[2]
    p_09 <- partido[3]
    t_09 <- partido[1]+partido[2]+partido[3]
    l_09 <- lovi[1]/(lovi[1]+lovi[3]+lovi[5])
    v_09 <- lovi[6]/(lovi[2]+lovi[4]+lovi[6])
  }else if(anio == "2010"){
    g_10 <- partido[1]
    t_10 <- partido[1]+partido[2]+partido[3]
    e_10 <- partido[2]
    p_10 <- partido[3]
    l_10 <- lovi[1]/(lovi[1]+lovi[3]+lovi[5])
    v_10 <- lovi[6]/(lovi[2]+lovi[4]+lovi[6])
  }else if(anio == "2011"){
    g_11 <- partido[1]
    t_11 <- partido[1]+partido[2]+partido[3]
    e_11 <- partido[2]
    p_11 <- partido[3]
    l_11 <- lovi[1]/(lovi[1]+lovi[3]+lovi[5])
    v_11 <- lovi[6]/(lovi[2]+lovi[4]+lovi[6])
  }else if(anio== "2012"){
    g_12 <- partido[1]
    e_12 <- partido[2]
    p_12 <- partido[3]
    t_12 <- partido[1]+partido[2]+partido[3]
    l_12 <- lovi[1]/(lovi[1]+lovi[3]+lovi[5])
    v_12 <- lovi[6]/(lovi[2]+lovi[4]+lovi[6])
  }else if(anio == "2013"){
    g_13 <- partido[1]
    e_13 <- partido[2]
    p_13 <- partido[3]
    t_13 <- partido[1]+partido[2]+partido[3]
    l_13 <- lovi[1]/(lovi[1]+lovi[3]+lovi[5])
    v_13 <- lovi[6]/(lovi[2]+lovi[4]+lovi[6])
  }else if(anio == "2014"){
    g_14 <- partido[1]
    e_14 <- partido[2]
    p_14 <- partido[3]
    t_14 <- partido[1]+partido[2]+partido[3]
    l_14 <- lovi[1]/(lovi[1]+lovi[3]+lovi[5])
    v_14 <- lovi[6]/(lovi[2]+lovi[4]+lovi[6])
  }else if(anio == "2015"){
    g_15 <- partido[1]
    e_15 <- partido[2]
    p_15 <- partido[3]
    t_15 <- partido[1]+partido[2]+partido[3]
    l_15 <- lovi[1]/(lovi[1]+lovi[3]+lovi[5])
    v_15 <- lovi[6]/(lovi[2]+lovi[4]+lovi[6])
  }else if(anio == "2016"){
    g_16 <- partido[1]
    e_16 <- partido[2]
    p_16 <- partido[3]
    t_16 <- partido[1]+partido[2]+partido[3]
    l_16 <- lovi[1]/(lovi[1]+lovi[3]+lovi[5])
    v_16 <- lovi[6]/(lovi[2]+lovi[4]+lovi[6])
  }else if(anio == "2017"){
    g_17 <- partido[1]
    e_17 <- partido[2]
    p_17 <- partido[3]
    t_17 <- partido[1]+partido[2]+partido[3]
    l_17 <- lovi[1]/(lovi[1]+lovi[3]+lovi[5])
    v_17 <- lovi[6]/(lovi[2]+lovi[4]+lovi[6])
  }else if(anio == "2018"){
    g_18 <- partido[1]
    e_18 <- partido[2]
    p_18 <- partido[3]
    t_18 <- partido[1]+partido[2]+partido[3]
    l_18 <- lovi[1]/(lovi[1]+lovi[3]+lovi[5])
    v_18 <- lovi[6]/(lovi[2]+lovi[4]+lovi[6])
  }else if(anio == "2019"){
    g_19 <- partido[1]
    e_19 <- partido[2]
    p_19 <- partido[3]
    t_19 <- partido[1]+partido[2]+partido[3]
    l_19 <- lovi[1]/(lovi[1]+lovi[3]+lovi[5])
    v_19 <- lovi[6]/(lovi[2]+lovi[4]+lovi[6])
  }else {
    g_20 <- partido[1]
    e_20 <- partido[2]
    p_20 <- partido[3]
    t_20 <- partido[1]+partido[2]+partido[3]
    l_20 <- lovi[1]/(lovi[1]+lovi[3]+lovi[5])
    v_20 <- lovi[6]/(lovi[2]+lovi[4]+lovi[6])
  }
}

mean(t_09,t_10,t_11,t_12,t_13,t_14,t_15,t_16,t_17,t_18,t_19,t_20)
for (i in 1:1){
  if (equipo == "Colo Colo"){
    ganados_cc <- c(g_09,g_10,g_11,g_12,g_13,g_14,g_15,g_16,g_17,g_18,g_19,g_20) 
    empatados_cc<- c(e_09,e_10,e_11,e_12,e_13,e_14,e_15,e_16,e_17,e_18,e_19,e_20)
    perdidos_cc <- c(p_09,p_10,p_11,p_12,p_13,p_14,p_15,p_16,p_17,p_18,p_19,p_20)
    totales_cc<- c(t_09,t_10,t_11,t_12,t_13,t_14,t_15,t_16,t_17,t_18,t_19,t_20)
    glocal_cc <- c(l_09,l_10,l_11,l_12,l_13,l_14,l_15,l_16,l_17,l_18,l_19,l_20)
    gvisita_cc <-c(v_09,v_10,v_11,v_12,v_13,v_14,v_15,v_16,v_17,v_18,v_19,v_20)
  }else if (equipo == "Universidad Catolica"){
    ganados_uc <- c(g_09,g_10,g_11,g_12,g_13,g_14,g_15,g_16,g_17,g_18,g_19,g_20) 
    empatados_uc<- c(e_09,e_10,e_11,e_12,e_13,e_14,e_15,e_16,e_17,e_18,e_19,e_20)
    perdidos_uc <- c(p_09,p_10,p_11,p_12,p_13,p_14,p_15,p_16,p_17,p_18,p_19,p_20)
    totales_uc<- c(t_09,t_10,t_11,t_12,t_13,t_14,t_15,t_16,t_17,t_18,t_19,t_20)
    glocal_uc <- c(l_09,l_10,l_11,l_12,l_13,l_14,l_15,l_16,l_17,l_18,l_19,l_20)
    gvisita_uc <-c(v_09,v_10,v_11,v_12,v_13,v_14,v_15,v_16,v_17,v_18,v_19,v_20)
  }else{
    ganados_u <- c(g_09,g_10,g_11,g_12,g_13,g_14,g_15,g_16,g_17,g_18,g_19,g_20) 
    empatados_u<- c(e_09,e_10,e_11,e_12,e_13,e_14,e_15,e_16,e_17,e_18,e_19,e_20)
    perdidos_u <- c(p_09,p_10,p_11,p_12,p_13,p_14,p_15,p_16,p_17,p_18,p_19,p_20)
    totales_u<- c(t_09,t_10,t_11,t_12,t_13,t_14,t_15,t_16,t_17,t_18,t_19,t_20)
    glocal_u <- c(l_09,l_10,l_11,l_12,l_13,l_14,l_15,l_16,l_17,l_18,l_19,l_20)
    gvisita_u <-c(v_09,v_10,v_11,v_12,v_13,v_14,v_15,v_16,v_17,v_18,v_19,v_20)
  }
}

# Graficos de rendimiento local y visita
U <- data.frame(Local=(glocal_u)*100,Visita=(gvisita_u)*100)
CC <- data.frame(Local=(glocal_cc)*100,Visita=(gvisita_cc)*100)
UC <- data.frame(Local=(glocal_uc)*100,Visita=(gvisita_uc)*100)

ggplot(U, aes(años,Local,fill="mediumblue")) + geom_col() + 
  scale_x_continuous(breaks = seq(2009,2020,1)) + 
  scale_fill_manual(values="mediumblue") + 
  scale_y_continuous(limits = c(0,100)) + 
  theme(legend.position='none') + 
  labs(title = "Rendimiento partidos local Universidad de Chile",subtitle = "Entre los años 2009 y 2020", y = "Porcentaje", x = NULL) +
  theme_bw() +
  theme(legend.position='none')

ggplot(U, aes(años,Visita,fill="mediumblue")) + geom_col() + 
  scale_x_continuous(breaks = seq(2009,2020,1)) + 
  scale_fill_manual(values="mediumblue") + 
  scale_y_continuous(limits = c(0,100)) + 
  theme(legend.position='none') + 
  labs(title = "Rendimiento partidos visita Universidad de Chile",subtitle = "Entre los años 2009 y 2020", y = "Porcentaje", x = NULL)+
  theme_bw() +
  theme(legend.position='none')


ggplot(CC, aes(años,Local,fill="mediumblue")) + geom_col() + 
  scale_x_continuous(breaks = seq(2009,2020,1)) + 
  scale_fill_manual(values="gray0") + 
  scale_y_continuous(limits = c(0,100)) + 
  theme(legend.position='none') + 
  labs(title = "Rendimiento partidos local Colo Colo",subtitle = "Entre los años 2009 y 2020", y = "Porcentaje", x = NULL) + 
  theme_bw() +
  theme(legend.position='none')

ggplot(CC, aes(años,Visita,fill="mediumblue")) + geom_col() + 
  scale_x_continuous(breaks = seq(2009,2020,1)) + 
  scale_fill_manual(values="gray0") + 
  scale_y_continuous(limits = c(0,100)) + 
  labs(title = "Rendimiento partidos visita Colo Colo",subtitle = "Entre los años 2009 y 2020", y = "Porcentaje", x = NULL) + 
  theme_bw() +
  theme(legend.position='none')

require(gridExtra)

grid.arrange(p1, p2, mfrow=2)
p1 <- ggplot(UC, aes(años,Local,fill="turquoise1")) + geom_col() + 
  scale_x_continuous(breaks = seq(2009,2020,1)) + 
  scale_fill_manual(values="turquoise1") + 
  scale_y_continuous(limits = c(0,100)) + 
  theme(legend.position='none') + 
  labs(title = "Rendimiento partidos local Universidad Catolica",subtitle = "Entre los años 2009 y 2020", y = "Porcentaje", x = NULL) + 
  theme_bw() +
  theme(legend.position='none')

p2 <- ggplot(UC, aes(años,Visita,fill="turquoise1")) + geom_col() + 
  scale_x_continuous(breaks = seq(2009,2020,1)) + 
  scale_fill_manual(values="turquoise1") + 
  scale_y_continuous(limits = c(0,100)) + 
  theme(legend.position='none') + 
  labs(title = "Rendimiento partidos visita Universidad Catolica",subtitle = "Entre los años 2009 y 2020", y = "Porcentaje", x = NULL) + 
  theme_bw() +
  theme(legend.position='none')

promediouc <- data.frame(mean(glocal_uc),mean(gvisita_uc)) %>% 
  gt() %>%
  tab_header(title = "Rendimiento UC") %>% 
  cols_label(mean.glocal_uc. = "Local",
             mean.gvisita_uc. = "Visita") 

promediou <- data.frame(mean(glocal_u),mean(gvisita_u)) %>% 
  gt() %>%
  tab_header(title = "Rendimiento U") %>% 
  cols_label(mean.glocal_u. = "Local",
             mean.gvisita_u. = "Visita")

promediocc <- data.frame(mean(glocal_cc),mean(gvisita_cc)) %>% 
  gt() %>%
  tab_header(title = "Rendimiento CC") %>% 
  cols_label(mean.glocal_cc. = "Local",
             mean.gvisita_cc. = "Visita")

#media de partidos por temporada

años <- c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
Equipo <- c("Universidad de Chile","Colo Colo","Universidad Catolica")

ganados <- data.frame(año=años,U=ganados_u,CC=ganados_cc,UC=ganados_uc)

##Grafico para comparar resultados o rendimiento
ggplot(ganados,aes(año,UC,color = Equipo[3])) + geom_line(lwd = 1) + 
  geom_line(aes(año,CC,color=Equipo[2]),lwd = 1) + 
  geom_line(aes(año,U,color=Equipo[1]),lwd = 1) +
  geom_vline(xintercept = 2013, color = "red",linetype = 2)+
  geom_text(aes(2012,5,label = "Fin de Playoffs"),stat = "unique",color="red")+
  geom_vline(xintercept = 2017, color = "green",linetype = 2)+
  geom_text(aes(2015.75,5,label = "Inicio Torneo Largo"),stat = "unique",color="green")+
  scale_y_continuous(limits = c(0,30))+
  scale_x_continuous(breaks = seq(2009,2020,1)) +
  scale_color_manual(values = c("gray0","turquoise1","mediumblue"))+
  labs(title = "Figura 1. Partidos Ganados por los tres equipos más populares", subtitle = "Entre los años 2009 y 2020", y = "Partidos Ganados", x = NULL,color="Equipo:",caption = "Fuente: Elaboración propia a partir de datos disponibles en Kaggle") + 
  theme_bw() +
  theme(legend.position = "bottom")


##Graficos por separado
ggplot(ganados,aes(año,UC)) + geom_line(color = "turquoise1")+geom_point(color = "darkblue") + scale_y_continuous(limits = c(0,30)) + scale_x_continuous(breaks = seq(2009,2020,1))+labs(title = "Figura 2. Partidos Ganados UC", subtitle = "Entre los años 2009 y 2020", y = "Partidos Ganados", x = NULL,caption = "Promedio de Partidos por año: 34") + theme_bw()

ggplot(ganados,aes(año,CC)) + geom_line(color = "gray0")+geom_point(color = "darkblue") + scale_y_continuous(limits = c(0,30)) + scale_x_continuous(breaks = seq(2009,2020,1))+labs(title = "Figura 3. Partidos Ganados Colo Colo", subtitle = "Entre los años 2009 y 2020", y = "Partidos Ganados", x = NULL,caption = "Promedio de Partidos por año: 34") + theme_bw()

ggplot(ganados,aes(año,U)) + geom_line(color = "mediumblue")+geom_point(color = "darkblue") + scale_y_continuous(limits = c(0,30)) + scale_x_continuous(breaks = seq(2009,2020,1))+labs(title = "Figura 4. Partidos Ganados U de Chile", subtitle = "Entre los años 2009 y 2020", y = "Partidos Ganados", x = NULL,caption = "Promedio de Partidos por año: 34") + theme_bw()

##CREAR TABLA CON GT

tabla_cc <- data.frame(años,cbind(ganados_cc,empatados_cc,perdidos_cc,totales_cc)) %>% gt() %>% tab_header(title = "Rendimiento CC",subtitle = "Entre los años 2009 y 2020") %>% 
  cols_label(años = "Años", ganados_cc = "Ganados",empatados_cc = "Empatados",perdidos_cc = "Perdidos", totales_cc = "Total Partidos")


tabla_uc <- data.frame(años,cbind(ganados_uc,empatados_uc,perdidos_uc,totales_uc)) %>% 
  gt() %>%
  tab_header(title = "Rendimiento UC",subtitle = "Entre los años 2009 y 2020") %>% 
  cols_label(años = "Años", ganados_uc = "Ganados",empatados_uc = "Empatados",perdidos_uc = "Perdidos", totales_uc = "Total Partidos")


tabla_u <- data.frame(años,cbind(ganados_u,empatados_u,perdidos_u,totales_u)) %>% gt() %>% tab_header(title = "Rendimiento U",subtitle = "Entre los años 2009 y 2020") %>% 
  cols_label(años = "Años", ganados_u = "Ganados",empatados_u = "Empatados",perdidos_u = "Perdidos", totales_u = "Total Partidos")
##Enfrentamientos
## CC VS UC
ccvsuc <- Datos %>% 
  filter(HT == "Colo Colo", AT == "Universidad Catolica")
ucvscc <- Datos %>% 
  filter(HT == "Universidad Catolica",AT == "Colo Colo")

re <- puntos(ccvsuc)
er <- puntos(ucvscc)

d1<- c(re[4] + er[6],re[5] + er[5],re[6] + er[4])
cc <- c("Ganados CC", "Empatados", "Ganados UC")
data.frame(cc,d1)

tabla <- data.frame(cc,d1) %>% gt() %>% 
  tab_header(title = "Enfrentamientos directos CC vs UC",subtitle = "Entre los años 2009 y 2020") %>% 
  cols_label(cc = "", d1 = "Cantidad")
  
tabla

## CC VS U
ccvsu <- Datos %>% 
  filter(HT == "Colo Colo", AT == "Universidad de Chile")
uvscc <- Datos %>% 
  filter(HT == "Universidad de Chile",AT == "Colo Colo")

ucc <- puntos(ccvsu)
ccu <- puntos(uvscc)

d2 <- c(ucc[4] + ccu[6],ucc[5] + ccu[5],ucc[6] + ccu[4])
cu <- c("Ganados CC", "Empatados", "Ganados U")

tabla2 <- data.frame(cu,d2) %>% gt() %>% 
  tab_header(title = "Enfrentamientos directos CC vs U",subtitle = "Entre los años 2009 y 2020") %>% 
  cols_label(cu = "", d2 = "Cantidad")

## U VS UC
ucvsu <- Datos %>% 
  filter(HT == "Universidad Catolica", AT == "Universidad de Chile")
uvsuc <- Datos %>% 
  filter(HT == "Universidad de Chile",AT == "Universidad Catolica")

ucu <- puntos(ucvsu)
uuc <- puntos(uvsuc)

d3 <- c(ucu[4] + uuc[6],ucu[5] + uuc[5],ucu[6] + uuc[4])
uc <- c("Ganados UC", "Empatados", "Ganados U")

tabla3 <- data.frame(uc,d3) %>% gt() %>% 
  tab_header(title = "Enfrentamientos directos UC vs U",subtitle = "Entre los años 2009 y 2020") %>% 
  cols_label(uc = "", d3 = "Cantidad")
