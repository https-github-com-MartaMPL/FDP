#**********************************************************************************
#AUTHOR				  :	Marta Padin
#CREATION DATE	: Feb 2019 - Sep 2019
#**********************************************************************************
#DESCRIPTION:   This file is a draft where you can later find how to make Bayesian 
#               seasonal forecasts using the DLM library
#
#**********************************************************************************
#To use this file we need a time series: LEIBmarzo.ts, a data set with all the 
#variables indicated below and the historical observations saved in LEIB_6h.



#Necessary libraries:
library(dlm)
library(tidyverse)
library(lubridate)
library(tseries)
library(ggfortify)
library(broom)
library(forecast)
library(readr)
library(xlsx)
library(readxl)
library(ggplot2)
library(RColorBrewer)
library(fmsb)
library(gridExtra)
library(GGally)

#A priori distribution using data including 2016:

Vel12 <- c(7,8,7,6,4,2,2,1,3,7)
Vel13 <- c(3,10,10,11,9,14,13,13,5,3)
Vel14 <- c(14,10,13,13,13,14,14,19,18,19)
Vel15 <- c(5,5,6,9,10,11,10,9,7,8)
Vel16 <- c(5,4,1,2,3,5,4,1,0,0)
Vel_ibi <- c(Vel12,Vel13,Vel14,Vel15,Vel16)

Dir12 <- c(220,220,220,230,230,230,230,230,240,230)
Dir13 <- c(140,170,170,140,130,150,160,150,110,340)
Dir14 <- c(340,330,330,330,330,340,330,330,330,330)
Dir15 <- c(230,240,230,240,240,240,240,240,240,240)
Dir16 <- c(240,260,260,260,290,310,310,310,0,0)
Dir_ibi <-c(Dir12,Dir13,Dir14,Dir15,Dir16)

Dataibi <- data.frame(Vel_ibi,Dir_ibi)

Dataibi <- Dataibi %>% mutate(EsteOeste=(-Vel_ibi)*sin((Dir_ibi)*pi/180)) %>% 
                       mutate(NorteSur = (-Vel_ibi)*cos((Dir_ibi)*pi/180))
MedianaNS <- median(Dataibi$NorteSur) 
MedianaEO <- median(Dataibi$EsteOeste)
VarNS <- var(Dataibi$NorteSur)
VarEO <- var(Dataibi$EsteOeste)

my_data <- read.delim("misdatos.txt", header = TRUE, sep = "\t")
my_data <- ts(my_data)

#Training observations:
HistLEIB       = "LEIB16.xls"
HistLEIBmarzo <- read_xls(HistLEIB)

#We change the names of the variables:
HistLEIBmarzo %>%
  rename(fecha=`valid`, direccion = `drct`, estacion = `station`,
         v_nudos = `sknt`, hora = `time`) -> His_IB_m

#We build the same structure that we have in the real data of the years 2017/18:
His_IB_m        <-His_IB_m %>% mutate(fecha = as.POSIXct(hora))
His_IB_m$Date   <- as.Date(His_IB_m$fecha)
His_IB_m$anho   <- as.numeric(format(His_IB_m$fecha, "%Y"))
His_IB_m$mes    <- months(His_IB_m$fecha)
His_IB_m$dia    <- as.numeric(format(His_IB_m$fecha, "%d"))
His_IB_m$tiempo <- format(as.POSIXct(strptime(His_IB_m$hora,"%Y-%m-%d %H:%M",tz="")), 
                          format = "%H:%M")
His_IB_m  <- filter(His_IB_m, tiempo %in% obsLEIB)
His_IB_m <- His_IB_m %>% 
  mutate(grupohorario19 = ifelse(tiempo %in% grupo1, 1, grupohorario19)) %>% 
  mutate(grupohorario19 = ifelse(tiempo %in% grupo2, 2, grupohorario19)) %>% 
  mutate(grupohorario19 = ifelse(tiempo %in% grupo3, 3, grupohorario19)) %>% 
  mutate(grupohorario19 = ifelse(tiempo %in% grupo4, 4, grupohorario19))

His_IB_m <- His_IB_m %>% 
  select(fecha, grupohorario19, direccion, v_nudos, dia, mes, anho, Date) %>%
  mutate(direccion=as.numeric(direccion), 
         v_nudos=as.numeric(v_nudos)) %>% 
         arrange(fecha, anho, mes, dia, grupohorario19)

His_IB_m  %>% group_by(dia, mes, anho, grupohorario19, Date) %>% summarise(v_nudos=median(v_nudos,na.rm=TRUE), direccion =median(direccion,na.rm=TRUE)) -> His_IB_m 

His_IB_m <- His_IB_m %>% mutate(EsteOeste=(v_nudos)*sin((direccion)*pi/180)) %>% 
  mutate(NorteSur = (v_nudos)*cos((direccion)*pi/180)) %>%
  mutate(EsteOeste=round(EsteOeste, digits = 1)) %>% 
  mutate(NorteSur=round(NorteSur, digits = 1))
His_IB_m %>% arrange(anho, dia, grupohorario19, mes) -> His_IB_m

#Filter East-West component:
my_data.dlm <- dlm(m0 = MedianaEO, C0 = VarEO , FF = 1, V = 30 , GG = 1, W = 0.9*(30))
data_filter <- dlmFilter(LEIBmarzoVD_EO.ts,mod=my_data.dlm)

#Forecast:
set.seed(5)
data_forecast <-dlmForecast(data_filter, nAhead = 8, sampleNew = 100, method = "svd")



Ib_m_f <- LEIBmarzoVD[,10]
Ib_19m_f <- LEIB19_6h[,9]
IB_bay <- rbind(Ib_m_f,Ib_19m_f)
IB_bay$Dia <- seq(1,372,1)




lapply(data_forecast$newObs, `[[`, 1)-> Hora1
Hora1 <- as.numeric(Hora1)
lapply(data_forecast$newObs, `[[`, 2)-> Hora2
Hora2 <- as.numeric(Hora2)
lapply(data_forecast$newObs, `[[`, 3)-> Hora3
Hora3 <- as.numeric(Hora3)
lapply(data_forecast$newObs, `[[`, 4)-> Hora4
Hora4 <- as.numeric(Hora4)
lapply(data_forecast$newObs, `[[`, 5)-> Hora5
Hora5 <- as.numeric(Hora5)
lapply(data_forecast$newObs, `[[`, 6)-> Hora6
Hora6 <- as.numeric(Hora6)
lapply(data_forecast$newObs, `[[`, 7)-> Hora7
Hora7 <- as.numeric(Hora7)
lapply(data_forecast$newObs, `[[`, 8)-> Hora8
Hora8 <- as.numeric(Hora8)

#Calculation of the quantiles:
quantile(Hora1,c(0.025,0.5,0.975))-> prim
quantile(Hora2,c(0.025,0.5,0.975))-> seg
quantile(Hora3,c(0.025,0.5,0.975))-> ter
quantile(Hora4,c(0.025,0.5,0.975))-> cuart
quantile(Hora5,c(0.025,0.5,0.975))-> quin
quantile(Hora6,c(0.025,0.5,0.975))-> sex
quantile(Hora7,c(0.025,0.5,0.975))-> sep
quantile(Hora8,c(0.025,0.5,0.975))-> oct

#Calculation of IC:

Q025 <- c(prim[1],seg[1],ter[1],cuart[1],quin[1],sex[1],sep[1],oct[1])
Q05  <- c(prim[2],seg[2],ter[2],cuart[2],quin[2],sex[2],sep[2],oct[2])
Q975 <- c(prim[3],seg[3],ter[3],cuart[3],quin[3],sex[3],sep[3],oct[3])

#Representation with forecast:
ggplot()+
  geom_line(aes(x=IB_bay$Dia[150:248],y=IB_bay$EsteOeste[150:248]),linetype = 2)+
  geom_line(aes(x=IB_bay$Dia[249:256],y=IB_bay$EsteOeste[249:256]), color = "red")+
  geom_line(aes(x=IB_bay$Dia[249:256],y=Q025), color = "green")+
  geom_line(aes(x=IB_bay$Dia[249:256],y=Q05), color = "blue")+
  geom_line(aes(x=IB_bay$Dia[249:256],y=Q975), color = "green")+
  ggtitle ("Aeropuerto de Ibiza")+ 
  labs(x = " ",y = "Componente Este - Oeste ")+ 
  theme (plot.title = element_text(size=rel(1.2), vjust=1.5, face="bold", color="black", lineheight=1.5)) + 
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="black",size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="black", size=rel(1)))



