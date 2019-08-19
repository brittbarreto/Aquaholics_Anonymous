library(readxl)
library(topmodel)
library(Hmisc)
library(tiff)
library(raster)
library(rgdal)
library(dplyr)
library(lubridate)

data(huagrahuma)
data(huagrahuma.dem)
attach(huagrahuma)

raster<-'hub7_10dem.tif'
dem7 <-raster(raster)
plot(dem7)

dem7 <-as.matrix(dem7)
is.na(dem7)
#plot(dem7)
############################Topo Analysis#############################

hubbard7_topidx<-topidx(dem7, 10)$atb
hubbard7_topidx_class<-make.classes(hubbard7_topidx, 14)
hubbard7_topidx_class
plot(hubbard7_topidx_class)

hub7<-read.csv("2014_hub7_hourly_data.csv")
#hub7_rain<-hub7$precip_m
#hub7_flow<-hub7$flow_mh
#hub7_ETp<-hub7$Etp

z<-hub7 %>% 
  mutate(Date = mdy(date))

hub7_filter<- z%>%
  filter(Date >= "2014-06-1", 
         Date <= "2014-09-1")

hub7_rain<-hub7_filter$precip_m
hub7_flow<-hub7_filter$flow_mh
hub7_ETp<-hub7_filter$Etp

k<-10
hubbard7_delay_flowlength<-flowlength(dem7)*10
hubbard7_delay_class<-make.classes(hubbard7_delay_flowlength,k)
hubbard7_delay_class
plot(hubbard7_delay_class)
hubbard7_delay_classorder<- hubbard7_delay_class[k:1,]
hubbard7_delay_classorder
hubbard7_delay_classorder[,2] <- c(0, cumsum(hubbard7_delay_classorder[1:(k-1),2]))
hubbard7_delay_classorder

######################Assigning Specific Parameters#######################
n <- 1


qs0 <- runif(n, min = 0.000004, max = 0.000004)
lnTe <- runif(n, min = -1.8, max = -1.8)
m <- runif(n, min = 0.015, max = 0.015)
Sr0 <- runif(n, min = 0.0001, max = 0.0001)
Srmax <- runif(n, min = 0.003, max = 0.003)
td <- runif(n, min = 0.3, max = 0.3)
vch <- runif(n, min = 100, max = 100)
vr <- runif(n, min = 500, max = 500)
k0 <- runif(n, min = 0.1487, max = 0.1487)
CD <- runif(n, min = 0.428, max = 0.428)
dt <- 24

parameters <- cbind(qs0,lnTe,m,Sr0,Srmax,td,vch,vr,k0,CD,dt)

#######################Hub7 Model Run######################

hub7_sim<-topmodel(parameters, hubbard7_topidx_class, hubbard7_delay_classorder, hub7_rain, hub7_ETp, TRUE)

Q<-hub7_sim$Q
qo<-hub7_sim$qo
qs<-hub7_sim$qs
S<-hub7_sim$S
fex<-hub7_sim$fex
#Ea<-hub7_sim$Ea

plot(hub7_flow, type='l', main="2014 Hubbard Creek Subcatchment 7", xlab="Hours", ylab="Normalized Flow")
plot(Q, type='l', main="June 24 - July 7, 2014 Hubbard Creek Subcatchment 7", xlab="Hours", ylab="Normalized Flow")
lines(hub7_flow, col='blue')
legend("topright", legend=c("TOPMODEL", "Alternate Model", "Observed"),
       col=c("blue", "red", "black"), lty=1, cex=0.9)

lines(hub7_flow, col='blue')
lines(Q, col='blue')
lines(Q, col='red')
lines(Q, col='green')
lines(Q-qs, col='red')

NSeff(hub7_flow, Q)

fed_data<-read.csv("hub7_20000_1_decay0.015_modelOutput.csv")

zz<-fed_data %>% 
  mutate(Date = mdy(date))

fed_filter<- zz%>%
  filter(Date >= "2014-06-1", 
         Date <= "2014-09-1")

fed_rain<-fed_filter$P
fed_flow<-fed_filter$model_mmhr
#fed_ETp<-fed_filter$
fed_date<-hub7_filter$Date
fed_time<-hub7_filter$time

lines(fed_flow/1000, col='red')
plot(fed_flow)

length(Q)
length(hub7_flow)
length(fed_flow)
length(fed_rain)
length(fed_date)


brittany_hub7<-cbind(fed_rain, fed_flow, Q, hub7_flow)
write.csv(brittany_hub7, "C:/Users/Stefany Baron/Documents/GitHub/Aquaholics_Anonymous/TOPMODEL_Hubbard_Final/file_for_brittany.csv")

##############Area under the curve(Hydrograph Volume)#############

obs_flow <- function(i) {
  j <- hub7_flow[i]
  return(j)
}
obs_flow(50)
obs_vol<-integrate(obs_flow, 610, 680)

sim_flow<- function(s) {
  t<-Q[s]
  return(t)
}
sim_flow(50)
sim_vol<-integrate(sim_flow, 610, 680)

alt_flow<- function(s) {
  t<-fed_flow[s]/1000
  return(t)
}
alt_flow(50)
alt_vol<-integrate(alt_flow, 610, 680)

(obs_vol$value-sim_vol$value)/obs_vol$value
(obs_vol$value-alt_vol$value)/obs_vol$value




#################Monte Carlo Simulation#########################

n <- 500

qs0 <- runif(n, min = 0.000003, max = 0.000005)
lnTe <- runif(n, min = -1.8, max = -0.3)
m <- runif(n, min = 0.01, max = 0.2)
Sr0 <- runif(n, min = 0.0001, max = 0.001)
Srmax <- runif(n, min = 0.003, max = 0.03)
td <- runif(n, min = 0.1, max = 0.5)
vch <- runif(n, min = 50, max = 500)
vr <- runif(n, min = 50, max = 500)
k0 <- runif(n, min = 0.1, max = 0.5)
CD <- runif(n, min = 0.1, max = 1)
dt <- 24

parameters <- cbind(qs0,lnTe,m,Sr0,Srmax,td,vch,vr,k0,CD,dt)

hub7_NS<-topmodel(parameters, hubbard7_topidx_class, hubbard7_delay_classorder, hub7_rain, hub7_ETp, Qobs=hub7_flow)
max(hub7_NS)
min(hub7_NS)

##############visualizing parameters with NS#############################
plot(qs0, hub7_NS)
plot(lnTe, hub7_NS, main="NSE vs lnTe for 5000 simulations", ylab = "NSE")
plot(m, hub7_NS, main="NSE vs m for 5000 simulations", ylab="NSE")
plot(Sr0, hub7_NS)
plot(Srmax, hub7_NS)
plot(td, hub7_NS)
plot(vch, hub7_NS)
plot(vr, hub7_NS)
plot(k0, hub7_NS)
plot(CD, hub7_NS)

####################Glue Sensititvty Analysis############################
parameters_glue<- parameters[hub7_NS > 0.15,]
parameters_glue

#############################stuff for hydroinformatics##################
library(dplyr)
library(hydroGOF)
hydroGOF::
  
  
  combined_table<- cbind(parameters, hub3_NS)
write.csv(combined_table, "C:/Users/Stefany Baron/Documents/GitHub/Aquaholics_Anonymous/TOPMODEL_Hubbard/Table1_hydroinfo.csv")
hub3_5000_flows<-topmodel(parameters, hubbard_topidx_class, hubbard_delay_classorder, hub3_may_dec_rain, hub3_may_dec_ETp)
hub3_5000_flows_transposed<-t(hub3_5000_flows)
write.csv(hub3_5000_flows_transposed, "C:/Users/Stefany Baron/Documents/GitHub/Aquaholics_Anonymous/TOPMODEL_Hubbard/Table3_hydroinfo.csv")

parameters_kyla<-read.csv("params_from_kyla.csv")

qs0_kyla<-parameters_kyla$qs0
lnTe_kyla<-parameters_kyla$lnTe
m_kyla<-parameters_kyla$m
Sr0_kyla<-parameters_kyla$Sr0
Srmax_kyla<-parameters_kyla$Srmax
td_kyla<-parameters_kyla$td
vch_kyla<-parameters_kyla$vch
vr_kyla<-parameters_kyla$vr
k0_kyla<-parameters_kyla$k0
CD_kyla<-parameters_kyla$CD
dt_kyla<-parameters_kyla$dt

param_kyla<- cbind(qs0_kyla,lnTe_kyla,m_kyla,Sr0_kyla,Srmax_kyla,td_kyla,vch_kyla,vr_kyla,k0_kyla,CD_kyla,dt_kyla)

#param_kyla<- as.matrix.data.frame(parameters_kyla)

hub3_NS<-topmodel(param_kyla, hubbard_topidx_class, hubbard_delay_classorder, hub3_may_dec_rain, hub3_may_dec_ETp, Qobs=hub3_may_dec_flow)
max(hub3_NS)
min(hub3_NS)

hub3_22000_flows<-topmodel(param_kyla, hubbard_topidx_class, hubbard_delay_classorder, hub3_may_dec_rain, hub3_may_dec_ETp)
write.csv(hub3_22000_flows, "C:/Users/Stefany Baron/Documents/GitHub/Aquaholics_Anonymous/TOPMODEL_Hubbard/hub3_22000_flows.csv")
write.csv(hub3_NS, "C:/Users/Stefany Baron/Documents/GitHub/Aquaholics_Anonymous/TOPMODEL_Hubbard/hub3_NS.csv" )

length(hub3_NS)
length(param_kyla)


