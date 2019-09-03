library(readxl)
library(topmodel)
library(Hmisc)
library(tiff)
library(raster)
library(rgdal)

data(huagrahuma)
data(huagrahuma.dem)
attach(huagrahuma)

raster<-'hub3_10dem.tif'
dem3 <-raster(raster)
plot(dem3)

dem3 <-as.matrix(dem3)
is.na(dem3)
#plot(dem3)
############################Topo Analysis#############################

hubbard_topidx<-topidx(dem3, 10)$atb
hubbard_topidx_class<-make.classes(hubbard_topidx, 16)
hubbard_topidx_class
plot(hubbard_topidx_class)

hub3<-read.csv("2012_hub3_hourly_data.csv")
hub3_rain<-hub3$Precip_m
hub3_flow<-hub3$streamflow_ms
hub3_ETp<-hub3$ET_mday

k<-10
hubbard_delay_flowlength<-flowlength(dem3)*10
hubbard_delay_class<-make.classes(hubbard_delay_flowlength,k)
hubbard_delay_class
plot(hubbard_delay_class)
hubbard_delay_classorder<- hubbard_delay_class[k:1,]
hubbard_delay_classorder
hubbard_delay_classorder[,2] <- c(0, cumsum(hubbard_delay_classorder[1:(k-1),2]))
hubbard_delay_classorder

################Assigning Specific Parameters from Kyla with low m#######################

n <- 1

qs0 <- runif(n, min = 1.60e-05, max = 1.60e-05)
lnTe <- runif(n, min = -1.3884277, max = -1.3884277)
m <- runif(n, min = 0.3728027, max = 0.3728027)
Sr0 <- runif(n, min = 0.008522949, max = 0.008522949)
Srmax <- runif(n, min = 0.1347168, max = 0.1347168)
td <- runif(n, min = 0.281494141, max = 0.281494141)
vch <- runif(n, min = 1846.9238, max = 1846.9238)
vr <- runif(n, min = 1427.9785, max = 1427.9785)
k0 <- runif(n, min = 0.06545410, max = 0.06545410)
CD <- runif(n, min = 7.02636719, max = 7.02636719)
dt <- 24

parameters <- cbind(qs0,lnTe,m,Sr0,Srmax,td,vch,vr,k0,CD,dt)

################Assigning Specific Parameters from Kyla highest NSE#################

n <- 1

qs0 <- runif(n, min = 2.13e-05, max = 2.13e-05)
lnTe <- runif(n, min = -1.430176, max = -1.430176)
m <- runif(n, min = 0.9254883, max = 0.9254883)
Sr0 <- runif(n, min = 0.006713867, max = 0.006713867)
Srmax <- runif(n, min = 0.1166992, max = 0.1166992)
td <- runif(n, min = 0.2475586, max = 0.2475586)
vch <- runif(n, min = 1392.090, max = 1392.090)
vr <- runif(n, min = 1204.590, max = 1204.590)
k0 <- runif(n, min = 0.1049316, max = 0.1049316)
CD <- runif(n, min = 3.505859, max = 3.505859)
dt <- 24

parameters <- cbind(qs0,lnTe,m,Sr0,Srmax,td,vch,vr,k0,CD,dt)

######################Assigning Specific Parameters#######################
n <- 1

qs0 <- runif(n, min = 0.00002, max = 0.00002)
lnTe <- runif(n, min = -1.8, max = -1.8)
m <- runif(n, min = 0.6, max = 0.6)
Sr0 <- runif(n, min = 1, max = 1)
Srmax <- runif(n, min = 0.02, max = 0.02)
td <- runif(n, min = 0.3, max = 0.3)
vch <- runif(n, min = 100, max = 100)
vr <- runif(n, min = 500, max = 500)
k0 <- runif(n, min = 0.1487, max = 0.1487)
CD <- runif(n, min = 0.428, max = 0.428)
dt <- 24

parameters <- cbind(qs0,lnTe,m,Sr0,Srmax,td,vch,vr,k0,CD,dt)

######################Adjusting Huagrahuma Parameters#######################

n <- 1

qs0 <- runif(n, min = 3.167914e-05, max = 3.167914e-05)
lnTe <- runif(n, min = -5.990615e-01, max = -5.990615e-01)
m <- runif(n, min = 2.129723e-02, max = 2.129723e-02)
Sr0 <- runif(n, min = 2.626373e-03, max = 2.626373e-03)
Srmax <- runif(n, min = 8.683245e-01, max = 8.683245e-01)
td <- runif(n, min = 2.850000e+00, max = 2.850000e+00)
vch <- runif(n, min = 1.000000e+03, max = 1.000000e+03)
vr <- runif(n, min = 1.199171e+03, max = 1.199171e+03)
k0 <- runif(n, min = 9.361053e-03, max = 9.361053e-03)
CD <- runif(n, min = 7.235573e-01, max = 7.235573e-01)
dt <- 1

parameters <- cbind(qs0,lnTe,m,Sr0,Srmax,td,vch,vr,k0,CD,dt)

#######################Hub3 Model Run######################

hub3_sim<-topmodel(parameters, hubbard_topidx_class, hubbard_delay_classorder, hub3_rain, hub3_ETp, TRUE)

Q<-hub3_sim$Q
qo<-hub3_sim$qo
qs<-hub3_sim$qs
S<-hub3_sim$S
fex<-hub3_sim$fex
#Ea<-hub3_sim_1yr$Ea

plot(hub3_may_dec_flow, type='l', main="2012 Hubbard Creek", xlab="Days", ylab="Normalized Flow")
plot(Q, type='l', main="2012 Hubbard Creek using Huagrahuma Parameters", xlab="Days", ylab="Normalized Flow")
lines(hub3_flow, col='blue')
legend("topright", legend=c("modeled", "observed"),
       col=c("black", "blue"), lty=1, cex=0.9)

lines(hub3_may_dec_flow, col='blue')
lines(Q, col='blue')
lines(Q, col='red')
lines(qs, col='green')
lines(Q-qs, col='red')

NSeff(hub3_may_dec_flow, Q)

Q2<- as.numeric(Q)
hub3_may_dec_flow2<- as.numeric(hub3_may_dec_flow)
NSeff(hub3_may_dec_flow2, Q2)

hydroGOF::
  library(hydroGOF)
hydroGOF:: pbias(hub3_may_dec_flow2, Q2)
r.squared

#################Monte Carlo Simulation#########################

n <- 5000

qs0 <- runif(n, min = 0, max = 0.00003)
lnTe <- runif(n, min = -2, max = -0.5)
m <- runif(n, min = 0.3, max = 1)
Sr0 <- runif(n, min = 0.005, max = 0.2)
Srmax <- runif(n, min = 0.1, max = 1)
td <- runif(n, min = 0, max = 0.5)
vch <- runif(n, min = 500, max = 2000)
vr <- runif(n, min = 500, max = 2000)
k0 <- runif(n, min = 0.05, max = 0.2)
CD <- runif(n, min = 0, max = 10)
dt <- 24

parameters <- cbind(qs0,lnTe,m,Sr0,Srmax,td,vch,vr,k0,CD,dt)

hub3_NS<-topmodel(parameters, hubbard_topidx_class, hubbard_delay_classorder, hub3_may_dec_rain, hub3_may_dec_ETp, Qobs=hub3_may_dec_flow)
max(hub3_NS)
min(hub3_NS)

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

##############visualizing parameters with NS#############################
plot(qs0, hub3_NS)
plot(lnTe, hub3_NS, main="NSE vs lnTe for 5000 simulations", ylab = "NSE")
plot(m, hub3_NS, main="NSE vs m for 5000 simulations", ylab="NSE")
plot(Sr0, hub3_NS)
plot(Srmax, hub3_NS)
plot(td, hub3_NS)
plot(vch, hub3_NS)
plot(vr, hub3_NS)
plot(k0, hub3_NS)
plot(CD, hub3_NS)

####################Glue Sensititvty Analysis############################
parameters_glue<- param_kyla[hub3_NS > 0.26,]
NS_glue <- hub3_NS[hub3_NS > 0.26]

Qsim <- topmodel(parameters_glue, hubbard_topidx_class, hubbard_delay_classorder, hub3_may_dec_rain, hub3_may_dec_ETp)
hist(Qsim[1,])

weights <- hub3_NS - 0.2
weights <- weights / sum(weights)

limits <- apply(Qsim, 1, "wtd.quantile", weights = weights,
                probs = c(0.05,0.95), normwt=T)

plot(limits[2,], type="l")
points(limits[1,], type="l")
points(hub3_may_dec_flow, col="red")

outside <- (hub3_may_dec_flow > limits[2,]) | (hub3_may_dec_flow < limits[1,])
summary(outside)

mean(limits[2,] - limits[1,]) / mean(hub3_may_dec_flow, na.rm=T)

###############################Ishrat##########################
table_glue<- cbind(parameters_glue, NS_glue)
#write.csv(table_glue, "C:/Users/Stefany Baron/Documents/GitHub/Aquaholics_Anonymous/TOPMODEL_Hubbard/Table3_hydroinfo.csv")

table_glue_frame<-as.data.frame(table_glue)
hist(table_glue_frame$m)

