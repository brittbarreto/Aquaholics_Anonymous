library(readxl)
library(topmodel)
library(Hmisc)
library(tiff)
library(raster)
library(rgdal)
library(dplyr)
library(lubridate)

data("huagrahuma")

raster<-'w_3_DEM.tif'
demVT <-raster(raster)
plot(demVT)

demVT <-as.matrix(demVT)
is.na(demVT)
#plot(demVT)
############################Topo Analysis#############################

sleepers_topidx<-topidx(demVT, 10)$atb
sleepers_topidx_class<-make.classes(sleepers_topidx, 18)
sleepers_topidx_class
plot(sleepers_topidx_class)

sleepers_data<-read.csv("Sleepers_Data_0401_1026.csv")
#sleepers_rain<-sleepers_data$precip_m
#sleepers_flow<-sleepers_data$flow_mh
#sleepers_ETp<-sleepers_data$Etp

z<-sleepers_data %>% 
  mutate(Date = mdy(date))

sleepers_filter<- z%>%
  filter(Date >= "2017-04-30", 
         Date <= "2017-06-12")

sleepers_rain<-sleepers_filter$precip_m
sleepers_flow<-sleepers_filter$flow_mh
sleepers_ETp<-sleepers_filter$Etp_mm_hour


k<-10
sleepers_delay_flowlength<-flowlength(demVT)*10
sleepers_delay_class<-make.classes(sleepers_delay_flowlength,k)
sleepers_delay_class
plot(sleepers_delay_class)
sleepers_delay_classorder<- sleepers_delay_class[k:1,]
sleepers_delay_classorder
sleepers_delay_classorder[,2] <- c(0, cumsum(sleepers_delay_classorder[1:(k-1),2]))
sleepers_delay_classorder

################Assigning Specific Parameters#######################
n <- 1

qs0 <- runif(n, min = 0.000004, max = 0.000004)
lnTe <- runif(n, min = -1., max = -1.8)
m <- runif(n, min = 0.05, max = 0.05)
Sr0 <- runif(n, min = 0.0001, max = 0.0001)
Srmax <- runif(n, min = 0.003, max = 0.003)
td <- runif(n, min = 0.3, max = 0.3)
vch <- runif(n, min = 100, max = 100)
vr <- runif(n, min = 500, max = 500)
k0 <- runif(n, min = 0.1487, max = 0.1487)
CD <- runif(n, min = 0.428, max = 0.428)
dt <- 24

parameters <- cbind(qs0,lnTe,m,Sr0,Srmax,td,vch,vr,k0,CD,dt)

######################Glue Parameteres###############################
n <- 1

qs0 <- runif(n, min =  3.926510e-06, max =  3.926510e-06)
lnTe <- runif(n, min = -1.196998e+00, max = -1.196998e+00)
m <- runif(n, min = 2.787428e-02 , max = 2.787428e-02 )
Sr0 <- runif(n, min = 8.382271e-04, max = 8.382271e-04)
Srmax <- runif(n, min = 4.538280e-03, max = 4.538280e-03)
td <- runif(n, min = 1.650550e-01, max = 1.650550e-01)
vch <- runif(n, min = 1.074347e+02, max = 1.074347e+02)
vr <- runif(n, min = 2.012713e+02, max = 2.012713e+02)
k0 <- runif(n, min = 2.101055e-01, max = 2.101055e-01)
CD <- runif(n, min = 4.125508e+00, max = 4.125508e+00)
dt <- 24

parameters <- cbind(qs0,lnTe,m,Sr0,Srmax,td,vch,vr,k0,CD,dt)


#######################Sleepers Model Run######################

sleep_sim<-topmodel(parameters, sleepers_topidx_class, sleepers_delay_classorder, sleepers_rain, sleepers_ETp, TRUE)

Q<-sleep_sim$Q
qo<-sleep_sim$qo
qs<-sleep_sim$qs
S<-sleep_sim$S

plot(sleepers_flow, type='l', main="2017 Sleepers River", xlab="Hours", ylab="Normalized Flow")
plot(Q, type='l', main="2017 Sleepers River", xlab="Hours", ylab="Normalized Flow")
lines(sleepers_flow, col='blue')
legend("topright", legend=c("modeled", "observed"),
       col=c("black", "blue"), lty=1, cex=0.9)

lines(sleepers_flow, col='blue')
lines(Q, col='blue')
lines(fed_flow, col='red')
lines(Q, col='green')
lines(qs, col='red')

NSeff(sleepers_flow, Q)

output_date<- as.data.frame(sleepers_filter$Date)
output_time<-as.data.frame(sleepers_filter$Time)
output_observed<-as.data.frame(sleepers_filter$flow_mh)
output_Q<-as.data.frame(sleep_sim$Q)


outputs<-dplyr::bind_cols(output_date, output_time, output_observed, output_Q)
write.csv(outputs, "C:/Users/Stefany Baron/Documents/GitHub/Aquaholics_Anonymous/TOPMODEL_Sleepers_VT/Brittany_sleepers.csv" )


##############Area under the curve(Hydrograph Volume)#############

fed_output<-read.csv("sleepers_20000_1_decay0.02787428_modelOutput.csv")

zz<-fed_output %>% 
  mutate(Date = mdy(date))

fed_output_filter<- zz%>%
  filter(Date >= "2017-04-30", 
         Date <= "2017-6-12")

fed_flow<-fed_output_filter$model_mmhr/1000

plot(fed_flow, type='l')
lines(Q, col='blue')
lines(sleepers_flow, col="red")
lines(fed_flow, col="green")
plot(fed_flow, type = "l")

length(fed_flow)
length(Q)
length(sleepers_flow)

brittany_file<-cbind(fed_flow, Q, sleepers_flow)
write.csv(brittany_file, "C:/Users/Stefany Baron/Documents/GitHub/Aquaholics_Anonymous/TOPMODEL_Sleepers_VT/brittany_file.csv" )


fed_sim_flow<- function(s) {
  t<-fed_flow[s]
  return(t)
}

integrate(fed_sim_flow, 450,460)

fed_vol<-c()
for ( i in 4:1000){ 
  start1<-i-3
  end1<-i+3
  fed_volume<-integrate(fed_sim_flow, start1, end1)$value
  fed_vol[[i]]<-fed_volume
}
fed_vol
length(fed_vol)

sim_flow<- function(s) {
  t<-Q[s]
  return(t)
}

sim_vol<-c()
for (u in 4:1000){
  start2<-u-3
  end2<-u+3
  sim_volume<-integrate(sim_flow, start2, end2)$value
  sim_vol[[u]]<-sim_volume
}
sim_vol
length(sim_vol)

obs_flow <- function(i) {
  j <- sleepers_flow[i]
  return(j)
}

obs_vol<-c()
for(h in 4:1000) {
  start3<-h-3
  end3<-h+3
  obs_volume<-integrate(obs_flow, start3, end3)$value
  obs_vol[[h]]<-obs_volume
}
obs_vol
length(obs_vol)

vol_table<-cbind(fed_vol, sim_vol, obs_vol)
vol_table<-as.data.frame(vol_table)

fed_error<-((vol_table$fed_vol-vol_table$obs_vol)/vol_table$obs_vol)*100
sim_error<-((vol_table$sim_vol-vol_table$obs_vol)/vol_table$obs_vol)*100

hist(fed_error, breaks=20, main = "Distribution of Error in Volume for Alternate Model", xlab = "% Error")
hist(sim_error, main = "Distribution of Error in Volume for TOPMODEL", xlab = "% Error")

integrate(fed_sim_flow, 757, 794)$value
integrate(sim_flow, 757, 794)$value
integrate(obs_flow, 757, 794)$value


#################Monte Carlo Simulation#########################

n <- 5000

qs0 <- runif(n, min = 0.000002, max = 0.000004)
lnTe <- runif(n, min = -1.8, max = -0.3)
m <- runif(n, min = 0.01, max = 0.2)
Sr0 <- runif(n, min = 0.0001, max = 0.001)
Srmax <- runif(n, min = 0.003, max = 0.03)
td <- runif(n, min = 0.1, max = 0.5)
vch <- runif(n, min = 50, max = 200)
vr <- runif(n, min = 100, max = 600)
k0 <- runif(n, min = 0.1, max = 0.5)
CD <- runif(n, min = 0.1, max = 5)
dt <- 24

parameters <- cbind(qs0,lnTe,m,Sr0,Srmax,td,vch,vr,k0,CD,dt)

sleep_NS<-topmodel(parameters, sleepers_topidx_class, sleepers_delay_classorder, sleepers_rain, sleepers_ETp, Qobs=sleepers_flow)
max(sleep_NS)
min(sleep_NS)

#############################stuff for hydroinformatics##################
library(dplyr)
library(hydroGOF)

combined_table<- cbind(parameters, sleep_NS)
write.csv(combined_table, "C:/Users/Stefany Baron/Documents/GitHub/Aquaholics_Anonymous/TOPMODEL_Sleepers_VT/Table1_hydroinfo.csv")
sleep_5000_flows<-topmodel(parameters, sleepers_topidx_class, sleepers_delay_classorder, sleepers_rain, sleepers_ETp)
sleep_5000_flows_transposed<-t(sleep_5000_flows)
write.csv(sleep_5000_flows_transposed, "C:/Users/Stefany Baron/Documents/GitHub/Aquaholics_Anonymous/TOPMODEL_Sleepers_VT/Table2_hydroinfo.csv")

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
plot(qs0, sleep_NS)
plot(lnTe, sleep_NS, main="NSE vs lnTe for 5000 simulations", ylab = "NSE")
plot(m, sleep_NS, main="NSE vs m for 5000 simulations", ylab="NSE")
plot(Sr0, sleep_NS)
plot(Srmax, sleep_NS)
plot(td, sleep_NS)
plot(vch, sleep_NS)
plot(vr, sleep_NS)
plot(k0, sleep_NS)
plot(CD, sleep_NS)

####################Glue Sensititvty Analysis############################
parameters_glue<- parameters[sleep_NS > 0.55,]
parameters_glue
NS_glue <- sleep_NS[sleep_NS > 0.55]

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
table_glue_frame<-as.data.frame(table_glue)
hist(table_glue_frame$m)
