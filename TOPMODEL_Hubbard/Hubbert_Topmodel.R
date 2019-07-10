library(readxl)
library(topmodel)
library(Hmisc)
library(tiff)
library(raster)
library(rgdal)

data(huagrahuma)
data(huagrahuma.dem)
attach(huagrahuma)
raster<-'hubbard_clipped.tif'
dem <-raster(raster)
plot(dem)

dem <-as.matrix(dem)
is.na(dem)
sum(is.na(dem))
length(is.na(dem))

####################################################################
hubbard_raster<-raster("hubbard_clipped.tif")
plot(hubbard_raster)

hubbard_topidx<-topidx(dem, 10)$atb
hubbard_topidx_class<-make.classes(hubbard_topidx, 11)
hubbard_topidx_class
plot(hubbard_topidx_class)

dem_sink<-sinkfill(dem, 10, 0.1)
topidx_sink<-topidx(dem_sink,10)$atb
topidx_sink_class<-make.classes(topidx_sink, 22)
topidx_sink_class
plot(topidx_sink_class)

hubbard_data<-read.csv("2016_hubbert_precip.csv")
hubbard_rain_mm<-hubbard_data$ï..Precip_2016_mm
hubbard_rain_m<-hubbard_rain_mm/1000
hubbard_rain_m
hubbard_flow_ls<-hubbard_data$Streamflow_l.s
hubbard_flow_ms<-hubbard_data$flow_m.s
hubbard_flow_ms

hubbard_data_jan_jun<-read.csv("2016_hubbert_jan1-june30.csv")
jan_june_rain<-hubbard_data_jan_jun$Precip_2016_m
jan_june_flow_mday<-hubbard_data_jan_jun$flow_m.day

hubbard_data_aug_nov<-read.csv("2016_hubbert_aug1_nov31.csv")
aug_nov_rain<-hubbard_data_aug_nov$Precip_2016_m
aug_nov_flow_mday<-hubbard_data_aug_nov$flow_m.day

delay
k<-10
hubbard_delay_flowlength<-flowlength(dem)*10
hubbard_delay_class<-make.classes(hubbard_delay_flowlength,k)
hubbard_delay_class
plot(hubbard_delay_class)
hubbard_delay_classorder<- hubbard_delay_class[k:1,]
hubbard_delay_classorder
hubbard_delay_classorder[,2] <- c(0, cumsum(hubbard_delay_classorder[1:(k-1),2]))
hubbard_delay_classorder

##dem[217,858]

########################full year###########################################
hubbard_sim<-topmodel(parameters, hubbard_topidx_class, hubbard_delay_classorder, hubbard_rain_m, ETp)
hubbard_sim<-topmodel(parameters, topidx_sink_class, hubbard_delay_classorder, hubbard_rain_m, ETp)
plot(hubbard_sim, type="l", col="blue", main="2016 Modeled Daily Flow for Hubbard Creek", xlab="Days", ylab="Flow")
plot(hubbard_flow_ms, type="l", col="blue", main="2016 Observed Daily Flow for Hubbard Creek", xlab="Days", ylab="Flow")
lines(hubbard_sim)
legend("topright", legend=c("modeled", "observed"),
       col=c("blue", "black"), lty=1, cex=0.9)

NSeff(hubbard_flow_ms, hubbard_sim)
##using sinks makes results worse for full year
##############################################################################

########################january-june##########################################
hubbard_sim_jan_jun<-topmodel(parameters, hubbard_topidx_class, hubbard_delay_classorder, jan_june_rain, ETp)
hubbard_sim_jan_jun<-topmodel(parameters, topidx_sink_class, hubbard_delay_classorder, jan_june_rain, ETp)
plot(hubbard_sim_jan_jun, type="l", col="blue", main="2016 Modeled Daily Flow for Hubbard Creek", xlab="Days", ylab="Flow")
plot(jan_june_flow_mday, type="l", col="blue", main="2016 Observed Daily Flow for Hubbard Creek", xlab="Days", ylab="Flow")
lines(jan_june_flow_mday)
lines(hubbard_sim_jan_jun-0.0025)
NSeff(jan_june_flow_mday, hubbard_sim_jan_jun-0.0025)
##using sinks makes results worse for jan-jun
##############################################################################

########################Aug-Nov###############################################
hubbard_sim_aug_nov<-topmodel(parameters, hubbard_topidx_class, hubbard_delay_classorder, aug_nov_rain, ETp)
hubbard_sim_aug_nov<-topmodel(parameters, topidx_sink_class, hubbard_delay_classorder, aug_nov_rain, ETp)
plot(hubbard_sim_aug_nov, type="l", col="blue", main="2016 Modeled Daily Flow for Hubbard Creek", xlab="Days", ylab="Flow")
plot(aug_nov_flow_mday, type="l", col="blue", main="2016 Observed Daily Flow for Hubbard Creek", xlab="Days", ylab="Flow")
lines(aug_nov_flow_mday)
lines(hubbard_sim_aug_nov-0.002)
NSeff(aug_nov_flow_mday, hubbard_sim_aug_nov-0.002)

###################Plotting Huagrahuma#####################################

Qsim <- topmodel(parameters,topidx, delay, rain, ETp)
plot(Qsim, type="l", col="red")
points(Qobs)

###################Assigning Specific Parameter############################
n <- 1

qs0 <- runif(n, min = 0.0001, max = 0.0002)
lnTe <- runif(n, min = -2, max = -0.3)
#lnte not very sensitive
m <- runif(n, min = 0.1, max = 0.1)
#m very sensitive
Sr0 <- runif(n, min = 0, max = 0.15)
Srmax <- runif(n, min = 0.015, max = 0.1)
vch <-runif(n, min =100, max= 2500)
td <- runif(n, min = 0, max = 0.1)
vr <- runif(n, min = 100, max = 2500)
k0 <- runif(n, min = 0.05, max = 0.15)
CD <- runif(n, min = 2, max = 20)
dt <- 24

parameters <- cbind(qs0,lnTe,m,Sr0,Srmax,td,vch,vr,k0,CD,dt)

####################Monte Carlo Parameters for Hubbard######################
n <- 1000

qs0 <- runif(n, min = 0.00012, max = 0.00015)
lnTe <- runif(n, min = -1.7, max = -1)
m <- runif(n, min = 0.08, max = 0.15)
Sr0 <- runif(n, min = 0.05, max = 0.2)
Srmax <- runif(n, min = 0.05, max = 0.09)
td <- runif(n, min = 0, max = 0.3)
vch <- runif(n, min = 100, max = 2500)
vr <- runif(n, min = 100, max = 2500)
k0 <- runif(n, min = 0.05, max = 0.15)
CD <- runif(n, min = 0, max = 5)
dt <- 24

parameters <- cbind(qs0,lnTe,m,Sr0,Srmax,td,vch,vr,k0,CD,dt)

hubbard_NS<-topmodel(parameters, hubbard_topidx_class, hubbard_delay_classorder, hubbard_rain_m, ETp, Qobs=hubbard_flow_ms)
max(hubbard_NS)

hubbard_NS_jan_jun<-topmodel(parameters, hubbard_topidx_class, hubbard_delay_classorder, jan_june_rain, ETp, Qobs=jan_june_flow_mday+0.0025)
max(hubbard_NS_jan_jun)

hubbard_NS_aug_nov<-topmodel(parameters, hubbard_topidx_class, hubbard_delay_classorder, aug_nov_rain, ETp, Qobs=aug_nov_flow_mday+0.002)
max(hubbard_NS_aug_nov)

##############visualizing parameters with NS#############################
plot(qs0, hubbard_NS_jan_jun)
plot(lnTe, hubbard_NS_jan_jun)
plot(m, hubbard_NS_jan_jun)
plot(Sr0, hubbard_NS_jan_jun)
plot(Srmax, hubbard_NS_jan_jun)
plot(td, hubbard_NS_jan_jun)
plot(vch, hubbard_NS_jan_jun)
plot(vr, hubbard_NS_jan_jun)
plot(k0, hubbard_NS_jan_jun)
plot(CD, hubbard_NS_jan_jun)

####################Glue Sensititvty Analysis############################
parameters_glue<- parameters[hubbard_NS_jan_jun > 0.41,]
NS_glue <- hubbard_NS_jan_jun[hubbard_NS_jan_jun > 0.41]

sim_jan_jun_glue<-topmodel(parameters_glue, hubbard_topidx_class, hubbard_delay_classorder, jan_june_rain, ETp)
hist(sim_jan_jun_glue[1,])

weights <- hubbard_NS_jan_jun - 0.2
weights <- weights / sum(weights)

limits <- apply(sim_jan_jun_glue, 1, "wtd.quantile", weights = weights,
                probs = c(0.05,0.95), normwt=T)

plot(limits[2,], type="l")
points(limits[1,], type="l")
points(jan_june_flow_mday, col="red")

outside <- (jan_june_flow_mday > limits[2,]) | (jan_june_flow_mday < limits[1,])
summary(outside)
mean(limits[2,] - limits[1,]) / mean(jan_june_flow_mday, na.rm=T)
########################################################################
