library(readxl)
library(topmodel)
library(Hmisc)
library(tiff)
library(raster)
library(rgdal)
library(gdata)

data(huagrahuma)
data(huagrahuma.dem)
attach(huagrahuma)
raster<-'shales_clipped2.tif'
dem <-raster(raster)
plot(dem)
dem <-as.matrix(dem)
is.na(dem)
plot(dem)

####################################################################
shale_raster<-raster("shales_clipped2.tif")
plot(shale_raster)
shale_raster

shale_topidx<-topidx(dem, 3)$atb
shale_topidx_class<-make.classes(shale_topidx, 12)
shale_topidx_class
plot(shale_topidx_class)

dem_sink<- sinkfill(dem, 3, degree=0.1)
shale_sink_topidx<-topidx(dem_sink, 10)$atb
shale_sink_topidx_wclass<-make.classes(shale_sink_topidx, 12)
shale_sink_topidx_wclass
plot(shale_sink_topidx_wclass)

#data_aug_oct<-read.csv("2007-08-01_2007-10-31SSHCZOHourPrecipSH.csv")
data_jan_dec<-read.csv("2007-01-01_2007-12-31SSHCZOHourPrecipSH.csv")
#aug_oct_rain<-data_aug_oct$Precip_m
#length(aug_oct_rain)
#aug_oct_rain_noNA <- aug_oct_rain[!is.na(aug_oct_rain)]
#length(aug_oct_rain_noNA)
jan_dec_rain<-data_jan_dec$Precip_m
length(jan_dec_rain)
#jan_dec_rain_noNA <- jan_dec_rain[!is.na(jan_dec_rain)]
#length(jan_dec_rain_noNA)
#aug_oct_flow<-data_aug_oct$hourly_discharge_m.s
jan_dec_flow<-data_jan_dec$hourly_discharge_cms
#jan_dec_flow<-jan_dec_flow[!is.na(jan_dec_flow)]
length(jan_dec_flow)

k<-10
shale_flowlength<-flowlength(dem)*3
shale_delay_class<-make.classes(shale_flowlength,k)
shale_delay_class
plot(shale_delay_class)
shale_delay_classorder<- shale_delay_class[k:1,]
shale_delay_classorder
shale_delay_classorder[,2] <- c(0, cumsum(shale_delay_classorder[1:(k-1),2]))
shale_delay_classorder

jan_dec_sim<-topmodel(parameters, shale_topidx_class, shale_delay_classorder, jan_dec_rain_noNA, ETp)

plot(jan_dec_sim, type="l", col="blue", main="Jan1-Dec31 2007", xlab="Hour", ylab="Discharge (m/s)")
plot(jan_dec_flow, type="l")

legend("topright", legend=c("modeled", "observed"),
       col=c("blue", "black"), lty=1, cex=0.9)

aug_oct_sim<-topmodel(parameters,shale_topidx_class, shale_delay_classorder, aug_oct_rain_noNA, ETp)
plot(aug_oct_sim, type="l", col="blue", main="Aug1-Oct31 2007", xlab="Hour", ylab="Discharge (m/s)")
plot(aug_oct_flow, type="l")

###########################################################################

n <- 1

qs0 <- runif(n, min = 1.141, max = 1.141)
lnTe <- runif(n, min = -2, max = 1.5)
m <- runif(n, min = 0.05, max = 0.05)
Sr0 <- runif(n, min = 0, max = 0.15)
Srmax <- runif(n, min = 0.015, max = 0.1)
vch <-runif(n, min =100, max= 2500)
td <- runif(n, min = 0, max = 0.1)
vr <- runif(n, min = 100, max = 2500)
k0 <- runif(n, min = 0, max = 20)
CD <- runif(n, min = 2, max = 20)
dt <- 1

parameters <- cbind(qs0,lnTe,m,Sr0,Srmax,td,vch,vr,k0,CD,dt)

