library(readxl)
library(topmodel)
library(Hmisc)
library(tiff)
library(raster)
library(rgdal)

data(huagrahuma)
data(huagrahuma.dem)
attach(huagrahuma)
##getwd("C:/summer_institute_2019/Scaling_theme/R_Files/TOPMODEL/")
raster<-'hubbard_clipped.tif'
dem <-raster(raster)
dem <-as.matrix(dem)
is.na(dem)
#new_dem=remove.na(dem, iftell = TRUE)

raster2<-'topidx_hubbard_madeinGIS.tif'
dem2 <-raster(raster2)
plot(dem2)
dem2 <-as.matrix(dem2)
is.na(dem2)
#new_dem2=remove.na(dem2, iftell = TRUE)

##creek_topoindex_atb<-topidx(dem, 10)$atb
##expss::if_na(creek_topoindex_atb, label = NULL) <- 9999
##atb<-unmatrix(creek_topoindex_atb, byrow=T)
##atb<-as.vector(creek_topoindex_atb)
##is.vector(creek_topoindex_atb)
##creek_topoindex_area<-topidx(dem, 10)$area
##expss::if_na(creek_topoindex_area, label = NULL) <- 9999
##area<-as.vector(creek_topoindex_area)

##is.na(atb)
##area<-as.vector(creek_topoindex_area)
##is.na(area)
##topidx_new<-cbind(atb, area)
##image(creek_topoindex)
##write.csv(creek_topoindex_area, "C:\\summer_institute_2019\\Scaling_theme\\R_Files\\TOPMODEL\\creek_topoindex_area.csv")
##write.csv(creek_topoindex_atb, "C:\\summer_institute_2019\\Scaling_theme\\R_Files\\TOPMODEL\\creek_topoindex_atb.csv")

#experimental_topidx<- topmodel::topidx(dem, resolution=10, river=NULL)$atb
#experimental_topidx
#image(experimental_topidx)
#topidx_new$atb<- as.double(topidx_new$atb)
#class(topidx_new$atb)
#topidx$area<- topidx_new$area

####################################################################
hubbard_raster<-raster("hubbard_clipped.tif")
plot(hubbard_raster)

#GIS_topoidx<-raster("topidx_hubbard_madeinGIS.tif")
#plot(GIS_topoidx)

#dem_sink<- sinkfill(dem, 10, degree=0.1)

hubbard_topidx_wdem1<-topidx(dem, 10)$atb
#hubbard_topidx_wdem2<-topidx(dem_sink,10)$atb

hubbard_topidx_withdem1<-make.classes(hubbard_topidx_wdem1, 11)
#hubbard_topidx_withdem2<-make.classes(hubbard_topidx_wdem2,22)
#hubbard_topidx_withGIS<-make.classes(dem2, 12)

hubbard_topidx_withdem1
#hubbard_topidx_withdem2
#hubbard_topidx_withGIS
plot(hubbard_topidx_withdem1)
#plot(hubbard_topidx_withdem2)
#plot(hubbard_topidx_withGIS)

hubbard_data<-read.csv("2016_hubbert_precip.csv")
hubbard_rain_mm<-hubbard_data$..Precip_2016_mm
hubbard_rain_m<-hubbard_data$..Precip_2016_m
hubbard_flow_ls<-hubbard_data$Streamflow_l.s
hubbard_flow_ls
hubbard_flow_m3d<-hubbard_data$Streamflow_m3.day
hubbard_flow_m3d
simple_rain<-runif(365,0,0)

delay
k<-10
hubbard_delay_flowlength<-flowlength(dem)*10
hubbard_delay_class<-make.classes(hubbard_delay_flowlength,k)
hubbard_delay_class
hubbard_delay_classorder<- hubbard_delay_class[k:1,]
plot(hubbard_delay_classorder)
hubbard_delay_classorder[,2] <- c(0, cumsum(hubbard_delay_classorder[1:(k-1),2]))
hubbard_delay_classorder

#hubbard_sim2<-topmodel(parameters, hubbard_topidx_withGIS, hubbard_delay_classorder, hubbard_rain_mm, ETp)
hubbard_sim<-topmodel(parameters, hubbard_topidx_withdem1, delay, hubbard_rain_mm, ETp)

plot(hubbard_sim, type="l", col="blue", main="2016 Daily Flow for Hubbard Creek", xlab="Days", ylab="Flow")
lines(hubbard_sim2)
points(hubbard_flow_ls)
legend("topright", legend=c("modeled", "observed"),
       col=c("blue", "black"), lty=1, cex=0.9)

Qsim <- topmodel(parameters,topidx, delay, rain, ETp)
plot(Qsim, type="l", col="red")
points(Qobs)
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
dt <- 24

parameters <- cbind(qs0,lnTe,m,Sr0,Srmax,td,vch,vr,k0,CD,dt)

