################################################################
################################################################
################################################################
###################  				             	######################
###################		Ordinary Kriging		######################
###################  				             	######################
################################################################
################################################################
################################################################

rm(list=ls())
library(sp)
library(gstat)
library(geoR)
library(raster)
library(reshape)
library(rgdal)
library(foreign)

setwd("~/Box Sync/work/MODISsummer/paper/analysis/kriging/")

res <- 1000 # resoultion of the grid in meters

stationMaster <- read.dbf("./GIS/station_master_UTM.dbf", as.is=T) #stations and coordinates
stationTemps <- read.csv("./DailyMax_Keydate.csv", header=T)
s.unq <- unique(stationTemps[,c("name", "year",	"month", "day", "metro",	"max_HI")]) #remove duplicates
stationTempsMaster <- merge(s.unq, stationMaster, by.x="name",by.y="Station")


stationTempsMaster$date <- paste(stationTempsMaster$month, stationTempsMaster$day, stationTempsMaster$year, sep="/")
stationTempsMaster$date <- as.Date(stationTempsMaster$date, "%m/%d/%Y")
metros <- c("Atlanta","Chicago")

#set extents, list ATL then CHI
xmin <- c(700728, 394633)
xmax <- c(795854, 480646)
ymin <- c(3693681, 4558156)
ymax <- c(3786700, 4704834)

i<-1
d<-2

for(i in 1:2){
  msaStations <- subset(stationMaster, Metro == metros[i]) 
  msaTemps <- subset(stationTempsMaster, metro == metros[i])
  dates <- unique(msaTemps$date)
  
  grid <- expand.grid(x=seq(xmin[i],xmax[i],res),y=seq(ymin[i],ymax[i],res))
  grid2 <- grid
  colnames(grid)=c("x","y")
  coordinates(grid)=~x+y
  gridded(grid)=T
  grid = as(grid, "SpatialPixelsDataFrame") # to full grid
  
  for(d in 1:length(dates)){
    
    #load stations with temps
    dayTemps <- subset(msaTemps, date == dates[d])  
    SID <- as.data.frame(dayTemps[,c("name")])
    locations=dayTemps[,c("X","Y")]    #locations=x,y coordinates
    locs <- locations
    locations=SpatialPointsDataFrame(locations,SID)
    proj4string(locations)<-CRS("+init=epsg:32616")	
    
    geoRdata=as.data.frame(dayTemps[,c("X","Y","max_HI")])
    geodata=as.geodata(geoRdata,1:2,3)  #geodata should be in X,Y,Z format, where 'Z' is your response variables
    
    ## plot variogram
    vario=variog(geodata,estimator.type="modulus",max=100000)
    plot(vario,max=100000)
    sill=3 			#set sill/range based on variogram
    range=2000
    
    ## maximum likelihood fit for model
    ml.0=likfit(geodata,ini.cov.pars=c(sill,range),cov.model='exponential')
    ml.00=likfit(geodata,ini.cov.pars=c(sill,range), cov.model='spherical')
    
    ##information criteria (use lowest as obj.model, below)
    c(ml.0$AIC,ml.0$BIC)
    c(ml.00$AIC,ml.00$BIC)
    
    ok=krige.conv(geodata,loc=grid2,krige=krige.control(obj.model=ml.0))
    
    ## Plot results
    b=ok$predict
    ok.df=data.frame(b,grid2$x,grid2$y)
    names(ok.df)=c("OK","X","Y")
    coordinates(ok.df)=~X+Y
    gridded(ok.df)=T
    ok.spdf = as(ok.df, "SpatialPixelsDataFrame")
    ok.r=raster(ok.spdf)
    proj4string(ok.r)<-CRS("+init=epsg:32616")	
    spplot(ok.r,col.regions=rainbow(100,start=4/6,end=1),
           sp.layout=list("sp.points", locations, pch="+", cex=1.5, col="black"))
    # plot(as(ATLcnty, "SpatialLinesDataFrame"), add=T)
    
    filename <- paste(metros[i], dates[d], "OK", sep="_")
    path <- paste("./GIS/OKrasters/", filename,sep="")
    writeRaster(ok.r,path, format="GTiff", overwrite=TRUE)
    
    
    ## Plot Variance
    bv=ok$krige.var
    cv=data.frame(bv,grid2$x,grid2$y)
    names(cv)=c("OK","X","Y")
    coordinates(cv)=~X+Y
    gridded(cv)=T
    cv = as(cv, "SpatialPixelsDataFrame")
    cv.r = raster(cv)
    spplot(cv["OK"],col.regions=heat.colors,sp.layout=list("sp.points", 
                                                           locations, pch="+", cex=1.5, col="black"))
    
    filename <- paste(metros[i], dates[d], "OK_variance", sep="_")
    path <- paste("./GIS/OKrasters/", filename,sep="")
    writeRaster(cv.r,path, format="GTiff", overwrite=TRUE)

  }
  
}


##################################################################
##################################################################
##################################################################
#############						                        ##################
#############	       	Universal Kriging	       	##################
#############		             			            	##################
##################################################################
##################################################################
##################################################################

#Universal kriging uses ordinary kriging in conjunction with 
#linear regression.  It first creates a map of the variation the
#regression model can explain, then it performs ordinary kriging
#on the ressiduals

rm(list=ls())
library(lattice)
library(sp)
library(gstat)
library(geoR)
library(raster)
library(reshape)
library(rgdal)
library(maptools)
library(scales)
library(maps)
library(mapdata)
library(SDMTools)
library(ggplot2)
library(foreign)


setwd("~/Box Sync/work/MODISsummer/paper/analysis/kriging/")

res <- 1000 # resoultion of the grid in meters

stationMaster <- read.dbf("./GIS/station_master_UTM.dbf", as.is=T) #stations and coordinates
stationTemps <- read.csv("./DailyMax_Keydate.csv", header=T)
s.unq <- unique(stationTemps[,c("name", "year",  "month", "day", "metro",	"max_HI")]) #remove duplicates
stationTempsMaster <- merge(s.unq, stationMaster, by.x="name",by.y="Station")


stationTempsMaster$date <- paste(stationTempsMaster$month, stationTempsMaster$day, stationTempsMaster$year, sep="/")
stationTempsMaster$date <- as.Date(stationTempsMaster$date, "%m/%d/%Y")
metros <- c("Atlanta","Chicago")

#set extents, list ATL then CHI
xmin <- c(700728, 394633)
xmax <- c(795854, 480646)
ymin <- c(3693681, 4558156)
ymax <- c(3786700, 4704834)


i<-2
d<-2

for(i in 1:2){
  msaStations <- subset(stationMaster, Metro == metros[i]) 
  msaTemps <- subset(stationTempsMaster, metro == metros[i])
  dates <- unique(msaTemps$date)
  
  grid <- expand.grid(x=seq(xmin[i],xmax[i],res),y=seq(ymin[i],ymax[i],res))
  grid2 <- grid
  colnames(grid)=c("x","y")
  coordinates(grid)=~x+y
  gridded(grid)=T
  grid = as(grid, "SpatialPixelsDataFrame") # to full grid
  proj4string(grid)<-CRS("+init=epsg:32616")  
  
  elev.r <- raster(ifelse(i==1,"./GIS/ATL_elev_UTM.tif","./GIS/CHI_elev_UTM.tif"))
  elev.grid <- extract(elev.r, grid2, method='simple', buffer=100, fun=mean)
  elev.grid2 <- cbind(grid2, elev.grid)
  
  imperv.r <- raster(ifelse(i==1,"./GIS/ATL_imperv_UTM2.tif","./GIS/CHI_imperv_UTM.tif"))
  imperv.grid <- extract(imperv.r, grid2, method='simple', buffer=100, fun=mean)
  imperv.grid2 <- cbind(grid2, imperv.grid)
  
  for(d in 1:length(dates)){
    
    #load stations with temps
    dayTemps <- subset(msaTemps, date == dates[d])  
    SID <- as.data.frame(dayTemps[,c("name")])
    locations = dayTemps[,c("X","Y")]      		#locations=x,y coordinates
    locs <- dayTemps[,c("name","X","Y")] 
    locations=SpatialPointsDataFrame(locations,SID)
    proj4string(locations)<-CRS("+init=epsg:32616")	
    

    ##gather impervious around points
    imperv.stations <- extract(imperv.r, locations, method='simple', buffer=100, fun=mean)
    imperv.stations <- cbind(locs, imperv.stations)
    #write.csv(imperv.grid2, "atl_grid_imperv.csv", row.names=F)
    #imperv.grid2 <- read.csv("atl_grid_imperv.csv", header=T)
    
    ##gather elevation around points
    elev.stations <- extract(elev.r, locations, method='simple', buffer=100, fun=mean)
    elev.stations <- cbind(locs, elev.stations)
    #imperv.grid2$elev.grid <- elev.grid2[,3]
    #write.csv(imperv.grid2, "ATL_Grid_w_covars.csv", row.names=F)
    
    
    data2 <- as.data.frame(dayTemps[,c("X","Y","max_HI", "name")])
    data3 <- merge(data2, imperv.stations)
    data3 <- merge(data3, elev.stations)
    #data3 <- data3[, c(1:4,7,10)] ###############
    #ggplot(imperv.grid2, aes(x=x, y=y, color=imperv.grid))+ geom_point(size=3) + scale_colour_gradient(low="blue", high="red")
    #ggplot(elev.grid2, aes(x=x, y=y, color=elev.grid))+ geom_point(size=3) + scale_colour_gradient(low="blue", high="red")
    
    #################################
    ####  Set covariates for UK  ####
    #################################
    
    #this is the point data for each station 
    #the covariate for this and the grid points (data) below must be in same order
    geoRdata=as.data.frame(data3[,c("X","Y","max_HI","imperv.stations", "elev.stations")])
    geodata=as.geodata(geoRdata,1:2,3)  #geodata should be in X,Y,Z format, where 'Z' is your response variables
    #create covariates for original point data
    IMP=geoRdata$imperv.stations
    ELEV=geoRdata$elev.stations
    
    #create covariates for grid points (should match covariates above)
    gridi=as.data.frame(imperv.grid2)
    gride=as.data.frame(elev.grid2)
    # grids$Imp400[is.na(grids$Imp400)] <- 0
    grid3=data.frame(gridi$x,gridi$y)
    names(grid3)=c("x","y")
    IMP2=gridi$imperv.grid
    ELEV2=gride$elev.grid
    
    ## plot variogram
    vario=variog(geodata,estimator.type="modulus", max=10000)  # 'max' gives it a search radius to speed it up
    #plot(vario, max=10000)
    sill=2.5
    range=5000
    
    ## maximum likelihood fit for model
    ml.1=likfit(geodata,trend=~IMP,ini.cov.pars=c(sill,range),
                cov.model="exponential")
    ml.10=likfit(geodata,trend=~IMP,ini.cov.pars=c(sill,range),
                 cov.model="spherical")
    
    ##information criteria
    c(ml.1$AIC,ml.1$BIC)
    c(ml.10$AIC,ml.10$BIC)
    
    #Universal kriging w/covariates
    kc=krige.control(trend.d=~IMP+ELEV, trend.l=~IMP2+ELEV2,obj.model=ml.1) 
    uk=krige.conv(geodata,loc=grid3,krige=kc)
    
    #Temps
    a=uk$predict
    uk.df=data.frame(a,grid3$x,grid3$y)
    names(uk.df)=c("UK","X","Y")
    #temp.check <- merge(data2, uk.df, by.x=c("X", "y"), by.y=c("X","Y"))
    coordinates(uk.df)=~X+Y
    gridded(uk.df)=T
    uk.spdf = as(uk.df, "SpatialPixelsDataFrame")
    uk.r=raster(uk.spdf)
    #spplot(uk.r,col.regions=rainbow(100,start=4/6,end=1),sp.layout=list("sp.points", locations, pch="+", cex=2, col="black"))
    
    filename <- paste(metros[i], dates[d], "UK", sep="_")
    path <- paste("./GIS/UKrasters/", filename,sep="")
    writeRaster(uk.r,path, format="GTiff", overwrite=TRUE)
    
    
    
    ##### create residuals
    UK.stations.predict <- extract(uk.r, locations, method='simple', fun=mean)
    UK.stations.predict <- cbind(locs, UK.stations.predict)
    station.compare <- merge(data2, UK.stations.predict)
    station.compare$difference <- station.compare$max_HI - station.compare$UK.stations.predict
    station.compare$date <- dates[d]
    station.compare$sill <- sill
    station.compare$range <- range
    filename <- paste(metros[i], dates[d], "UK_resid.csv", sep="_")
    path <- paste("./GIS/UKrasters/", filename,sep="")
    write.csv(station.compare, path, row.names=F)
    #ggplot(station.compare, aes(x=X, y=Y, color=difference, size=max_HI))+ geom_point() + scale_colour_gradient(low="blue", high="red")
    
    
    #Variance
    av=uk$krige.var
    cv=data.frame(av,grid3$x,grid3$y)
    names(cv)=c("UK","X","Y")
    coordinates(cv)=~X+Y
    gridded(cv)=T
    cv = as(cv, "SpatialPixelsDataFrame")
    cv.r = raster(cv)
    #spplot(cv["UK"],col.regions=heat.colors,sp.layout=list("sp.points", locations, pch="+", cex=2, col="black"))
    
    filename <- paste(metros[i], dates[d], "UK_var", sep="_")
    path <- paste("./GIS/UKrasters/", filename,sep="")
    writeRaster(cv.r,path, format="GTiff", overwrite=TRUE)
    
  }

}


########################
### cross validation ###
########################
xvalid1=xvalid(geodata,model=ml.1)
print(1-var(xvalid1$error)/var(xvalid1$data))

# RMSE:
sqrt(sum(xvalid1$error^2)/length(xvalid1$error))	#UK	

#goodness of fit statistics
library(hydroGOF)	
xval1=as.data.frame(do.call(cbind, xvalid1))
gof(xval1$data,xval1$predicted)	#UK


