library(reshape2)
library(ggplot2)
library(scales)
library(plyr)


setwd("~/Box Sync/work/MODISsummer/paper/analysis/")

allAdvisories <- read.csv("./long_NWS.csv", header=T)

allAdvisories$date <- as.Date(allAdvisories$date, "%m/%d/%Y")
#check County names 
counties <- unique(allAdvisories[,"County"])

#plot advisories over all years
ggplot(allAdvisories, aes(x=date, y=factor(NWSadvisory), color=factor(NWSadvisory)))+geom_point(stat="identity") + scale_x_date(labels = date_format("%m/%y"), breaks="1 year", minor_breaks="1 month") +  scale_color_manual(values = c("0" = "blue", "1" = "red")) + theme(legend.position="bottom")+ facet_grid(County ~ .)

NWSCountyCrossTab <- dcast(allAdvisories, County ~ date, sum, value.var="NWSadvisory")
NWSCountyCrossTab[NWSCountyCrossTab == 0] <- -1
write.csv(NWSCountyCrossTab, "./AdvisoriesForXL.csv")



allStations <- read.csv("./2Day105HI.csv", header=T)
allStationsClean <- allStations[which(allStations$County != "Douglas" & allStations$County != ""),]

#check data cleaning step
unique(allStationsClean[,"County"])



#write.csv(unique(read.csv("./stations_4_unique.csv", header=T)), "./uniqStations.csv")
#---------------Check agreement of stations and NWS-------------------------------

stationsNWS <- merge(allStationsClean, allAdvisories, by=c("Day", "Month", "Year", "County"), all.x=TRUE)
stationsNWS2011On <- stationsNWS[which(stationsNWS$Year > 2005 & stationsNWS$Year < 2013),]

stationsNWS2011OnClean <- na.omit(stationsNWS2011On)

truepos <- which(stationsNWS2011OnClean$NWSadvisory == 1 & stationsNWS2011OnClean$Heat_Day == 1) 
falseneg <- which(stationsNWS2011OnClean$NWSadvisory == 0 & stationsNWS2011OnClean$Heat_Day == 1) 
trueneg <- which(stationsNWS2011OnClean$NWSadvisory == 0 & stationsNWS2011OnClean$Heat_Day == 0) 
falsepos <- which(stationsNWS2011OnClean$NWSadvisory == 1 & stationsNWS2011OnClean$Heat_Day == 0) 

stationsNWS2011OnClean$test <- ""

stationsNWS2011OnClean[truepos,"test"] <-"truepositive"
stationsNWS2011OnClean[trueneg,"test"] <-"truenegative"
stationsNWS2011OnClean[falsepos,"test"] <-"falsepositive"
stationsNWS2011OnClean[falseneg,"test"] <-"falsenegative"


#---------------Calculate Sensitivity/Specificity--------------------------------

#imperv <- read.csv("~/Box Sync/work/MODISsummer/ATL_imperv_GIS/station_imerv_results.csv", header=T) 

stationTestCounts <- dcast(stationsNWS2011OnClean, County + Station + Metro.x + Long + Lat ~test, length)
stationTestCounts$sensitivity <- stationTestCounts$truepos /(stationTestCounts$truepos+stationTestCounts$falseneg)
stationTestCounts$specificity <- stationTestCounts$trueneg /(stationTestCounts$trueneg+stationTestCounts$falsepos)
stationTestCounts$obs <- stationTestCounts$truepos + stationTestCounts$falsepos + stationTestCounts$trueneg + stationTestCounts$falseneg

#stationTestCounts <- merge(stationTestCounts, imperv, by="Station", all.x=TRUE, all.y=FALSE)
#write.csv(stationTestCounts, "~/Box Sync/work/MODISsummer/paper/analysis/stationTestCounts.csv", row.names=FALSE, na="")

#ggplot(stationTestCounts, aes(x=X500m, y=sensitivity, color=factor(County), size=obs)) + geom_point(stat="identity") + xlab("% impervious") + ylab("Sensitivity") + ggtitle("Station Sensitivity (Station = gold standard)") + theme(legend.position="bottom")# + geom_smooth(method = "lm", se=TRUE)# + facet_grid(.~County)


# #------------------------percent developed---------------------------------------
# ggplot(stationTestCounts, aes(x=pct_developed, y=sensitivity, color=factor(County), size=obs)) + geom_point(stat="identity") + xlab("% impervious") + ylab("Sensitivity") + ggtitle("Station Sensitivity (Station = gold standard)") + theme(legend.position="bottom")# + geom_smooth(method = "lm", se=TRUE)# + facet_grid(.~County)
# quartz()
# ggplot(stationTestCounts, aes(x=pct_developed, y=specificity, color=factor(County), size=obs)) + geom_point(stat="identity") + xlab("% impervious") + ylab("Sensitivity") + ggtitle("Station Sensitivity (Station = gold standard)") + theme(legend.position="bottom")# + geom_smooth(method = "lm", se=TRUE)# + facet_grid(.~County)
# 
# 
# quartz()
# ggplot(stationTestCounts, aes(x=Long, y=Lat, size=obs, color=sensitivity)) + geom_point(stat="identity") + xlab("Long") + ylab("Lat") + ggtitle("Station observations and % developed") + theme(legend.position="bottom")# + geom_smooth(method = "lm", se=TRUE)# + facet_grid(.~County)
# 
quartz()
par(mfrow=c(2,2))
ggplot(subset(stationTestCounts, Metro.x == "Atlanta"), aes(x=Long, y=Lat, size=obs, color=sensitivity)) + geom_point(stat="identity") + xlab("Long") + ylab("Lat") + ggtitle("Atlanta station observations and sensitivity 2008-2012") + theme(legend.position="bottom")
ggplot(subset(stationTestCounts, Metro.x == "Atlanta"), aes(x=Long, y=Lat, size=obs, color=specificity)) + geom_point(stat="identity") + xlab("Long") + ylab("Lat") + ggtitle("Atlanta station observations and specificity 2008-2012") + theme(legend.position="bottom") 
ggplot(subset(stationTestCounts, Metro.x == "Chicago"), aes(x=Long, y=Lat, size=obs, color=sensitivity)) + geom_point(stat="identity") + xlab("Long") + ylab("Lat") + ggtitle("Chicago station observations and sensitivity 2008-2012") + theme(legend.position="bottom")
ggplot(subset(stationTestCounts, Metro.x == "Chicago"), aes(x=Long, y=Lat, size=obs, color=specificity)) + geom_point(stat="identity") + xlab("Long") + ylab("Lat") + ggtitle("Chicago station observations and specificity 2008-2012") + theme(legend.position="bottom") 


ggplot(stationTestCounts, aes(x=sensitivity, y=specificity, color=factor(County), size=obs)) + geom_point(stat="identity") + xlab("Sensitivity") + ylab("Specificity") + ggtitle("NWS Accuracy 2008-2012 (Station = gold standard)") + theme(legend.position="bottom")+ facet_grid(. ~ Metro.x)

write.csv(stationTestCounts, "./stationTestsForGIS.csv", row.names=FALSE, na="")

#----------------calculate County stats-------------------------------

countyCounts <- dcast(stationsNWS2011OnClean, County ~test, length)

countyCounts$sensitivity <- countyCounts$truepos /(countyCounts$truepos+countyCounts$falseneg)
countyCounts$specificity <- countyCounts$trueneg /(countyCounts$trueneg+countyCounts$falsepos)


write.csv(countyCounts, "./countycounts.csv", row.names=FALSE, na="")

#------------------create alldays graphs-------------------
allDays <- read.csv("./all_days.csv", header=T)
allDays$date <- as.Date(allDays$date, "%m/%d/%Y")


stationDays <-expand.grid(Stations=unique(stationsNWS2011OnClean$Station), Days=unique(allDays$date), stringsAsFactors=T)
Funcfinal$total[is.na(Funcfinal$total)]<-0



