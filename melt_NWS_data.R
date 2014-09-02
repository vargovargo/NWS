NWS <- read.csv("~/Box Documents/work/MODISsummer/paper/analysis/5county_advisories_alldays.csv", header=T) 

long_NWS <- melt(NWS, id=c("date"))

write.csv(long_NWS, "~/Box Documents/work/MODISsummer/ATL_text/Non-Precip_Weather/long_NWS.csv", row.names=FALSE, na="")

library(reshape2)
library(ggplot2)


NWS <- read.csv("~/Box Documents/work/MODISsummer/paper/analysis/long_NWS.csv", header=T) 


#-----------------------------------------------


# # stations75 <- read.csv("~/Box Documents/work/MODISsummer/paper/analysis/1Day105HI.csv", header=T) 

# working <- merge(NWS, stations75, by=c("Day", "Month", "Year", "County"), all.x=TRUE)

# workclean <- na.omit(working)

# truepos <- which(workclean$NWSadvisory == 1 & workclean$Heat_Day == 1) 
# falseneg <- which(workclean$NWSadvisory == 0 & workclean$Heat_Day == 1) 
# trueneg <- which(workclean$NWSadvisory == 0 & workclean$Heat_Day == 0) 
# falsepos <- which(workclean$NWSadvisory == 1 & workclean$Heat_Day == 0) 

# workclean$test <- ""

# workclean[truepos,11] <-"truepositive"
# workclean[trueneg,11] <-"truenegative"
# workclean[falsepos,11] <-"falsepositive"
# workclean[falseneg,11] <-"falsenegative"

# #write.csv(workclean)


#-----------------------------------------------


stations75 <- read.csv("~/Box Documents/work/MODISsummer/paper/analysis/2Day105HI.csv", header=T) 

working <- merge(NWS, stations75, by=c("Day", "Month", "Year", "County"), all.x=TRUE)
working <- working[which(working$Year > 2011 & working$Year < 2013),]


workclean <- na.omit(working)

truepos <- which(workclean$NWSadvisory == 1 & workclean$Heat_Day == 1) 
falseneg <- which(workclean$NWSadvisory == 0 & workclean$Heat_Day == 1) 
trueneg <- which(workclean$NWSadvisory == 0 & workclean$Heat_Day == 0) 
falsepos <- which(workclean$NWSadvisory == 1 & workclean$Heat_Day == 0) 

workclean$test <- ""

workclean[truepos,12] <-"truepositive"
workclean[trueneg,12] <-"truenegative"
workclean[falsepos,12] <-"falsepositive"
workclean[falseneg,12] <-"falsenegative"


#-----------------------------------------------
~/Box Documents

imperv <- read.csv("~/Box Documents/work/MODISsummer/ATL_imperv_GIS/station_imerv_results.csv", header=T) 


stationcounts <- dcast(workclean, County + Station ~test, length)
stationcounts$sensitivity <- stationcounts$truepos /(stationcounts$truepos+stationcounts$falseneg)
stationcounts$specificity <- stationcounts$trueneg /(stationcounts$trueneg+stationcounts$falsepos)
stationcounts$obs <- stationcounts$truepos + stationcounts$falsepos + stationcounts$trueneg + stationcounts$falseneg

stationcounts <- merge(stationcounts, imperv, by="Station", all.x=TRUE, all.y=FALSE)
#write.csv(stationcounts, "~/Box Documents/work/MODISsummer/paper/analysis/stationcounts.csv", row.names=FALSE, na="")

ggplot(stationcounts, aes(x=X500m, y=sensitivity, color=factor(County), size=obs)) + geom_point(stat="identity") + xlab("% impervious") + ylab("Sensitivity") + ggtitle("Station Sensitivity (Station = gold standard)") + theme(legend.position="bottom")# + geom_smooth(method = "lm", se=TRUE)# + facet_grid(.~County)


#------------------------percent developed---------------------------------------
ggplot(stationcounts, aes(x=pct_developed, y=sensitivity, color=factor(County), size=obs)) + geom_point(stat="identity") + xlab("% impervious") + ylab("Sensitivity") + ggtitle("Station Sensitivity (Station = gold standard)") + theme(legend.position="bottom")# + geom_smooth(method = "lm", se=TRUE)# + facet_grid(.~County)
quartz()
ggplot(stationcounts, aes(x=pct_developed, y=specificity, color=factor(County), size=obs)) + geom_point(stat="identity") + xlab("% impervious") + ylab("Sensitivity") + ggtitle("Station Sensitivity (Station = gold standard)") + theme(legend.position="bottom")# + geom_smooth(method = "lm", se=TRUE)# + facet_grid(.~County)


quartz()
ggplot(stationcounts, aes(x=Long, y=Lat, size=obs, color=sensitivity)) + geom_point(stat="identity") + xlab("Long") + ylab("Lat") + ggtitle("Station observations and % developed") + theme(legend.position="bottom")# + geom_smooth(method = "lm", se=TRUE)# + facet_grid(.~County)

quartz()
ggplot(stationcounts, aes(x=Long, y=Lat, size=2, color=sensitivity)) + geom_point(stat="identity") + xlab("Long") + ylab("Lat") + ggtitle("Station observations and sensitivity 2012") + theme(legend.position="bottom")# + geom_smooth(method = "lm", se=TRUE)# + facet_grid(.~County)


ggplot(stationcounts, aes(x=X500m, y=specificity, color=factor(County), size=obs)) + geom_point(stat="identity") + xlab("% impervious") + ylab("Specificity") + ggtitle("Station Specificity (Station = gold standard)") + theme(legend.position="bottom")#+ facet_grid(.~Year)


#-----------------------------------------------


countycounts <- dcast(workclean, County ~test, length)

countycounts$sensitivity <- countycounts$truepos /(countycounts$truepos+countycounts$falseneg)
countycounts$specificity <- countycounts$trueneg /(countycounts$trueneg+countycounts$falsepos)


write.csv(countycounts, "~/Box Documents/work/MODISsummer/paper/analysis/countycounts.csv", row.names=FALSE, na="")

