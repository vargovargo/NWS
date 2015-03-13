library(ggplot2)
library(foreign)
library(reshape2)
library(plyr)
library(data.table)
library(Hmisc)

setwd("~/Dropbox/JPER_paper/Kait/")

#read in the raw data (mortality, not temperatures yet)
CULEraw <- read.csv("LandCover_Mortality_Demographics.csv", header=T)

attach(CULEraw)

#create variables to stratify data
# percent elderly
CULEraw$pct_elderly <- (A65_69 + A70_74 + A75_79 + A80_84 + A85plus)/(X2050POP)

# percent nonwhite
CULEraw$pct_nonwhite <- (black + asian + latino + native)/(X2050POP)

# percet in poverty
CULEraw$pct_pov <- (ILT10K + I10_20K + I20_30K + I30_45K)/(X2050POP)

# percent in extreme poverty
CULEraw$pct_extrpov <- (ILT10K + I10_20K)/(X2050POP)

# percent impervious
CULEraw$pct_imperv <- (imperv)/(areasqkm)

# percent to veg
CULEraw$pct_toveg <- (new_forest + new_grass)/(areasqkm)

# percent for albedo
CULEraw$pct_toalbedo <- (high_albedo_bldgs + high_albedo_roads)/(areasqkm)

# percent modified
CULEraw$pct_modified <- (high_albedo_bldgs + high_albedo_roads + new_forest + new_grass)/(areasqkm)

CULEraw$URC <- ifelse(CULEraw$URC ==1, "Rural", 
                          ifelse(CULEraw$URC ==2, "Suburban", "Urban"
                          ))
CULEraw$incomePOP <- ILT10K+I10_20K+I20_30K+I30_45K+I45_60K+I60_75K+I75_100K+I100_125K+I125_150K+I150_200K+I200Kplus 
CULEraw$medianIncome <- ifelse(ILT10K > CULEraw$incomePOP/2, 5000,
                          ifelse(ILT10K+I10_20K > CULEraw$incomePOP/2, 15000,
                               ifelse(ILT10K+I10_20K+I20_30K > CULEraw$incomePOP/2, 25000,
                                      ifelse(ILT10K+I10_20K+I20_30K+I30_45K > CULEraw$incomePOP/2, 37500,
                                             ifelse(ILT10K+I10_20K+I20_30K+I30_45K+I45_60K > CULEraw$incomePOP/2, 52500,
                                                    ifelse(ILT10K+I10_20K+I20_30K+I30_45K+I45_60K+I60_75K > CULEraw$incomePOP/2, 67500,
                                                           ifelse(ILT10K+I10_20K+I20_30K+I30_45K+I45_60K+I60_75K+I75_100K > CULEraw$incomePOP/2, 87500,
                                                                  ifelse(ILT10K+I10_20K+I20_30K+I30_45K+I45_60K+I60_75K+I75_100K+I100_125K > CULEraw$incomePOP/2, 112500,
                                                                         ifelse(ILT10K+I10_20K+I20_30K+I30_45K+I45_60K+I60_75K+I75_100K+I100_125K+I125_150K > CULEraw$incomePOP/2, 5000,
                                                                                ifelse(ILT10K+I10_20K+I20_30K+I30_45K+I45_60K+I60_75K+I75_100K+I100_125K+I125_150K+I150_200K > CULEraw$incomePOP/2, 175000,250000
                                                                                ))))))))))


CULEraw$agePOP <- A5yr + A5_9 + A10_14 + A15_19 + A20_24 + A25_29 + A30_34 + A35_39 + A40_44 + A45_49 + A50_54 + A55_59 + A60_64 + A65_69 + A70_74 + A75_79 + A80_84 + A85plus
CULEraw$medianAge <- ifelse(A5yr > CULEraw$agePOP/2, 2.5, 
                        ifelse(A5yr + A5_9 > CULEraw$agePOP/2, 7,      
                               ifelse(A5yr + A5_9 + A10_14 > CULEraw$agePOP/2, 12,
                                      ifelse(A5yr + A5_9 + A10_14 + A15_19  > CULEraw$agePOP/2, 17,
                                             ifelse(A5yr + A5_9 + A10_14 + A15_19 + A20_24 > CULEraw$agePOP/2, 22,
                                                    ifelse(A5yr + A5_9 + A10_14 + A15_19 + A20_24 + A25_29  > CULEraw$agePOP/2, 27,
                                                           ifelse(A5yr + A5_9 + A10_14 + A15_19 + A20_24 + A25_29 + A30_34 > CULEraw$agePOP/2, 32,
                                                                  ifelse(A5yr + A5_9 + A10_14 + A15_19 + A20_24 + A25_29 + A30_34 + A35_39  > CULEraw$agePOP/2, 37,
                                                                         ifelse(A5yr + A5_9 + A10_14 + A15_19 + A20_24 + A25_29 + A30_34 + A35_39 + A40_44 > CULEraw$agePOP/2, 42,
                                                                                ifelse(A5yr + A5_9 + A10_14 + A15_19 + A20_24 + A25_29 + A30_34 + A35_39 + A40_44 + A45_49 > CULEraw$agePOP/2, 47,
                                                                                       ifelse(A5yr + A5_9 + A10_14 + A15_19 + A20_24 + A25_29 + A30_34 + A35_39 + A40_44 + A45_49 + A50_54  > CULEraw$agePOP/2, 52,
                                                                                              ifelse(A5yr + A5_9 + A10_14 + A15_19 + A20_24 + A25_29 + A30_34 + A35_39 + A40_44 + A45_49 + A50_54 + A55_59 > CULEraw$agePOP/2, 57,
                                                                                                     ifelse(A5yr + A5_9 + A10_14 + A15_19 + A20_24 + A25_29 + A30_34 + A35_39 + A40_44 + A45_49 + A50_54 + A55_59 + A60_64 > CULEraw$agePOP/2, 62,
                                                                                                            ifelse(A5yr + A5_9 + A10_14 + A15_19 + A20_24 + A25_29 + A30_34 + A35_39 + A40_44 + A45_49 + A50_54 + A55_59 + A60_64 + A65_69 > CULEraw$agePOP/2, 67,
                                                                                                                   ifelse(A5yr + A5_9 + A10_14 + A15_19 + A20_24 + A25_29 + A30_34 + A35_39 + A40_44 + A45_49 + A50_54 + A55_59 + A60_64 + A65_69 + A70_74 > CULEraw$agePOP/2, 72,
                                                                                                                          ifelse(A5yr + A5_9 + A10_14 + A15_19 + A20_24 + A25_29 + A30_34 + A35_39 + A40_44 + A45_49 + A50_54 + A55_59 + A60_64 + A65_69 + A70_74 + A75_79 > CULEraw$agePOP/2, 77,
                                                                                                                                 ifelse(A5yr + A5_9 + A10_14 + A15_19 + A20_24 + A25_29 + A30_34 + A35_39 + A40_44 + A45_49 + A50_54 + A55_59 + A60_64 + A65_69 + A70_74 + A75_79 + A80_84 > CULEraw$agePOP/2, 82, 87
                                                                                                                                 )))))))))))))))))
                                                                                                                                        
                                                           

CULEraw$mortRate <- (Medina.Ramon + HWmort)/(HWpop/100000)
CULEraw$HWmortRate <- HWmort/(HWpop/100000)
CULEraw$SUMmortRate <- Medina.Ramon/(HWpop/100000)


detach(CULEraw)

#limit the dataset to only variables and fields of interest, easier to manage
CULElimited <- CULEraw[,c("JV_ID","scenario","MSA.x","URC","medianAge","medianIncome","mortRate", "HWmortRate","SUMmortRate", "pct_elderly","pct_nonwhite","pct_pov","pct_extrpov","pct_imperv","pct_toveg","pct_toalbedo","pct_modified", "high_albedo_roads","high_albedo_bldgs","new_forest","new_grass","Medina.Ramon","HWmort")]
names(CULElimited) <- c("JV_ID","Scenario","MSA","URC","medianAge","medianIncome","mortRate", "HWmortRate","SUMmortRate", "pct_elderly","pct_nonwhite","pct_pov","pct_extrpov","pct_imperv","pct_toveg","pct_toalbedo", "pct_modified","high_albedo_roads","high_albedo_bldgs","new_forest","new_grass","Medina.Ramon","HWmort")

#subset to just 3 scenarios
CULEworking <- subset(CULElimited, Scenario == "GREEN" | Scenario == "ALBEDO" | Scenario == "ALL")
#remove tracts with no population
CULEworkingClean <- na.omit(CULEworking)
#remove outlier rates - 4 stdev
llimit <- mean(CULEworkingClean$mortRate) - 5*sd(CULEworkingClean$mortRate)
ulimit <- mean(CULEworkingClean$mortRate) + 5*sd(CULEworkingClean$mortRate)
CULEworkingClean$outlier <- ifelse(CULEworkingClean$mortRate < ulimit & CULEworkingClean$mortRate > llimit, "OK","outlier")
CULEworkingClean <- subset(CULEworkingClean, outlier == "OK")

#write.csv(CULEworkingClean, file = "DataForTableau.csv", row.names=F)

######################
#Figure 1 
#Descriptives
######################
descMelt <- subset(CULEworkingClean, Scenario=="ALL")[,c("JV_ID","MSA","URC", "pct_elderly", "pct_nonwhite","pct_pov","pct_extrpov")]

descMelt <- melt(descMelt, id=c("JV_ID", "MSA","URC"))

descriptives <- ddply(descMelt, .(MSA, URC, variable), summarise, 
                      number = length(value),
                      min = min(value),
                      max = max(value),        
                      mean = mean(value), 
                      stdev = sd(value), 
                      var = var(value),
                      lowerCI = mean(value) - 1.96*sqrt(var(value)/length(value)), 
                      upperCI = mean(value) + 1.96*sqrt(var(value)/length(value))             
)

thresholds <- ddply(descMelt, .(MSA, variable), summarise, 
                    median = median(value)
                    
)

descriptives <- subset(merge(descriptives, thresholds), variable != "pct_extrpov")

limits <- aes(ymax=mean+1.96*sqrt(var/number), ymin=mean-1.96*sqrt(var/number))
ggplot(descriptives, aes(y=mean, x=factor(URC))) +geom_point(stat="identity")+ geom_errorbar(limits, position="dodge") + facet_grid(variable~MSA, scales="free") + geom_hline(aes(yintercept=median), colour="#990000", linetype="dashed")



##########################
#catagorize tracts
##########################

thresholds <- dcast(thresholds, MSA~variable)
names(thresholds) <- c("MSA","Telderly","Tnonwhite","Tpoverty","Textpov")


CULEworkingClean <- merge(CULEworkingClean, thresholds)

CULEworkingClean$poorTract <- ifelse(CULEworkingClean$pct_pov > CULEworkingClean$Tpoverty, "Poor","notPoor")
CULEworkingClean$oldTract <- ifelse(CULEworkingClean$pct_elderly > CULEworkingClean$Telderly, "Old","notOld")
CULEworkingClean$whiteTract <- ifelse(CULEworkingClean$pct_nonwhite > CULEworkingClean$Tnonwhite, "NonWhite","White")
CULEworkingClean$threeFactor <- paste(CULEworkingClean$whiteTract, CULEworkingClean$oldTract, CULEworkingClean$poorTract, sep="_")



######################
#Figure 2 
#Total Mortality by MSA and Scenario
######################
mortMelt <- CULEworkingClean[,c("JV_ID","Scenario","MSA","URC", "mortRate", "HWmortRate","SUMmortRate", "poorTract","oldTract","whiteTract","threeFactor","Medina.Ramon","HWmort")]

mortMelt <- melt(mortMelt, id=c("JV_ID", "Scenario","MSA","URC","poorTract","oldTract","whiteTract","threeFactor"))

#write.csv(mortMelt, file = "DataForTableau_mortality.csv", row.names=F)


mortBARS <- ddply(subset(mortMelt, variable=="Medina.Ramon" | variable =="HWmort"), .(MSA, threeFactor, Scenario, variable), summarise, 
                 Mortality = sum(value)           
) 

ggplot(mortBARS, aes(y=Mortality, x=factor(threeFactor), fill=factor(variable))) +geom_bar(stat="identity", position="stack")+ facet_grid(Scenario~MSA)  + coord_flip()

######################
#Figure 3 
#Average Mortality Rate by MSA and Scenario
######################


rateBARS <- ddply(subset(mortMelt, variable=="mortRate"), .(MSA, threeFactor, Scenario, variable), summarise, 
                  Mortality = mean(value)           
) 

ggplot(rateBARS, aes(y=Mortality, x=factor(threeFactor), fill=factor(variable))) +geom_bar(stat="identity", position="stack")+ facet_grid(Scenario~MSA)  + coord_flip()


#######################################
#dot plots with mortality, race, age##
#examining for linear trends
#######################################


dotsMelt <- subset(CULEworkingClean, Scenario=="ALL")[,c("JV_ID","MSA","URC", "pct_pov","pct_elderly","pct_nonwhite","mortRate")]

dotsMelt <- melt(dotsMelt, id=c("JV_ID","MSA","URC"))

dotsMeltMort <- subset(dotsMelt, variable == "mortRate")[,c("JV_ID","value")]
names(dotsMeltMort) <- c("JV_ID","mortRate")
dotsMeltFinal <- merge(subset(dotsMelt, variable != "mortRate"), dotsMeltMort)

ggplot(dotsMeltFinal, aes(x=value, y=mortRate,  color=factor(URC))) + geom_point(alpha=0.4) + stat_smooth(method = "lm") + facet_grid(variable~MSA)

##########################
#other plots
##########################

ggplot(CULEworkingClean, aes(x=pct_modified, y=mortRate,  color=factor(poorTract))) + geom_point(alpha=0.4) + stat_smooth(method = "lm") + facet_grid(Scenario~MSA)

ggplot(subset(CULEworkingClean, Scenario!= "ALBEDO"), aes(x=pct_toveg, y=mortRate, color=factor(URC))) + geom_point(alpha=0.4) + stat_smooth(method = "lm") + facet_grid(MSA~Scenario)
ggplot(subset(CULEworkingClean, Scenario!= "GREEN"), aes(x=pct_toalbedo, y=mortRate,  color=factor(URC))) + geom_point(alpha=0.4) + stat_smooth(method = "lm") + facet_grid(MSA~Scenario)






