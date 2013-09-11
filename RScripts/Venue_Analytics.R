library(ggplot)
library(ggplot2)
library(rJava)
library(scales)
library(iplots)
library(car)
library(MASS)
library(nnet)
library(multcomp)
library(mvtnorm)
library(survival)
library(splines)
library(effects)
library(lattice)
library(grid)
library(colorspace)

#Change it to your R directory
#Reads parsed data generated from Venue_Analytics_Parser for rapid iteration
trades <- read.table("C:/Users/JiongF/Desktop/Code/trades.csv",header=T,sep="",quote="\"")

########################################################
#Aggregate by time of the date
#Convert time stamp into the R format
trades$time <- as.POSIXct(paste('23/04/2013',substr(as.character(trades$time),1,11)), format = "%d/%m/%Y %H:%M:%S") 

#break into three group 9:40 and 15:50, NYC time
# or convert into UTC 13:40 and UTC 19:50 
trades$timegrp[trades$time >= as.POSIXct('23/04/2013 19:50:00', format = "%d/%m/%Y %H:%M:%S")] = 'pastafternoon'
trades$timegrp[trades$time < as.POSIXct('23/04/2013 19:50:00', format = "%d/%m/%Y %H:%M:%S")] = 'noon'
trades$timegrp[trades$time < as.POSIXct('23/04/2013 13:40:00', format = "%d/%m/%Y %H:%M:%S")] = 'early'
#Make a plot
dev.new()
ggplot() +
         geom_point(aes(x = timegrp,y = exchange,size = size),data=trades) +
         scale_area(guide = guide_legend())

########################################################
#Aggregate by primary listing exchange


########################################################
#Aggregate by average price range


########################################################
#Aggregate by median spread



