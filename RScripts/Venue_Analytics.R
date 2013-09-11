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
mid_day_start <- as.POSIXct('23/04/2013 13:40:00', format = "%d/%m/%Y %H:%M:%S")
mid_day_end <- as.POSIXct('23/04/2013 19:50:00', format = "%d/%m/%Y %H:%M:%S")
early <- as.POSIXct('23/04/2013 12:10:00', format = "%d/%m/%Y %H:%M:%S")
late <- as.POSIXct('23/04/2013 23:50:00', format = "%d/%m/%Y %H:%M:%S")
trades$timegrp[trades$time >= late ] = 'trade_startend'
trades$timegrp[mid_day_end <= trades$time & trades$time < late] = 'afternoon'
trades$timegrp[mid_day_start <= trades$time & trades$time < mid_day_end] = 'midday'
trades$timegrp[early <= trades$time & trades$time < mid_day_start] = 'morning'
trades$timegrp[trades$time < early ] = 'trade_startend'

#Make a plot of exchange vs timegrp vs (size of trades)
dev.new()
ggplot() +
         geom_point(aes(x = timegrp,y = exchange,size = size),data=trades) +
         scale_area(guide = guide_legend())

########################################################
#Further Aggregate by average price range
trade_startend <- subset(trades, timegrp == 'trade_startend')
afternoon <- subset(trades, timegrp == 'afternoon')
midday <- subset(trades, timegrp == 'midday')
morning <- subset(trades, timegrp == 'morning')

#Plot
dev.new()
ggplot() +
        geom_point(aes(x = price,y = exchange),data=morning)

########################################################
#Aggregate by latency

########################################################
#Aggregate by median spread



