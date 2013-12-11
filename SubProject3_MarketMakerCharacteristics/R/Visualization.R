library(Deducer)
Partial2 <- read.table("C:point/path/to/Market Maker D Exchange.csv",header=T,sep=",",quote="")
Partial01 <- Partial2[Partial2$Rank_Spread_Tick.1==1,]
Partial02 <- Partial2[Partial2$Rank_Spread_Tick.1==2,]
Partial03 <- Partial2[Partial2$Rank_Spread_Tick.1==3,]
Partial04 <- Partial2[Partial2$Rank_Spread_Tick.1==4,]
Partial05 <- Partial2[Partial2$Rank_Spread_Tick.1==5,]
Partial06 <- Partial2[Partial2$Rank_Spread_Tick.1==6,]
Partial07 <- Partial2[Partial2$Rank_Spread_Tick.1==7,]
Partial08 <- Partial2[Partial2$Rank_Spread_Tick.1==8,]
Partial09 <- Partial2[Partial2$Rank_Spread_Tick.1==9,]
Partial00 <- Partial2[Partial2$Rank_Spread_Tick.1==0,]

#Hist of ticks/Change data argument to plot different distribution
dev.new()
ggplot() +
        geom_bar(aes(y = ..count..,x = Dist),data=Partial09)

#Distribution of the rank space by volume bucket
dev.new()
ggplot() +
        geom_bar(aes(y = ..count..,x = Dist,fill = as.factor(Volume.Bucket)),data=Partial2)

#Distribution of the distance to 45 degree line vs volume, colored by whether the ticker switches side in the next month 
dev.new()
ggplot() +
        geom_bar(aes(y = ..count..,x = Dist,colour = as.factor(SameSide)),data=Partial2)

#Distribution of Rank Space colored by Spread/Tick Size
Partial2$Compute <- Partial2$Spread/Partial2$Tick.Size
Partial2 <- Partial2[Partial2$Compute<25,]
dev.new()
ggplot() +
        geom_point(aes(x = Exc.Vol.April,y = d4,colour = Compute,fill = Compute),data=Partial2) +
        geom_smooth(aes(x = Exc.Vol.April,y = d4),data=Partial2,fill = '#ff0000',method = 'loess') +
        scale_fill_gradientn(colours = rainbow(7)) +
        scale_color_gradientn(colours = rainbow(7))

####################################################################
#Distribution of the rank space colored by log10[price]
dev.new()
ggplot() +
        geom_point(aes(x = Exc.Vol.April,y = d4,colour = LogP,fill = LogP),data=Partial2) +
        geom_smooth(aes(x = Exc.Vol.April,y = d4),data=Partial2,method = 'loess') + 
        scale_fill_gradientn(colours = rainbow(7)) +
        scale_color_gradientn(colours = rainbow(7))

#Prepare for moving average calculation
temp.zoo<-zoo(Partial2$d4,Partial2$Exc.Vol.April)
#Calculate moving average with window 3 and make first and last value as NA (to ensure identical length of vectors)
m.av<-rollmean(temp.zoo, 255,fill = list(NA, NULL, NA))

#Add calculated 255 point moving averages to existing data frame
Partial2$mav=coredata(m.av)

#Rank Space colored by Volume bucket with moving average 
dev.new()
ggplot() +
        geom_point(aes(x = Exc.Vol.April,y = d4,colour = Volume.Bucket,fill = Volume.Bucket),data=Partial2) +
        geom_smooth(aes(x = Exc.Vol.April,y = d4),data=Partial2,colour = '#ff3300',method = 'loess') +
        geom_line(aes(c(2000,4000),c(2000,4000)),color="black") +
        geom_line(aes(c(0,6000),c(0,6000)),color="blue") +
        scale_fill_gradientn(colours = rainbow(7)) +
        scale_color_gradientn(colours = rainbow(7)) 



