trades=read.csv("AAPL 20130424.csv",head=T)
loc=as.character(trades$exchange)
loc[loc=='D']='D2'
loc[loc=='B']='O'
loc[loc=='C']='O'
loc[loc=='J']='O'
loc[loc=='K']='O'
loc[loc=='P']='O'
loc[loc=='W']='O'
loc[loc=='X']='O'
loc[loc=='Y']='O'
loc[loc=='Z']='O'

trade=cbind(trades[,-c(1,2,5)],as.factor(loc))
colnames(trade)[5]="exchange"
head(trade)

########## 1. Contigency Analysis (exchange vs time) ##########
# Cramer's V ranges from 0 to 1, and it is somewhat like Pearson's r correlation and we can interpret it as
# >=0.25 (very strong relationship); 0.15-0.25 (strong relationship);
# 0.11-0.15 (moderate relationship); 0.06-0.1 (weak relationship);
# 0.01-0.05 (no relationship)
library(vcd)

Cramers_v = function(table){
  chisq = chisq.test(table)$statistic[[1]]
  nobs = sum(table)
  ncols = ncol(table)
  nrows = nrow(table)
  v = sqrt(chisq/(nobs*(min(ncols,nrows)-1))) 
  return (v)
}

# Number of trades only
table1 = xtabs( ~ trade$exchange+trade$timegrp)[,c("Early","Midday","Late")]
table1_freq = t(t(table1)/rowSums(t(table1)))
Cramers_v(table1)  # 0.04198899 - weakrelationship
mosaic(table1,shade=T,legend=T,main="Mosaic Plot for # of trades")

# trades size included
table2 = xtabs(trade$size ~ trade$exchange+trade$timegrp)[,c("Early","Midday","Late")]
table2_freq = t(t(table2)/rowSums(t(table2)))
Cramers_v(table2)  # 0.6760487 - highly strong relationship
mosaic(table2,shade=T,legend=T,main="Mosaic Plot for # of trades")

# Block trades vs Non-block trades
table3_1 = table1[1,]+table1[3,]+table1[4,]
table3 = rbind(table3_1,table1[2,])[,c("Early","Midday","Late")]
rownames(table3) = c("Non block trades","Block trades")
table3_freq = t(t(table3)/rowSums(t(table3)))
Cramers_v(table3)  # 0.04832802 - weakrelationship
mosaic(table3,shade=T,legend=T,main="Mosaic Plot for # of trades")

table4_1 = table2[1,]+table2[3,]+table2[4,]
table4 = rbind(table4_1,table2[4,])[,c("Early","Midday","Late")]
rownames(table4) = c("Non block trades","Block trades")
table4_freq = t(t(table4)/rowSums(t(table4)))
Cramers_v(table4)  # 0.02908549 - weakrelationship
mosaic(table4,shade=T,legend=T,main="Mosaic Plot for # of shares")


########## 2. Multinomial Logistic Regression (exchange vs latency) ##########
library(nnet)
mlr1 = multinom(exchange ~ timegrp,data=trade)
summary(mlr1)
z1 = summary(mlr1)$coefficients/summary(mlr1)$standard.errors   # standardized coefficients
p1 = (1-pnorm(abs(z1),0,1))/2  # 2-tailed z test


########## 3. Clustering ##########
# Processing Data #
library("scales")

data1 = read.csv2("early_clus.csv",head=T)
data1_1 = as.matrix(data1[,-1])
early = data1_1/rowSums(data1_1)

data2 = read.csv("mid_clus.csv",head=T)
data2_1 = as.matrix(data2[,-1])
mid = data2_1/rowSums(data2_1)

data3 = read.csv2("late_clus.csv",head=T)
data3_1 = as.matrix(data3[,-1])
late = data3_1/rowSums(data3_1)


# Detect 0 in Data #
Detect_Zero = function(table){
	n = dim(table)[2]
	det = vector(length=n)
	for (i in 1:n){
		det[i] = length(which(table[,i]==0))/dim(table)[1]
	}
	return(det)
}

d1 = Detect_Zero(early)
d2 = Detect_Zero(mid)
d3 = Detect_Zero(late)
colnames(early)[which(d1>mean(d1)&d2>mean(d2)&d3>mean(d3))]
# "A" "C" "M" "W" "X"

# k-means clustering - non-aggregate
cols = c(1:6)

K_Means_CH = function(data,k){
  n = dim(data)[1]
  km = kmeans(data,centers=k,iter.max=1000,alg="Lloyd")
  CH = (km$betweenss/k-1)/((km$totss-km$betweenss)/(n-k))
  return(CH)
}

#################################
n = dim(data2)[1]
k = 6
km = kmeans(mid,centers=k,iter.max=1000,alg="Lloyd")
CH = (km$betweenss/k-1)/((km$totss-km$betweenss)/(n-k))
CH
pairs(mid,col=alpha(cols[km$cluster],0.3),cex=0.4,pch=16,main="Pairs Plots for late trading and k=6") # D
km$centers
sum = rowSums(data3_1)
length(which(km$cluster == 1))
length(which(km$cluster == 2))
max(sum[which(km$cluster == 1)])
min(sum[which(km$cluster == 1)])
mean(sum[which(km$cluster == 1)])
max(sum[which(km$cluster == 2)])
min(sum[which(km$cluster == 2)])
mean(sum[which(km$cluster == 2)])
t.test(sum[which(km$cluster == 1)],sum[which(km$cluster == 2)])
data3[order(sum,decreasing = T),1][which(km$cluster[order(sum,decreasing = T)[1:1000]]==1)[1:5]]
data3[order(sum,decreasing = T),1][which(km$cluster[order(sum,decreasing = T)[1:1000]]==2)[1:5]]
##################################

# early
km2_1 = kmeans(early,centers=2,iter.max=1000,alg="Lloyd")
km3_1 = kmeans(early,centers=3,iter.max=1000,alg="Lloyd")
km4_1 = kmeans(early,centers=4,iter.max=1000,alg="Lloyd")
km5_1 = kmeans(early,centers=5,iter.max=1000,alg="Lloyd")
km6_1 = kmeans(early,centers=6,iter.max=1000,alg="Lloyd")


pairs(early,col=alpha(cols[km2_1$cluster],0.3),cex=0.4,pch=16,main="Pairs Plots for early trading and k=2") # D
pairs(early,col=alpha(cols[km3_1$cluster],0.3),cex=0.4,pch=16,main="Pairs Plots for early trading and k=3") # P
pairs(early,col=alpha(cols[km4_1$cluster],0.3),cex=0.4,pch=16,main="Pairs Plots for early trading and k=4") # N
pairs(early,col=alpha(cols[km5_1$cluster],0.3),cex=0.4,pch=16,main="Pairs Plots for early trading and k=5") # Q
pairs(early,col=alpha(cols[km6_1$cluster],0.3),cex=0.4,pch=16,main="Pairs Plots for early trading and k=6")
＃ D

km2_1$centers
length(which(km2_1$cluster == 1))
length(which(km2_1$cluster == 2))
sum = rowSums(data1_1)
max(sum[which(km2_1$cluster == 1)])
min(sum[which(km2_1$cluster == 1)])
mean(sum[which(km2_1$cluster == 1)])
max(sum[which(km2_1$cluster == 2)])
min(sum[which(km2_1$cluster == 2)])
mean(sum[which(km2_1$cluster == 2)])
t.test(sum[which(km6_2$cluster == 1)],sum[which(km2_3$cluster == 2)])
data1[order(sum,decreasing = T),1][which(km2_1$cluster[order(sum,decreasing = T)[1:1000]]==1)[1:5]]
data1[order(sum,decreasing = T),1][which(km2_1$cluster[order(sum,decreasing = T)[1:1000]]==2)[1:5]]

km$centers
length(which(km$cluster == 1))
length(which(km$cluster == 2))
length(which(km$cluster == 3))
length(which(km$cluster == 4))
length(which(km$cluster == 5))
length(which(km$cluster == 6))
max(sum[which(km$cluster == 1)])
min(sum[which(km$cluster == 1)])
mean(sum[which(km$cluster == 1)])
max(sum[which(km$cluster == 2)])
min(sum[which(km$cluster == 2)])
mean(sum[which(km$cluster == 2)])
max(sum[which(km$cluster == 3)])
min(sum[which(km$cluster == 3)])
mean(sum[which(km$cluster == 3)])
max(sum[which(km$cluster == 4)])
min(sum[which(km$cluster == 4)])
mean(sum[which(km$cluster == 4)])
max(sum[which(km$cluster == 5)])
min(sum[which(km$cluster == 5)])
mean(sum[which(km$cluster == 5)])
max(sum[which(km$cluster == 6)])
min(sum[which(km$cluster == 6)])
mean(sum[which(km$cluster == 6)])
t.test(sum[which(km$cluster == 3)],sum[which(km6_1$cluster != 3)])
t.test(sum[which(km$cluster == 6)],sum[which(km6_1$cluster != 6)])
t.test(sum[which(km$cluster == 1)],sum[which(km6_1$cluster == 2)])
t.test(sum[which(km$cluster == 4)],sum[which(km6_1$cluster == 5)])
data3[order(sum,decreasing = T),1][which(km$cluster[order(sum,decreasing = T)[1:1000]]==1)[1:5]]
data3[order(sum,decreasing = T),1][which(km$cluster[order(sum,decreasing = T)[1:1000]]==2)[1:5]]
data3[order(sum,decreasing = T),1][which(km$cluster[order(sum,decreasing = T)[1:2000]]==3)[1:5]]
data3[order(sum,decreasing = T),1][which(km$cluster[order(sum,decreasing = T)[1:1000]]==4)[1:5]]
data3[order(sum,decreasing = T),1][which(km$cluster[order(sum,decreasing = T)[1:1000]]==5)[1:5]]
data3[order(sum,decreasing = T),1][which(km$cluster[order(sum,decreasing = T)[1:1000]]==6)[1:5]]

# midday
km2_2 = kmeans(mid,centers=2,iter.max=1000,alg="Lloyd")
km3_2 = kmeans(mid,centers=3,iter.max=1000,alg="Lloyd")
km4_2 = kmeans(mid,centers=4,iter.max=1000,alg="Lloyd")
km5_2 = kmeans(mid,centers=5,iter.max=1000,alg="Lloyd")
km6_2 = kmeans(mid,centers=6,iter.max=1000,alg="Lloyd")

pairs(mid,col=alpha(cols[km2_2$cluster],0.3),cex=0.4,pch=16,main="Pairs Plots for midday trading and k=2")
pairs(mid,col=alpha(cols[km3_2$cluster],0.3),cex=0.4,pch=16,main="Pairs Plots for midday trading and k=3")
pairs(mid,col=alpha(cols[km4_2$cluster],0.3),cex=0.4,pch=16,main="Pairs Plots for midday trading and k=4")
pairs(mid,col=alpha(cols[km5_2$cluster],0.3),cex=0.4,pch=16,main="Pairs Plots for midday trading and k=5")
pairs(mid,col=alpha(cols[km6_2$cluster],0.3),cex=0.4,pch=16,main="Pairs Plots for midday trading and k=6")


# late
km2_3 = kmeans(late,centers=2,iter.max=1000,alg="Lloyd")
km3_3 = kmeans(late,centers=3,iter.max=1000,alg="Lloyd")
km4_3 = kmeans(late,centers=4,iter.max=1000,alg="Lloyd")
km5_3 = kmeans(late,centers=5,iter.max=1000,alg="Lloyd")
km6_3 = kmeans(late,centers=6,iter.max=1000,alg="Lloyd")

pairs(late,col=alpha(cols[km2_3$cluster],0.3),cex=0.4,pch=16,main="Pairs Plots for late trading and k=2")
pairs(late,col=alpha(cols[km3_3$cluster],0.3),cex=0.4,pch=16,main="Pairs Plots for late trading and k=3")
pairs(late,col=alpha(cols[km4_3$cluster],0.3),cex=0.4,pch=16,main="Pairs Plots for late trading and k=4")
pairs(late,col=alpha(cols[km5_3$cluster],0.3),cex=0.4,pch=16,main="Pairs Plots for late trading and k=5")
pairs(late,col=alpha(cols[km6_3$cluster],0.3),cex=0.4,pch=16,main="Pairs Plots for late trading and k=6")

# Analysis of trading volumes through clusters
data1_2 = rowSums(data1_1)
early_clus1 = data1_2[which(km2_1$cluster==1)]
early_clus2 = data1_2[which(km2_1$cluster==2)]
par(mfrow=c(1,2))
boxplot(early_clus1)
boxplot(early_clus2)
ticker1_2 = data1[which(data1_2 == max(early_clus2)),1] # detect outlier - BAC
early_clus2 = early_clus2[-which(early_clus2==max(early_clus2))]
data1_2 = data1_2[-which(data1_2==max(data1_2))]
mean(data1_2)
mean(early_clus1)
mean(early_clus2)

data2_2 = rowSums(data2_1)
mid_clus1 = data2_2[which(km2_2$cluster==1)]
mid_clus2 = data2_2[which(km2_2$cluster==2)]
par(mfrow=c(1,2))
boxplot(mid_clus1)
boxplot(mid_clus2)
ticker2 = data2[which(data2_2 == max(mid_clus1)),1] # - GE
ticker3 = data2[which(data2_2 == max(mid_clus2)),1] # - BAC
mid_clus1 = mid_clus1[-which(mid_clus1==max(mid_clus1))]
mid_clus2 = mid_clus2[-which(mid_clus2==max(mid_clus2))]
data2_2 = data2_2[-c(which(data2_2 == max(mid_clus1)),which(data2_2 == max(mid_clus2)))]
mean(data2_2)
mean(mid_clus1)
mean(mid_clus2)

data3_2 = rowSums(data3_1)
late_clus1 = data3_2[which(km2_3$cluster==1)]
late_clus2 = data3_2[which(km2_3$cluster==2)]
par(mfrow=c(1,2))
boxplot(late_clus1)
boxplot(late_clus2)
ticker4 = data3[which(data3_2 == max(late_clus1)),1] # - BAC
ticker5 = data3[which(data3_2 == max(late_clus2)),1] # - XLF
late_clus1 = late_clus1[-which(late_clus1==max(late_clus1))]
late_clus2 = late_clus2[-which(late_clus2==max(late_clus2))]
data3_2 = data3_2[-c(which(data3_2 == max(late_clus1)),which(data3_2 == max(late_clus2)))]
mean(data3_2)
mean(late_clus1)
mean(late_clus2)


# Analysis of Primary Exchange through Clusters
tickers = read.csv("Ticker-Exchange-Lookup.csv",head=T)

early_ticker1 = as.character(data1[which(km2_1$cluster==1),1])
early_ticker2 = as.character(data1[which(km2_1$cluster==2),1])
mid_ticker1 = as.character(data2[which(km2_2$cluster==1),1])
mid_ticker2 = as.character(data2[which(km2_2$cluster==2),1])
late_ticker1 = as.character(data3[which(km2_3$cluster==1),1])
late_ticker2 = as.character(data3[which(km2_3$cluster==2),1])

Primary_Exchange = function(data,tickers){
	n = length(data)
	tick = as.character(tickers[,1])
	C = vector()
	for (i in 1:n){
		C[i] = as.character(tickers[which(tick == data[i]),2])
	}
	return(C)
}

par(mfrow=c(3,2))
plot(as.factor(Primary_Exchange(early_ticker2,tickers)),main="Early K=2: Cluster D", ylim = c(0,1800))
plot(as.factor(Primary_Exchange(early_ticker1,tickers)),main="Early K=2: Cluster O", ylim = c(0,1800))
plot(as.factor(Primary_Exchange(mid_ticker2,tickers)),main="Mid K=2: Cluster D", ylim = c(0,1800))
plot(as.factor(Primary_Exchange(mid_ticker1,tickers)),main="Mid K=2: Cluster O", ylim = c(0,1800))
plot(as.factor(Primary_Exchange(late_ticker1,tickers)),main="Late K=2: Cluster D", ylim = c(0,1800))
plot(as.factor(Primary_Exchange(late_ticker2,tickers)),main="Late K=2: Cluster O", ylim = c(0,1800))

e_tick1 = as.character(data1[which(km6_1$cluster==1),1])
e_tick2 = as.character(data1[which(km6_1$cluster==2),1])
e_tick3 = as.character(data1[which(km6_1$cluster==3),1])
e_tick4 = as.character(data1[which(km6_1$cluster==4),1])
e_tick5 = as.character(data1[which(km6_1$cluster==5),1])
e_tick6 = as.character(data1[which(km6_1$cluster==6),1])

par(mfrow=c(3,2))
plot(as.factor(Primary_Exchange(e_tick4,tickers)),main="Early K=6: Cluster D1",ylim=c(0,1000),col=1)
plot(as.factor(Primary_Exchange(e_tick6,tickers)),main="Early K=6: Cluster D2",ylim=c(0,1000),col=1)
plot(as.factor(Primary_Exchange(e_tick1,tickers)),main="Early K=6: Cluster N",col=c(2,1),ylim=c(0,1000))
plot(as.factor(Primary_Exchange(e_tick2,tickers)),main="Early K=6: Cluster P",col=c(1,1,2,1,1),ylim=c(0,1000))
plot(as.factor(Primary_Exchange(e_tick3,tickers)),main="Early K=6: Cluster Q",col=c(1,1,1,2),ylim=c(0,1000))
plot(as.factor(Primary_Exchange(e_tick5,tickers)),main="Early K=6: Cluster O",ylim=c(0,1000),col=1)

m_tick1 = as.character(data2[which(km6_2$cluster==1),1])
m_tick2 = as.character(data2[which(km6_2$cluster==2),1])
m_tick3 = as.character(data2[which(km6_2$cluster==3),1])
m_tick4 = as.character(data2[which(km6_2$cluster==4),1])
m_tick5 = as.character(data2[which(km6_2$cluster==5),1])
m_tick6 = as.character(data2[which(km6_2$cluster==6),1])

par(mfrow=c(3,2))
plot(as.factor(Primary_Exchange(m_tick2,tickers)),main="Mid K=6: Cluster D1",col=1,ylim=c(0,1200))
plot(as.factor(Primary_Exchange(m_tick4,tickers)),main="Mid K=6: Cluster D2",col=1,ylim=c(0,1200))
plot(as.factor(Primary_Exchange(m_tick3,tickers)),main="Mid K=6: Cluster N",col=c(1,2,1,1),ylim=c(0,1200))
plot(as.factor(Primary_Exchange(m_tick5,tickers)),main="Mid K=6: Cluster P",col=c(1,1,2,1,1),ylim=c(0,1200))
plot(as.factor(Primary_Exchange(m_tick6,tickers)),main="Mid K=6: Cluster Q",col=c(1,1,1,2,1),ylim=c(0,1200))
plot(as.factor(Primary_Exchange(m_tick1,tickers)),main="Mid K=6: Cluster O",col=1, ylim=c(0,1200))

l_tick1 = as.character(data3[which(km6_3$cluster==1),1])
l_tick2 = as.character(data3[which(km6_3$cluster==2),1])
l_tick3 = as.character(data3[which(km6_3$cluster==3),1])
l_tick4 = as.character(data3[which(km6_3$cluster==4),1])
l_tick5 = as.character(data3[which(km6_3$cluster==5),1])
l_tick6 = as.character(data3[which(km6_3$cluster==6),1])

par(mfrow=c(3,2))
plot(as.factor(Primary_Exchange(l_tick4,tickers)),main="Late K=6: Cluster D1",col=1,ylim=c(0,1000))
plot(as.factor(Primary_Exchange(l_tick1,tickers)),main="Late K=6: Cluster D2",col=1,ylim=c(0,1000))
plot(as.factor(Primary_Exchange(l_tick5,tickers)),main="Late K=6: Cluster N",col=c(2,1),ylim=c(0,1000))
plot(as.factor(Primary_Exchange(l_tick2,tickers)),main="Late K=6: Cluster P",col=c(1,1,2,1),ylim=c(0,1000))
plot(as.factor(Primary_Exchange(l_tick6,tickers)),main="Late K=6: Cluster Q",col=c(1,1,1,2),ylim=c(0,1000))
plot(as.factor(Primary_Exchange(l_tick3,tickers)),main="Late K=6: Cluster K",col=1,ylim=c(0,1000))

########## 4. Binary and Three ##########

data1 = read.csv2("early_clus.csv",head=T)
data1_1 = as.matrix(data1[,-1])
early = data1_1/rowSums(data1_1)

data2 = read.csv("mid_clus.csv",head=T)
data2_1 = as.matrix(data2[,-1])
mid = data2_1/rowSums(data2_1)

data3 = read.csv2("late_clus.csv",head=T)
data3_1 = as.matrix(data3[,-1])
late = data3_1/rowSums(data3_1)

tickers = read.csv("Ticker-Exchange-Lookup.csv",head=T)

Primary_Exchange = function(data,tickers){
	n = length(data)
	tick = as.character(tickers[,1])
	C = vector()
	for (i in 1:n){
		C[i] = as.character(tickers[which(tick == data[i]),2])
	}
	return(C)
}

# 0 & 1
Binary = function(a){		# change value to 0 & 1
	threshold = 0.05		# adjust threshold here
	if (a >= threshold){
		b = 1
	} else {
		b = 0
	}
	return (b)
}

Cat1 = function(a){			# Cat1 to Cat3 is to create a new factor variable
	n = dim(a)[1]
	m = dim(a)[2]
	b = matrix(nrow=n, ncol=m)
	for (i in 1:n){
		for (j in 1:m){
			if (a[i,j] == 1){
				b[i,j] = colnames(a)[j]
			} else {b[i,j]=""}
		}
	}
	return(b)
}

Cat2 = function(matrix){
	n = dim(matrix)[1]
	name = vector(length = n)
	for (i in 1:n){
		name[i] = gsub(" ","",paste(matrix[i,1],matrix[i,2],matrix[i,3],matrix[i,4],matrix[i,5],matrix[i,6],matrix[i,7],matrix[i,8],matrix[i,9],matrix[i,10],matrix[i,11],matrix[i,12],matrix[i,13],matrix[i,14]),fixed = T)
	}
	return(name)
}

Cat3 = function(matrix){
	matrix_bi = matrix(sapply(matrix,Binary),nrow=dim(matrix)[1],byrow=F)
	colnames(matrix_bi) = colnames(matrix)
	matrix_cat1 = Cat2(Cat1(matrix_bi))
	matrix_cat2 = sort(table(matrix_cat1),T)
	return(list(matrix_cat1,matrix_cat2))
}

MEAN = function(time,pe,cat){
	mean(get(paste(time,"_volume",sep=""))[which(get(paste(time,"_new",sep=""))[,3] == pe & get(paste(time,"_new",sep=""))[,4] == cat)])
}

TP_Cluster = function(time,pe,n){
	a = sort(table(get(paste(time,"_new",sep=""))[which(get(paste(time,"_new",sep=""))[,3] == pe),4]),T)
	b = matrix(nrow = n,ncol=5)
	colnames(b) = c("Index","Exchange","# of stock","Percentage","Mean_Trading_Volume")
	b[,2] = names(a)[1:n]
	for (i in 1:n){
		b[i,1] = i
		b[i,3] = a[i]
		b[i,4] = a[i]/sum(a)
		b[i,5] = as.numeric(MEAN(time,pe,names(a)[i]))
	}
	c = sum(as.numeric(b[,4]))
	d = plot(as.numeric(b[,5]),type = "h",lwd = 10,xaxt = "n",ylab = "Average Trading Volume", xlab = "Most Popular Trading Pattern", main = as.character(paste(time, " with ",pe," as the primary exchange",sep="")), axis=F)
	axis(1,at = c(1:10), labels = names(a)[1:10], cex.axis=0.5)
	return(list(b,c))
}

early_ticker = as.character(data1[,1])
early_volume = rowSums(data1_1)
early_exchange = Primary_Exchange(as.character(data1[,1]),tickers)
early_cat = Cat3(early)[[1]]
early_new = cbind(early_ticker,early_volume,early_exchange,early_cat)
colnames(early_new) = c("tikcer","volume","primary_exchange","trading_exchange")

mid_ticker = as.character(data2[,1])
mid_volume = rowSums(data2_1)
mid_exchange = Primary_Exchange(as.character(data2[,1]),tickers)
mid_cat = Cat3(mid)[[1]]
mid_new = cbind(mid_ticker,mid_volume,mid_exchange,mid_cat)
colnames(mid_new) = c("tikcer","volume","primary_exchange","trading_exchange")

late_ticker = as.character(data3[,1])
late_volume = rowSums(data3_1)
late_exchange = Primary_Exchange(as.character(data3[,1]),tickers)
late_cat = Cat3(late)[[1]]
late_new = cbind(late_ticker,late_volume,late_exchange,late_cat)
colnames(late_new) = c("tikcer","volume","primary_exchange","trading_exchange")

TP_Cluster("early","A",10)
TP_Cluster("early","N",10)
TP_Cluster("early","P",10)
TP_Cluster("early","Q",10)
TP_Cluster("mid","A",10)
TP_Cluster("mid","N",10)
TP_Cluster("mid","P",10)
TP_Cluster("mid","Q",10)
TP_Cluster("late","A",10)
TP_Cluster("late","N",10)
TP_Cluster("late","P",10)
TP_Cluster("late","Q",10)

# 0 vs 1 vs 2
Three = function(a){
	threshold1 = 0.05		# adjust threshold here
	threshold2 = 0.5        # adjust threshold here
	if (a < threshold1){
		b = 0
	} else {
		if (a >= threshold2){
			b = 2
		} else {
			b = 1
		}
	}
	return (b)
}

Cat4 = function(a){			# Cat4 and Cat5 is to create a new factor variable
	n = dim(a)[1]
	m = dim(a)[2]
	b = matrix(nrow=n, ncol=m)
	for (i in 1:n){
		for (j in 1:m){
			if (a[i,j] == 1){
				b[i,j] = colnames(a)[j]
			} else {
				if (a[i,j] == 2){
					b[i,j] = paste(colnames(a)[j],colnames(a)[j],sep="")
				} else{
					b[i,j] = ""
				}
			}
		}
	}
	return(b)
}

Cat5 = function(matrix){
	n = dim(matrix)[1]
	name = vector(length = n)
	for (i in 1:n){
		name[i] = gsub(" ","",paste(matrix[i,1],matrix[i,2],matrix[i,3],matrix[i,4],matrix[i,5],matrix[i,6],matrix[i,7],matrix[i,8],matrix[i,9],matrix[i,10],matrix[i,11],matrix[i,12],matrix[i,13],matrix[i,14]),fixed = T)
	}
	return(name)
}

Cat6 = function(matrix){
	matrix_bi = matrix(sapply(matrix,Three),nrow=dim(matrix)[1],byrow=F)
	colnames(matrix_bi) = colnames(matrix)
	matrix_cat1 = Cat5(Cat4(matrix_bi))
	matrix_cat2 = sort(table(matrix_cat1),T)
	return(list(matrix_cat1,matrix_cat2))
}

early1_volume = rowSums(data1_1)
early1_cat = Cat6(early)[[1]]
early1_new = cbind(early_ticker,early1_volume,early_exchange,early1_cat)
colnames(early1_new) = c("tikcer","volume","primary_exchange","trading_exchange")

mid1_volume = rowSums(data2_1)
mid1_cat = Cat6(mid)[[1]]
mid1_new = cbind(mid_ticker,mid1_volume,mid_exchange,mid1_cat)
colnames(mid1_new) = c("tikcer","volume","primary_exchange","trading_exchange")

late1_volume = rowSums(data3_1)
late1_cat = Cat6(late)[[1]]
late1_new = cbind(late_ticker,late1_volume,late_exchange,late1_cat)
colnames(late1_new) = c("tikcer","volume","primary_exchange","trading_exchange")

TP_Cluster("early1","A",10)
TP_Cluster("early1","N",10)
TP_Cluster("early1","P",10)
TP_Cluster("early1","Q",10)
TP_Cluster("mid1","A",10)
TP_Cluster("mid1","N",10)
TP_Cluster("mid1","P",10)
TP_Cluster("mid1","Q",10)
TP_Cluster("late1","A",10)
TP_Cluster("late1","N",10)
TP_Cluster("late1","P",10)
TP_Cluster("late1","Q",10)


########## 5. Principal Component Analysi ##########
# early
pc.1 = prcomp(early,scale.=T)
summary(pc.1)
d1 = round(pc.1$rotation,digit=3)

# midday
pc.2 = prcomp(mid,scale.=T)
summary(pc.2)
d2 = round(pc.2$rotation,digit=3)

# late
pc.3 = prcomp(late,scale.=T)
summary(pc.3)
d3 = round(pc.3$rotation,digit=3)
write.csv2(d2,"a.csv")
write.csv2(d3,"b.csv")