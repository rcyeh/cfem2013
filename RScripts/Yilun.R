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
cols = c("red","green","blue","black","yellow","pink")
K_Means_CH = function(data,k){
  n = dim(data)[1]
  km = kmeans(data,centers=k,iter.max=1000,alg="Lloyd")
  CH = (km$betweenss/k-1)/((km$totss-km$betweenss)/(n-k))
  return(CH)
}

# early
km2_1 = kmeans(early,centers=2,iter.max=1000,alg="Lloyd")
km3_1 = kmeans(early,centers=3,iter.max=1000,alg="Lloyd")
km4_1 = kmeans(early,centers=4,iter.max=1000,alg="Lloyd")
km5_1 = kmeans(early,centers=5,iter.max=1000,alg="Lloyd")
km6_1 = kmeans(early,centers=6,iter.max=1000,alg="Lloyd")

pairs(early,col=cols[km2_1$cluster]) # D
pairs(early,col=cols[km3_1$cluster]) # D,N
pairs(early,col=cols[km4_1$cluster]) # D,N,P
pairs(early,col=cols[km5_1$cluster]) # D,N,P,Q
pairs(early,col=cols[km6_1$cluster]) # D1,N,P,Q,D2

# midday
km2_2 = kmeans(mid,centers=2,iter.max=1000,alg="Lloyd")
km3_2 = kmeans(mid,centers=3,iter.max=1000,alg="Lloyd")
km4_2 = kmeans(mid,centers=4,iter.max=1000,alg="Lloyd")
km5_2 = kmeans(mid,centers=5,iter.max=1000,alg="Lloyd")
km6_2 = kmeans(mid,centers=6,iter.max=1000,alg="Lloyd")

pairs(mid,col=cols[km2_2$cluster]) # D
pairs(mid,col=cols[km3_2$cluster]) # D,P
pairs(mid,col=cols[km4_2$cluster]) # D1,P,D2
pairs(mid,col=cols[km5_2$cluster]) # D1,P,D2,N
pairs(mid,col=cols[km6_2$cluster]) # D1,P,D2,N,Q

# late
km2_3 = kmeans(late,centers=2,iter.max=1000,alg="Lloyd")
km3_3 = kmeans(late,centers=3,iter.max=1000,alg="Lloyd")
km4_3 = kmeans(late,centers=4,iter.max=1000,alg="Lloyd")
km5_3 = kmeans(late,centers=5,iter.max=1000,alg="Lloyd")
km6_3 = kmeans(late,centers=6,iter.max=1000,alg="Lloyd")

pairs(late,col=cols[km2_3$cluster]) # D
pairs(late,col=cols[km3_3$cluster]) # D,N
pairs(late,col=cols[km4_3$cluster]) # D,N,P
pairs(late,col=cols[km5_3$cluster]) # D,N,P,Q
pairs(late,col=cols[km6_3$cluster]) # D,N,P,Q,K

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
plot(as.factor(Primary_Exchange(early_ticker2,tickers)),main="Cluster 1 of Early: More trades in D")
plot(as.factor(Primary_Exchange(early_ticker1,tickers)),main="Cluster 2 of Early: Less trades in D")
plot(as.factor(Primary_Exchange(mid_ticker1,tickers)),main="Cluster 1 of Midday: More trades in D")
plot(as.factor(Primary_Exchange(mid_ticker2,tickers)),main="Cluster 2 of Midday: Less trades in D")
plot(as.factor(Primary_Exchange(late_ticker1,tickers)),main="Cluster 1 of Late: More trades in D")
plot(as.factor(Primary_Exchange(late_ticker2,tickers)),main="Cluster 2 of Late: Less trades in D")

e_tick1 = as.character(data1[which(km6_1$cluster==1),1])
e_tick2 = as.character(data1[which(km6_1$cluster==2),1])
e_tick3 = as.character(data1[which(km6_1$cluster==3),1])
e_tick4 = as.character(data1[which(km6_1$cluster==4),1])
e_tick5 = as.character(data1[which(km6_1$cluster==5),1])
e_tick6 = as.character(data1[which(km6_1$cluster==6),1])

par(mfrow=c(3,2))
plot(as.factor(Primary_Exchange(e_tick1,tickers)),main="Cluster 1 of Early: More trades in N",col=c(2,1))
plot(as.factor(Primary_Exchange(e_tick2,tickers)),main="Cluster 2 of Early: More trades in P",col=c(1,1,2,1,1))
plot(as.factor(Primary_Exchange(e_tick3,tickers)),main="Cluster 3 of Early: More trades in D",col=1)
plot(as.factor(Primary_Exchange(e_tick4,tickers)),main="Cluster 4 of Early: More trades in Q",col=c(1,1,1,2))
plot(as.factor(Primary_Exchange(e_tick5,tickers)),main="Cluster 5 of Early: More trades in N",col=c(1,2,1,1))
plot(as.factor(Primary_Exchange(e_tick6,tickers)),main="Cluster 6 of Early: Other",col=1)

m_tick1 = as.character(data2[which(km6_2$cluster==1),1])
m_tick2 = as.character(data2[which(km6_2$cluster==2),1])
m_tick3 = as.character(data2[which(km6_2$cluster==3),1])
m_tick4 = as.character(data2[which(km6_2$cluster==4),1])
m_tick5 = as.character(data2[which(km6_2$cluster==5),1])
m_tick6 = as.character(data2[which(km6_2$cluster==6),1])

par(mfrow=c(3,2))
plot(as.factor(Primary_Exchange(m_tick1,tickers)),main="Cluster 1 of Midday: Other",col=1)
plot(as.factor(Primary_Exchange(m_tick2,tickers)),main="Cluster 2 of Midday: More trades in K",col=1)
plot(as.factor(Primary_Exchange(m_tick3,tickers)),main="Cluster 3 of Midday: More trades in P",col=c(1,1,2,1,1))
plot(as.factor(Primary_Exchange(m_tick4,tickers)),main="Cluster 4 of Midday: More trades in D",col=1)
plot(as.factor(Primary_Exchange(m_tick5,tickers)),main="Cluster 5 of Midday: More trades in Q",col=c(1,1,1,2,1))
plot(as.factor(Primary_Exchange(m_tick6,tickers)),main="Cluster 6 of Midday: More trades in N",col=c(1,2,1,1,1))

l_tick1 = as.character(data3[which(km6_3$cluster==1),1])
l_tick2 = as.character(data3[which(km6_3$cluster==2),1])
l_tick3 = as.character(data3[which(km6_3$cluster==3),1])
l_tick4 = as.character(data3[which(km6_3$cluster==4),1])
l_tick5 = as.character(data3[which(km6_3$cluster==5),1])
l_tick6 = as.character(data3[which(km6_3$cluster==6),1])

par(mfrow=c(3,2))
plot(as.factor(Primary_Exchange(l_tick1,tickers)),main="Cluster 1 of Late: Other",col=1)
plot(as.factor(Primary_Exchange(l_tick2,tickers)),main="Cluster 2 of Late: More trades in Q",col=c(1,1,1,2))
plot(as.factor(Primary_Exchange(l_tick3,tickers)),main="Cluster 3 of Late: More trades in P",col=c(1,1,2,1))
plot(as.factor(Primary_Exchange(l_tick4,tickers)),main="Cluster 4 of Late: Moderate trades in D",col=1)
plot(as.factor(Primary_Exchange(l_tick5,tickers)),main="Cluster 5 of Late: Most trades in D",col=1)
plot(as.factor(Primary_Exchange(l_tick6,tickers)),main="Cluster 6 of Late: More trades in N",col=2)

########## 4. Principal Component Analysi ##########
# early
pc.1 = prcomp(early,scale.=T)
summary(pc.1)
d1 = pc.1$rotation

# midday
pc.2 = prcomp(mid,scale.=T)
summary(pc.2)
d2 = pc.2$rotation

# late
pc.3 = prcomp(late,scale.=T)
summary(pc.3)
d3 = pc.3$rotation