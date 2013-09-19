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
  nobs = length(exchange)
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
mosaic(table2,shade=T,legend=T)

# Block trades vs Non-block trades
table3_1 = table1[1,]+table1[3,]+table1[4,]
table3 = rbind(table3_1,table1[2,])[,c("Early","Midday","Late")]
rownames(table3) = c("Non block trades","Block trades")
table3_freq = t(t(table3)/rowSums(t(table3)))
Cramers_v(table3)  # 0.04832802 - weakrelationship
mosaic(table3,shade=T,legend=T)

table4_1 = table2[1,]+table2[3,]+table2[4,]
table4 = rbind(table4_1,table1[4,])[,c("Early","Midday","Late")]
rownames(table4) = c("Non block trades","Block trades")
table4_freq = t(t(table4)/rowSums(t(table4)))
Cramers_v(table4)  # 0.02908549 - weakrelationship
mosaic(table4,shade=T,legend=T)


########## 2. Multinomial Logistic Regression (exchange vs latency) ##########
library(nnet)
mlr1 = multinom(exchange ~ timegrp,data=trade)
summary(mlr1)
z1 = summary(mlr1)$coefficients/summary(mlr1)$standard.errors   # standardized coefficients
p1 = (1-pnorm(abs(z1),0,1))/2  # 2-tailed z test


########## 3. Clustering ##########
# k-means clustering (based on size times)
n = dim(trades)[1]
times=matrix(nrow=n)
for (i in 1:n){
  if (timegrp[i] == "Early"){
    times[i] = 1
  } else{
    if (timegrp[i] == "Midday"){
      times[i] = 2
    } else {times[i] =3}
  }
}
clus = cbind(trades,times)[,-c(1,2,4,5)]

K_Means_CH = function(data,k){
  km = kmeans(data,centers=k,iter.max=1000,alg="Lloyd")
  CH = (km$betweenss/k-1)/((km$totss-km$betweens)/(n-k))
  return(CH)
}

k=5
km = kmeans(clus,centers=k,iter.max=1000,alg="Lloyd")
CH = (km$betweenss/k-1)/((km$totss-km$betweens)/(n-k))
CH
cols = c("red","green","blue","black","yellow")
plot(clus,col=cols[km$cluster])
points(km$centers,pch=9,cex=2,col=cols)

# Centering and Scaling
clus = as.data.frame(scale(clus))

library(lattice)
comp = cbind(trades$exchange,clus)
colnames(comp)[1] = ("exchange")
attach(comp)
xyplot(size~latency,group=exchange,cex=1.5)
xyplot(size~times,group=exchange,cex=1.5)
xyplot(latency~times,group=exchange,cex=1.5)
detach(comp)

# Scaling and Transformation
par(mfrow=c(2,2))
hist(clus$size, main="Distribution of size")
hist(clus$latency, main="Distribution of latency")
hist(log(clus$size), main="Distribution of log(size)")
hist(log(clus$latency), main="Distribution of log(latency)")

# eliminate 0 in the data set
for (i in 1:dim(clus)[1]){
	for (j in 1:dim(clus)[2]){
		if (clus[i,j] == 0){
			clus[i,j] = 0.000001
		}
	}
}

clus$size = log(clus$size)
clus$latency = log(clus$latency)

