trades=read.csv("AAPL 20130424.csv",head=T)head(trades)
dim(trades)
names(trades)

########## 1. Contigency Analysis (exchange vs time) ##########
# Cramer's V ranges from 0 to 1, and it is somewhat like Pearson's r correlation and we can interpret it as
# >=0.25 (very strong relationship); 0.15-0.25 (strong relationship);
# 0.11-0.15 (moderate relationship); 0.06-0.1 (weak relationship);
# 0.01-0.05 (no relationship)

Cramers_v = function(table){
  chisq = chisq.test(table)$statistic[[1]]
  nobs = length(exchange)
  ncols = ncol(table)
  nrows = nrow(table)
  v = sqrt(chisq/(nobs*(min(ncols,nrows)-1))) 
  return (v)
}

# Number of trades only
attach(trades)
table1 = table(exchange,timegrp)
Cramers_v(table1)  # 0.0433153 - weakrelationship

# trades size included
table2 = xtabs(size ~ exchange+timegrp)
Cramers_v(table2)  # 0.6954657 - highly strong relationship



########## 2. Multinomial Logistic Regression (exchange vs latency) ##########
library(nnet)
mlr1 = multinom(exchange ~ latency)
summary(mlr1)
z1 = summary(mlr1)$coefficients/summary(mlr1)$standard.errors   # standardized coefficients
p1 = (1-pnorm(abs(z1),0,1))/2  # 2-tailed z test

mlr2 = multinom(exchange ~ latency + timegrp)
summary(mlr2)
z2 = summary(mlr2)$coefficients/summary(mlr2)$standard.errors   # standardized coefficients
p2 = (1-pnorm(abs(z2),0,1))/2  # 2-tailed z test


########## 3. Clustering ##########
# k-means clustering (based on size, latency and times)
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

