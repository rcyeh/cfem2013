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