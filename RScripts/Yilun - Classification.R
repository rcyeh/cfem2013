setwd("~/Desktop")
library(glmnet)
library(MASS)
library(class)
library(adabag)
early_raw = read.csv("early.csv",head = T)
midday_raw = read.csv("midday.csv",head = T)
late_raw = read.csv("late.csv",head = T)
tickers = read.csv("Ticker-Exchange-Lookup.csv",head=T)

########## Data ##########
R = function(data){
	len = dim(data)[1]
	data_new = matrix(nrow = len, ncol = 6)
	exchange = as.factor(data[,6])
	dummies = model.matrix(~exchange)
	data_new[,1] = data[,4]
	data_new[,2] = data[,5]
	data_new[,3] = dummies[,2]
	data_new[,4] = dummies[,3]
	data_new[,5] = dummies[,4]
	data_new[,6] = dummies[,5]
	rownames(data_new) = rownames(data)
	colnames(data_new) = c("Price","Volume","N","P","Q","Z")
	data1 = as.data.frame(scale(data_new,center = T,scale = T))
	data2 = data.frame(data1,data[,2])
	exchange = rep("O",len)
	exchange[which(data[,2] == "D1")] = "D1"
	exchange[which(data[,2] == "D2")] = "D2"
	exchange[which(data[,2] == "B")] = "BJY"
	exchange[which(data[,2] == "J")] = "BJY"
	exchange[which(data[,2] == "Y")] = "BJY"
	exchange[as.character(early_raw[,2]) == as.character(early_raw[,6])] = "PE"
  exchange1 = rep("O",len)
	exchange1[which(data[,2] == "D1")] = "D"
	exchange1[which(data[,2] == "D2")] = "D" 
	exchange1[as.character(early_raw[,2]) == as.character(early_raw[,6])] = "PE"
	data3 = data.frame(data1,exchange)
  data4 = data.frame(data1,exchange1)
	return(list(data2,data3,data4))
}
early = R(early_raw)[[1]]
midday = R(midday_raw)[[1]]
late = R(late_raw)[[1]]
early1 = R(early_raw)[[2]]
midday1 = R(midday_raw)[[2]]
late1 = R(late_raw)[[3]]
early2 = R(early_raw)[[3]]
midday2 = R(midday_raw)[[3]]
late2 = R(late_raw)[[3]]

########## Cross Validation ##########
data = early2
n = dim(data)[1]
K = 18
d = ceiling(n/K)
set.seed(0)
i.mix = sample(1:n)
folds = vector(mode="list",length=K)
M=matrix(0, nrow=d, ncol=K)
for(j in 1:n){
	M[j]=i.mix[j]
}
for(i in 1:K){
	folds[[i]]=M[,i]
}

missclass.lda = c(rep(0,K))
missclass.knn10 = c(rep(0,K))
missclass.knn20 = c(rep(0,K))
missclass.knn50 = c(rep(0,K))
missclass.knn100 = c(rep(0,K))
missclass.knn300 = c(rep(0,K))

for(k in 1:K){
	i.train = unlist(folds[-k])
	i.test = folds[[k]]
	x.train = data[i.train,c(1:6)]
	y.train = data[i.train,7]
	x.test = data[i.test,c(1:6)]
	y.test = data[i.test,7]
	
########## LDA ##########
	fit.lda = lda(x.train,y.train)
	a.lda = predict(fit.lda,x.test)
	class.lda = a.lda$class

########## KNN ##########
	class.knn10 = knn(x.train,x.test,y.train,k=80)
	class.knn20 = knn(x.train,x.test,y.train,k=100)
	class.knn50 = knn(x.train,x.test,y.train,k=150)
	class.knn100 = knn(x.train,x.test,y.train,k=200)
	class.knn300 = knn(x.train,x.test,y.train,k=250)

########## misclassification rate ##########
	missclass.lda[k] = mean(class.lda != y.test)
	missclass.knn10[k] = mean(class.knn10 != y.test)
	missclass.knn20[k] = mean(class.knn20 != y.test)
	missclass.knn50[k] = mean(class.knn50 != y.test)
	missclass.knn100[k] = mean(class.knn100 != y.test)
	missclass.knn300[k] = mean(class.knn300 != y.test)
}

mean(missclass.lda)
mean(missclass.knn10)
mean(missclass.knn20)
mean(missclass.knn50)
mean(missclass.knn100)
mean(missclass.knn300)