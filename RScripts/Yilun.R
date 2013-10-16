library(vcd)

########## Raw Data ##########
early_raw = read.csv("early.csv",head = T)
midday_raw = read.csv("midday.csv",head = T)
late_raw = read.csv("late.csv",head = T)
tickers = read.csv("Ticker-Exchange-Lookup.csv",head=T)

raw_data = function(data){											
	table = xtabs(data[,5]~data[,3]+data[,2])
	table_freq = table/rowSums(table)
	total_volume = xtabs(data[,5]~data[,3])
	TWAP = xtabs(data[,4]~data[,3])/xtabs(~data[,3])
	n = dim(table)[1]
	tick = as.character(tickers[,1])
	primary_exchange = vector()
	for (i in 1:n){
		primary_exchange[i] = as.character(tickers[which(tick == rownames(table)[i]),2])
	}
	data_new1 = as.data.frame(cbind(table_freq,total_volume,TWAP))
	data_new = data.frame(data_new1,primary_exchange)
	return(data_new)
}

early = raw_data(early_raw)
midday = raw_data(midday_raw)
late = raw_data(late_raw)

########## Contingency Table ##########
Contingency = function(data){
	D1 = sum(data[,4]*data[,16])
	D2 = sum(data[,5]*data[,16])
	n = dim(data)[1]
	Primary = vector()
	for (i in 1:n){
			Primary[i] = data[i,which(colnames(data) == data[i,18])]*data[i,16]
	}
	Primary_Exchange = sum(Primary)
	Other = vector()
	for (i in 1:n){
		Other[i] = (1-data[i,which(colnames(data) == data[i,18])]-data[i,4]-data[i,5])*data[i,16]
	}
	Other_Exchange = sum(Other)
	r = rbind(D1,D2,Primary_Exchange,Other_Exchange)
	return(r)
}

table = cbind(Contingency(early),Contingency(midday),Contingency(late))
colnames(table) = c("early","midday","late")

table_freq = round(t(t(table)/rowSums(t(table))),2)

Cramers_v = function(table){
  chisq = chisq.test(table)$statistic[[1]]
  nobs = sum(table)
  ncols = ncol(table)
  nrows = nrow(table)
  v = sqrt(chisq/(nobs*(min(ncols,nrows)-1))) 
  return (v)
}

Cramers_v(table)

########## Clustering Data ##########
# D,Primary,Other
Clustering_Data1 = function(data){
	n = dim(data)[1]
	new1 = matrix(nrow = n, ncol = 3)
	new1[,1] = data[,which(colnames(data) == "D1")] + data[,which(colnames(data) == "D2")]
	for (i in 1:n){
		new1[i,2] = data[i,which(colnames(data) == data[i,18])]
		if (1 - new1[i,1] - new1[i,2] < 0){
			new1[i,3] = 0
		} else {
					new1[i,3] = 1 - new1[i,1] - new1[i,2]
		}
	}
	colnames(new1) = c("D","PE","Other")
	rownames(new1) = rownames(data)
	new2 = as.data.frame(new1)
	new = data.frame(new2,data[,c(16:18)])
	return(new)
}

# D1,D2,Other
Clustering_Data2 = function(data){
	n = dim(data)[1]
	new1 = matrix(nrow = n, ncol = 3)
	new1[,1] = data[,which(colnames(data) == "D1")]
	new1[,2] = data[,which(colnames(data) == "D2")]
	for (i in 1:n){
		if (1 - new1[i,1] - new1[i,2] < 0){
			new1[i,3] = 0
		} else {
					new1[i,3] = 1 - new1[i,1] - new1[i,2]
		}
	}
	colnames(new1) = c("D1","D2","Other")
	rownames(new1) = rownames(data)
	new2 = as.data.frame(new1)
	new = data.frame(new2,data[,c(16:18)])
	return(new)
}

# (B+J+Y),(D+Primary),Other
Clustering_Data3 = function(data){
	n = dim(data)[1]
	new1 = matrix(nrow = n, ncol = 3)
	new1[,1] = data[,which(colnames(data) == "B")] + data[,which(colnames(data) == "J")] + data[,which(colnames(data) == "Y")]
	for (i in 1:n){
		new1[i,2] = data[i,which(colnames(data) == data[i,18])] + data[i,which(colnames(data) == "D1")] + data[i,which(colnames(data) == "D2")]
		if (1 - new1[i,1] - new1[i,2] < 0){
			new1[i,3] = 0
		} else {
					new1[i,3] = 1 - new1[i,1] - new1[i,2]
		}
	}
	colnames(new1) = c("BJY","D$PE","Other")
	rownames(new1) = rownames(data)
	new2 = as.data.frame(new1)
	new = data.frame(new2,data[,c(16:18)])
	return(new)
}

early_clus1 = Clustering_Data1(early)
early_clus2 = Clustering_Data2(early)
early_clus3 = Clustering_Data3(early)
midday_clus1 = Clustering_Data1(midday)
midday_clus2 = Clustering_Data2(midday)
midday_clus3 = Clustering_Data3(midday)
late_clus1 = Clustering_Data1(late)
late_clus2 = Clustering_Data2(late)
late_clus3 = Clustering_Data3(late)

########## Ternary Plot ##########
library("scales")
Ternary_Volume = function(data,time){
	plot.new()
	n = 0.3
	ternaryplot(data[,c(1:3)],bg="lightgrey",grid_color=1,cex = log(data[,5]*data[,5])/12, main = "",col = rainbow(length(data[,	4]),start=0.2,end=0.7,alpha=n)[rank(data[,4])])
	legend('topright', c("Low Trading Volume","","","","","","High Trading Volume"), fill = c(rainbow(1,start = 0.2,alpha = n),rainbow(1,start = 0.3,alpha = n),rainbow(1,start = 0.4,alpha = n),rainbow(1,start = 0.5,alpha = n),rainbow(1,start = 0.6,alpha = n),rainbow(1,start = 0.7,alpha = n),rainbow(1,start = 0.8,alpha = n)))
	title(main = paste("Ternary Plot for",time,"session colored by level of Trading Volume",sep = " "))
}

Ternary_Volume(midday_clus1,"late")

Ternary_Price = function(data,time){
	plot.new()
	n = 0.3
	ternaryplot(data[,c(1:3)],bg="lightgrey",grid_color=1,cex = log(data[,4]*data[,4])/30, main = "",col = rainbow(length(data[,	5]),start=0.2,end=0.8,alpha=n)[rank(data[,5])])
	legend('topright', c("Low Average Price","","","","","","High Average Price"), fill = 	c(rainbow(1,start = 0.2,alpha = n),rainbow(1,start = 0.3,alpha = n),rainbow(1,start = 0.4,alpha = n),rainbow(1,start = 0.5,alpha = n),rainbow(1,start = 0.6,alpha = n),rainbow(1,start = 0.7,alpha = n),rainbow(1,start = 0.8,alpha = n)))
	title(main = paste("Ternary Plot for",time,"session colored by level of Average Price",sep = " "))
}

Ternary_Price(midday_clus1,"midday")

Ternary_Venue = function(data,time){
	plot.new()
	n = 0.3
	cols = c("firebrick1","darkorange","chartreuse2","deepskyblue","darkorchid")
	ternaryplot(data[,c(1:3)],bg="lightgrey",grid_color=1,cex = log(data[,5]*data[,5])/12, main="", col = alpha(cols[data[,6]],n))
	legend('topright', c("A","N","P","Q","Z"), fill = alpha(cols,n))
	title(main = paste("Ternary Plot for",time,"session colored by Primary Exchange",sep = " "))
}

Ternary_Venue(midday_clus1,"midday")


########## Clustering Analysis) ##########
Clustering_KM = function(data,k){
	if (k>10){
		cat("Warning: More than 10 clusters might be too much! Please choose k under 10!")
	} else {
		km = kmeans(data[,c(1:15)],centers = k,iter.max=1000,alg="Lloyd")
		centers = round(km$center,2)
		m = dim(data)[1]
		CH = CH = (km$betweenss*(m-k))/((km$totss-km$betweenss)*(k-1))
		vol = matrix(nrow = k,ncol = 3)
		price = matrix(nrow = k,ncol = 3)
		for (i in 1:k){
			vol[i,1] = max(data[which(km$cluster == i),16])
			vol[i,2] = min(data[which(km$cluster == i),16])
			vol[i,3] = mean(data[which(km$cluster == i),16])
			price[i,1] = max(data[which(km$cluster == i),17])
			price[i,2] = min(data[which(km$cluster == i),17])
			price[i,3] = mean(data[which(km$cluster == i),17])
		}	
		colnames(vol) = c("Max Volume","Min Volume","Mean Volume")
		colnames(price) = c("Max Price","Min Price","Mean Price")		
		return(list(centers,CH,vol,price))
	}
}

CH = vector(length = 9)
for (i in 1:9){
	CH[i] = Clustering_KM(early,i+1)[[2]]
}

plot(ts(CH),ylab = "CH value of k-means method", xlab = "K", xaxt = "n", main = "CH value of k-means clutering for early session")
axis(1,at = c(1:9), labels = c(2:10), cex.axis=1)

km = kmeans(late[,c(1:15)],centers = 2,iter.max=1000,alg="Lloyd")
km$center
rownames(late)[order(late[,17],decreasing = T)][which(km$cluster[order(late[,17],decreasing = T)[1:1000]] == 2)[1:5]]
par(mfrow = c(3,2))
plot(as.factor(as.character(early[which(km1$cluster==1),18])),main="Early K=2: Cluster 1",col=1,ylim=c(0,2000))
plot(as.factor(as.character(early[which(km1$cluster==2),18])),main="Early K=2: Cluster 2",col=1,ylim=c(0,2000))
plot(as.factor(as.character(early[which(km2$cluster==1),18])),main="Midday K=2: Cluster 1",col=1,ylim=c(0,2000))
plot(as.factor(as.character(early[which(km2$cluster==2),18])),main="Midday K=2: Cluster 2",col=1,ylim=c(0,2000))
plot(as.factor(as.character(late[which(km$cluster==1),18])),main="Late K=2: Cluster 1",col=1,ylim=c(0,2000))
plot(as.factor(as.character(late[which(km$cluster==2),18])),main="Late K=2: Cluster 2",col=1,ylim=c(0,2000))

########## Principal Component Analysis ##########
PCA = function(data){
	pca = prcomp(data[c(1:15)],scale=T)
	sum = summary(pca)
	d = round(pca$rotation,digit=3)
	return(list(sum,d))
}

PCA(early)

########## BJY ##########
BJY = function(data){
	num = which(data[,1] >= 0.8)
	n = length(num)
	mat = matrix(nrow = 2, ncol = 3)
	for (i in  1:2){
		mat[i,1] = max(data[num,i+3])
		mat[i,2] = min(data[num,i+3])
		mat[i,3] = mean(data[num,i+3])		
	}
	colnames(mat) = c("Max","Min","Mean")
	rownames(mat) = c("Volume","Price")		
	all = matrix(nrow = 2, ncol = 3)
	for (i in  1:2){
		all[i,1] = max(data[,i+3])
		all[i,2] = min(data[,i+3])
		all[i,3] = mean(data[,i+3])		
	}
	colnames(all) = c("Max","Min","Mean")
	rownames(all) = c("Volume","Price")	
	name = rownames(data)[num]	
	return(list(num,mat,all,name))
}
