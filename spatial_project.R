
# IMPORT DATASET IN R and call libraries
library(fields)
library(geoR)
library(readxl)
library(car)
library(rgeos)
library(caTools)

data <- read_excel("dataTR4.xlsx")
# cond_T=dataTR4$Condition^3.185476
# cond_T=cond_T/max(cond_T)
#### Splitting data into train and test sets
set.seed(101) 
sample = sample.split(data$Condition, SplitRatio = .90)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)

quilt.plot(train$Lon,train$Lat,train$Condition)
qqnorm(train$Condition)
qqline(train$Condition)

plot(train$Condition,train$Lon)
plot(train$Condition,train$WidthFt)
plot(train$Condition,train$Lat)
plot(train$Condition,train$Roughness, xlab="Condition",ylab="Roughness",col="blue")
plot(train$Condition,train$Lon, xlab="Condition",ylab="Lon",col="blue")

#########################################################
#########################################################
# Data trasformation 

#install.packages("car")

#l1=powerTransform(data$Condition)
#cond_T=data$Condition^3 # Transformed condition index

#############
#############
# Partitioning of original datasets into clusters using k-means 
#############
#############

# All data and covariates for the models
#cond_T=dataTR4$Condition^3.185476
z1=train$Condition^3.19
z1<-z1/max(z1)
z2=test$Condition^3.19
z2<-z2/max(z2)


lon=train$Lon
lat=train$Lat
lon1=test$Lon
lat1=test$Lat

x=train$Roughness^ 0.38
x<-x/max(x)

y=train$WidthFt^0.87
y<-y/max(y)

x1=test$Roughness^ 0.38
x1<-x1/max(x1)

y1=test$WidthFt^0.87
y1<-y1/max(y1)

# Select number of clusters for data partitioning
wss=(nrow(as.matrix(z1))-1)*sum(apply(as.matrix(z1),2,var))
for (i in 2:15) wss[i]= sum(kmeans(z1,centers=i)$withinss)
plot(2:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

# Apply kmeans clustering
ncluss=4
set.seed(101)
fit=kmeans(z1, ncluss)
fit1=kmeans(z2, ncluss)

Sclust1=cbind(lon,lat,x,y,z1,as.matrix(fit$cluster))
Sclust2=cbind(lon1,lat1,x1,y1,z2,as.matrix(fit1$cluster))

#Identify spatial location of clusters
quilt.plot(Sclust[,1],Sclust[,2],Sclust[,6],col= tim.colors(8),breaks= seq(0,ncluss,by =0.5),
           xlab="Longitude",ylab="Latitude",main="Spatial clusters of data")

#############
#############
#Cluster-wise data

# Divide data into respective clusters
# Train data
S1=subset(Sclust1,Sclust1[,6]== 1)
S2=subset(Sclust1,Sclust1[,6]== 2)
S3=subset(Sclust1,Sclust1[,6]== 3)
S4=subset(Sclust1,Sclust1[,6]== 4)

# Test data

test_S1<-subset(Sclust2,Sclust2[,6]==1)
test_S2<-subset(Sclust2,Sclust2[,6]==2)
test_S3<-subset(Sclust2,Sclust2[,6]==3)
test_S4<-subset(Sclust2,Sclust2[,6]==4)


#############
#############
# Cluster-wise data exploration

#Plot bar plots for each cluster
boxplot(S1[,5],S2[,5],S3[,5],S4[,5],ylab="Condition index",names=c("Cluster 1","Cluster 2","Cluster 3","Cluster 4"))

# Plot spatial maps of data for each cluster
dataplot=S1 #Cluster 1
par(mfrow=c(1,3))
quilt.plot(dataplot[,1],dataplot[,2],dataplot[,3],xlab="Longitude",ylab="Latitude",main="Roughness index-Cluster 1")
quilt.plot(dataplot[,1],dataplot[,2],dataplot[,4],xlab="Longitude",ylab="Latitude",main="Width (Ft)-Cluster 1")
quilt.plot(dataplot[,1],dataplot[,2],dataplot[,5]/10000,xlab="Longitude",ylab="Latitude",main="Condition indexE-4-Cluster 1")


dataplot=S2 #Cluster 2
par(mfrow=c(1,3))
quilt.plot(dataplot[,1],dataplot[,2],dataplot[,3],xlab="Longitude",ylab="Latitude",main="Roughness index-Cluster 2")
quilt.plot(dataplot[,1],dataplot[,2],dataplot[,4],xlab="Longitude",ylab="Latitude",main="Width (Ft)-Cluster 2")
quilt.plot(dataplot[,1],dataplot[,2],dataplot[,5]/10000,xlab="Longitude",ylab="Latitude",main="Condition indexE-4-Cluster 3")

dataplot=S3 #Cluster 3
par(mfrow=c(1,3))
quilt.plot(dataplot[,1],dataplot[,2],dataplot[,3],xlab="Longitude",ylab="Latitude",main="Roughness index-Cluster 3")
quilt.plot(dataplot[,1],dataplot[,2],dataplot[,4],xlab="Longitude",ylab="Latitude",main="Width (Ft)-Cluster 3")
quilt.plot(dataplot[,1],dataplot[,2],dataplot[,5]/10000,xlab="Longitude",ylab="Latitude",main="Condition indexE-4-Cluster 3")

dataplot=S4 #Cluster 4
par(mfrow=c(1,3))
quilt.plot(dataplot[,1],dataplot[,2],dataplot[,3],xlab="Longitude",ylab="Latitude",main="Roughness index-Cluster 4")
quilt.plot(dataplot[,1],dataplot[,2],dataplot[,4],xlab="Longitude",ylab="Latitude",main="Width (Ft)-Cluster 4")
quilt.plot(dataplot[,1],dataplot[,2],dataplot[,5]/10000,xlab="Longitude",ylab="Latitude",main="Condition indexE-4-Cluster 4")


# Calculate variograms for each cluster of data
var1=variog(coords = S1[,2:3],data=S1[,1])
var2=variog(coords = S2[,2:3],data=S2[,1])
var3=variog(coords = S3[,2:3],data=S3[,1])
var4=variog(coords = S4[,2:3],data=S4[,1])

par(mfrow=c(2,2))
plot(var1,main="Cluster 1",xlab="Distance (miles)",type="l",col=2)
plot(var2,main="Cluster 2",xlab="Distance (miles)",type="l",col=2)
plot(var3,main="Cluster 3",xlab="Distance (miles)",type="l",col=2)
plot(var4,main="Cluster 4",xlab="Distance (miles)",type="l",col=2)

################################################################################################################################

######################################   MY CODE     #########################################################

################################################################################################################################




# Data

data1=S1[,5]
data2=S2[,5]
data3=S3[,5]
data4=S4[,5]


#Calculating distance matrix
D1=rdist.earth(cbind(S1[,1],S1[,2]))
D2=rdist.earth(cbind(S2[,1],S2[,2]))
D3=rdist.earth(cbind(S3[,1],S3[,2]))
D4=rdist.earth(cbind(S4[,1],S4[,2]))

d1=rdist.earth(cbind(S1[,1],S1[,2]), cbind(test_S1[,1],test_S1[,2]))
d2=rdist.earth(cbind(S2[,1],S2[,2]), cbind(test_S2[,1],test_S2[,2]))
d3=rdist.earth(cbind(S3[,1],S3[,2]), cbind(test_S3[,1],test_S3[,2]))
d4=rdist.earth(cbind(S4[,1],S4[,2]), cbind(test_S4[,1],test_S4[,2]))


M1=as.matrix(cbind(rep(1,642), S1[,1],S1[,2],S1[,3],S1[,4]))
M2=as.matrix(cbind(rep(1,372), S2[,1],S2[,2],S2[,3],S2[,4]))
M3=as.matrix(cbind(rep(1,548), S3[,1],S3[,2],S3[,3],S3[,4]))
M4=as.matrix(cbind(rep(1,242), S4[,1],S4[,2],S4[,3],S4[,4]))


m1=as.matrix(cbind(rep(1,21), test_S1[,1],test_S1[,2],test_S1[,3],test_S1[,4]))
m2=as.matrix(cbind(rep(1,52), test_S2[,1],test_S2[,2],test_S2[,3],test_S2[,4]))
m3=as.matrix(cbind(rep(1,77), test_S3[,1],test_S3[,2],test_S3[,3],test_S3[,4]))
m4=as.matrix(cbind(rep(1,48), test_S4[,1],test_S4[,2],test_S4[,3],test_S4[,4]))


Z1=matrix(S1[,5], ncol=1)
Z2=matrix(S2[,5], ncol=1)
Z3=matrix(S3[,5], ncol=1)
Z4=matrix(S4[,5], ncol=1)

# Matern function for Region 1
REML.l.matern1=function(pars){
  
  cat("raw parameters",pars,"\n")
  
  pars[1:4] <- exp(pars[1:4])
  pars[3]=5*pars[3]/(1+pars[3])
  
  nu=pars[3]
  beta=pars[2]
  alpha=pars[1]
  
  delta=(pars[4]) ## nugget
  
  cat("transformed covariance parameters",pars[1:4],"\n")
  
  cov <- alpha*(D1/beta)^nu*besselK(D1/beta,nu)/(2^(nu-1)*gamma(nu))
  diag(cov) <- alpha+delta
  
  X=cbind(rep(1, dim(D1)[1]), S1[,3],S1[,4])
  
  Z=matrix(Z1, ncol=1)
  
  temp <- chol(cov)
  a=forwardsolve(t(temp),X)
  b=t(X) %*% backsolve(temp, a)
  
  a=forwardsolve(t(temp), Z)
  b1=t(X) %*% backsolve(temp, a)
  
  r=Z - X %*% solve(b) %*% b1
  
  temp1 <- 2*sum(log(diag(temp)))
  
  tempp=chol(b)
  
  temp2=2*sum(log(diag(tempp)))
  
  a=forwardsolve(t(temp), r)
  b=t(r) %*% backsolve(temp, a)
  
  temp4 <- (temp1+temp2+b)/2
  cat("reml loglikelihood",-temp4,"\n")
  
  return(temp4)
}

# Matern function for region 2
REML.l.matern2=function(pars){
  
  cat("raw parameters",pars,"\n")
  
  pars[1:4] <- exp(pars[1:4])
  pars[3]=5*pars[3]/(1+pars[3])
  
  nu=pars[3]
  beta=pars[2]
  alpha=pars[1]
  
  delta=(pars[4]) ## nugget
  
  cat("transformed covariance parameters",pars[1:4],"\n")
  
  cov <- alpha*(D2/beta)^nu*besselK(D2/beta,nu)/(2^(nu-1)*gamma(nu))
  diag(cov) <- alpha+delta
  
  X=cbind(rep(1, dim(D2)[1]), S2[,3],S2[,4])
  
  Z=matrix(Z2, ncol=1)
  
  temp <- chol(cov)
  a=forwardsolve(t(temp),X)
  b=t(X) %*% backsolve(temp, a)
  
  a=forwardsolve(t(temp), Z)
  b1=t(X) %*% backsolve(temp, a)
  
  r=Z - X %*% solve(b) %*% b1
  
  temp1 <- 2*sum(log(diag(temp)))
  
  tempp=chol(b)
  
  temp2=2*sum(log(diag(tempp)))
  
  a=forwardsolve(t(temp), r)
  b=t(r) %*% backsolve(temp, a)
  
  temp4 <- (temp1+temp2+b)/2
  cat("reml loglikelihood",-temp4,"\n")
  
  return(temp4)
}

# Matern function for region 3
REML.l.matern3=function(pars){
  
  cat("raw parameters",pars,"\n")
  
  pars[1:4] <- exp(pars[1:4])
  pars[3]=5*pars[3]/(1+pars[3])
  
  nu=pars[3]
  beta=pars[2]
  alpha=pars[1]
  
  delta=(pars[4]) ## nugget
  
  cat("transformed covariance parameters",pars[1:4],"\n")
  
  cov <- alpha*(D3/beta)^nu*besselK(D3/beta,nu)/(2^(nu-1)*gamma(nu))
  diag(cov) <- alpha+delta
  
  X=cbind(rep(1, dim(D3)[1]), S3[,3],S3[,4])
  
  Z=matrix(Z3, ncol=1)
  
  temp <- chol(cov)
  a=forwardsolve(t(temp),X)
  b=t(X) %*% backsolve(temp, a)
  
  a=forwardsolve(t(temp), Z)
  b1=t(X) %*% backsolve(temp, a)
  
  r=Z - X %*% solve(b) %*% b1
  
  temp1 <- 2*sum(log(diag(temp)))
  
  tempp=chol(b)
  
  temp2=2*sum(log(diag(tempp)))
  
  a=forwardsolve(t(temp), r)
  b=t(r) %*% backsolve(temp, a)
  
  temp4 <- (temp1+temp2+b)/2
  cat("reml loglikelihood",-temp4,"\n")
  
  return(temp4)
}

# Matern function for region 4
REML.l.matern4=function(pars){
  
  cat("raw parameters",pars,"\n")
  
  pars[1:4] <- exp(pars[1:4])
  pars[3]=5*pars[3]/(1+pars[3])
  
  nu=pars[3]
  beta=pars[2]
  alpha=pars[1]
  
  delta=(pars[4]) ## nugget
  
  cat("transformed covariance parameters",pars[1:4],"\n")
  
  cov <- alpha*(D4/beta)^nu*besselK(D4/beta,nu)/(2^(nu-1)*gamma(nu))
  diag(cov) <- alpha+delta
  
  X=cbind(rep(1, dim(D4)[1]), S4[,3],S4[,4])
  
  Z=matrix(Z4, ncol=1)
  
  temp <- chol(cov)
  a=forwardsolve(t(temp),X)
  b=t(X) %*% backsolve(temp, a)
  
  a=forwardsolve(t(temp), Z)
  b1=t(X) %*% backsolve(temp, a)
  
  r=Z - X %*% solve(b) %*% b1
  
  temp1 <- 2*sum(log(diag(temp)))
  
  tempp=chol(b)
  
  temp2=2*sum(log(diag(tempp)))
  
  a=forwardsolve(t(temp), r)
  b=t(r) %*% backsolve(temp, a)
  
  temp4 <- (temp1+temp2+b)/2
  cat("reml loglikelihood",-temp4,"\n")
  
  return(temp4)
}

# Gaussian function for region 1

REML.l.gauss1=function(pars){
  
  cat("raw parameters",pars,"\n")
  
  pars[1:3] <- exp(pars[1:3])
  
  
  alpha=pars[1] ##
  beta=pars[2] ##
  delta=(pars[3]) ## nugget
  
  cat("transformed covariance parameters",pars[1:3],"\n")
  
  cov <- alpha*exp(-(D1/beta)^2)
  diag(cov) <- alpha+delta
  
  X=cbind(rep(1, dim(D1)[1]), S1[,3],S1[,4])
  Z=matrix(Z1, ncol=1)
  
  temp <- chol(cov)
  a=forwardsolve(t(temp),X)
  b=t(X) %*% backsolve(temp, a)
  
  a=forwardsolve(t(temp), Z)
  b1=t(X) %*% backsolve(temp, a)
  
  r=Z - X %*% solve(b) %*% b1
  
  temp1 <- 2*sum(log(diag(temp)))
  
  tempp=chol(b)
  
  temp2=2*sum(log(diag(tempp)))
  
  a=forwardsolve(t(temp), r)
  b=t(r) %*% backsolve(temp, a)
  
  temp4 <- (temp1+temp2+b)/2
  cat("reml loglikelihood",-temp4,"\n")
  
  return(temp4)
}

# Gaussian function for region 2

REML.l.gauss2=function(pars){
  
  cat("raw parameters",pars,"\n")
  
  pars[1:3] <- exp(pars[1:3])
  
  
  alpha=pars[1] ##
  beta=pars[2] ##
  delta=(pars[3]) ## nugget
  
  cat("transformed covariance parameters",pars[1:3],"\n")
  
  cov <- alpha*exp(-(D2/beta)^2)
  diag(cov) <- alpha+delta
  
  X=cbind(rep(1, dim(D2)[1]), S2[,3],S2[,4])
  Z=matrix(Z2, ncol=1)
  
  temp <- chol(cov)
  a=forwardsolve(t(temp),X)
  b=t(X) %*% backsolve(temp, a)
  
  a=forwardsolve(t(temp), Z)
  b1=t(X) %*% backsolve(temp, a)
  
  r=Z - X %*% solve(b) %*% b1
  
  temp1 <- 2*sum(log(diag(temp)))
  
  tempp=chol(b)
  
  temp2=2*sum(log(diag(tempp)))
  
  a=forwardsolve(t(temp), r)
  b=t(r) %*% backsolve(temp, a)
  
  temp4 <- (temp1+temp2+b)/2
  cat("reml loglikelihood",-temp4,"\n")
  
  return(temp4)
}
# Gaussian function for region 3

REML.l.gauss3=function(pars){
  
  cat("raw parameters",pars,"\n")
  
  pars[1:3] <- exp(pars[1:3])
  
  
  alpha=pars[1] ##
  beta=pars[2] ##
  delta=(pars[3]) ## nugget
  
  cat("transformed covariance parameters",pars[1:3],"\n")
  
  cov <- alpha*exp(-(D3/beta)^2)
  diag(cov) <- alpha+delta
  
  X=cbind(rep(1, dim(D3)[1]), S3[,3],S3[,4])
  Z=matrix(Z3, ncol=1)
  
  temp <- chol(cov)
  a=forwardsolve(t(temp),X)
  b=t(X) %*% backsolve(temp, a)
  
  a=forwardsolve(t(temp), Z)
  b1=t(X) %*% backsolve(temp, a)
  
  r=Z - X %*% solve(b) %*% b1
  
  temp1 <- 2*sum(log(diag(temp)))
  
  tempp=chol(b)
  
  temp2=2*sum(log(diag(tempp)))
  
  a=forwardsolve(t(temp), r)
  b=t(r) %*% backsolve(temp, a)
  
  temp4 <- (temp1+temp2+b)/2
  cat("reml loglikelihood",-temp4,"\n")
  
  return(temp4)
}
# Gaussian function for region 4

REML.l.gauss4=function(pars){
  
  cat("raw parameters",pars,"\n")
  
  pars[1:3] <- exp(pars[1:3])
  
  
  alpha=pars[1] ##
  beta=pars[2] ##
  delta=(pars[3]) ## nugget
  
  cat("transformed covariance parameters",pars[1:3],"\n")
  
  cov <- alpha*exp(-(D4/beta)^2)
  diag(cov) <- alpha+delta
  
  X=cbind(rep(1, dim(D4)[1]), S4[,3],S4[,4])
  Z=matrix(Z4, ncol=1)
  
  temp <- chol(cov)
  a=forwardsolve(t(temp),X)
  b=t(X) %*% backsolve(temp, a)
  
  a=forwardsolve(t(temp), Z)
  b1=t(X) %*% backsolve(temp, a)
  
  r=Z - X %*% solve(b) %*% b1
  
  temp1 <- 2*sum(log(diag(temp)))
  
  tempp=chol(b)
  
  temp2=2*sum(log(diag(tempp)))
  
  a=forwardsolve(t(temp), r)
  b=t(r) %*% backsolve(temp, a)
  
  temp4 <- (temp1+temp2+b)/2
  cat("reml loglikelihood",-temp4,"\n")
  
  return(temp4)
}

# Exponential function for region 1

#Exponential function
REML.l.exp1=function(pars){
  
  cat("raw parameters",pars,"\n")
  
  pars[1:3] <- exp(pars[1:3])
  
  
  alpha=pars[1] ##
  beta=pars[2] ##
  delta=(pars[3]) ## nugget
  
  cat("transformed covariance parameters",pars[1:3],"\n")
  
  cov <- alpha*exp(-(D1/beta))
  diag(cov) <- alpha+delta
  
  X=cbind(rep(1, dim(D1)[1]), S1[,3],S1[,4])
  Z=matrix(Z1, ncol=1)
  
  temp <- chol(cov)
  a=forwardsolve(t(temp),X)
  b=t(X) %*% backsolve(temp, a)
  
  a=forwardsolve(t(temp), Z)
  b1=t(X) %*% backsolve(temp, a)
  
  r=Z - X %*% solve(b) %*% b1
  
  temp1 <- 2*sum(log(diag(temp)))
  
  tempp=chol(b)
  
  temp2=2*sum(log(diag(tempp)))
  
  a=forwardsolve(t(temp), r)
  b=t(r) %*% backsolve(temp, a)
  
  temp4 <- (temp1+temp2+b)/2
  cat("reml loglikelihood",-temp4,"\n")
  
  return(temp4)
}

# Exponential function for region 2

#Exponential function
REML.l.exp2=function(pars){
  
  cat("raw parameters",pars,"\n")
  
  pars[1:3] <- exp(pars[1:3])
  
  
  alpha=pars[1] ##
  beta=pars[2] ##
  delta=(pars[3]) ## nugget
  
  cat("transformed covariance parameters",pars[1:3],"\n")
  
  cov <- alpha*exp(-(D2/beta))
  diag(cov) <- alpha+delta
  
  X=cbind(rep(1, dim(D2)[1]), S2[,3],S2[,4])
  Z=matrix(Z2, ncol=1)
  
  temp <- chol(cov)
  a=forwardsolve(t(temp),X)
  b=t(X) %*% backsolve(temp, a)
  
  a=forwardsolve(t(temp), Z)
  b1=t(X) %*% backsolve(temp, a)
  
  r=Z - X %*% solve(b) %*% b1
  
  temp1 <- 2*sum(log(diag(temp)))
  
  tempp=chol(b)
  
  temp2=2*sum(log(diag(tempp)))
  
  a=forwardsolve(t(temp), r)
  b=t(r) %*% backsolve(temp, a)
  
  temp4 <- (temp1+temp2+b)/2
  cat("reml loglikelihood",-temp4,"\n")
  
  return(temp4)
}
# Exponential function for region 3

#Exponential function
REML.l.exp3=function(pars){
  
  cat("raw parameters",pars,"\n")
  
  pars[1:3] <- exp(pars[1:3])
  
  
  alpha=pars[1] ##
  beta=pars[2] ##
  delta=(pars[3]) ## nugget
  
  cat("transformed covariance parameters",pars[1:3],"\n")
  
  cov <- alpha*exp(-(D3/beta))
  diag(cov) <- alpha+delta
  
  X=cbind(rep(1, dim(D3)[1]), S3[,3],S3[,4])
  Z=matrix(Z3, ncol=1)
  
  temp <- chol(cov)
  a=forwardsolve(t(temp),X)
  b=t(X) %*% backsolve(temp, a)
  
  a=forwardsolve(t(temp), Z)
  b1=t(X) %*% backsolve(temp, a)
  
  r=Z - X %*% solve(b) %*% b1
  
  temp1 <- 2*sum(log(diag(temp)))
  
  tempp=chol(b)
  
  temp2=2*sum(log(diag(tempp)))
  
  a=forwardsolve(t(temp), r)
  b=t(r) %*% backsolve(temp, a)
  
  temp4 <- (temp1+temp2+b)/2
  cat("reml loglikelihood",-temp4,"\n")
  
  return(temp4)
}
# Exponential function for region 4

#Exponential function
REML.l.exp4=function(pars){
  
  cat("raw parameters",pars,"\n")
  
  pars[1:3] <- exp(pars[1:3])
  
  
  alpha=pars[1] ##
  beta=pars[2] ##
  delta=(pars[3]) ## nugget
  
  cat("transformed covariance parameters",pars[1:3],"\n")
  
  cov <- alpha*exp(-(D4/beta))
  diag(cov) <- alpha+delta
  
  X=cbind(rep(1, dim(D4)[1]), S4[,3],S4[,4])
  Z=matrix(Z4, ncol=1)
  
  temp <- chol(cov)
  a=forwardsolve(t(temp),X)
  b=t(X) %*% backsolve(temp, a)
  
  a=forwardsolve(t(temp), Z)
  b1=t(X) %*% backsolve(temp, a)
  
  r=Z - X %*% solve(b) %*% b1
  
  temp1 <- 2*sum(log(diag(temp)))
  
  tempp=chol(b)
  
  temp2=2*sum(log(diag(tempp)))
  
  a=forwardsolve(t(temp), r)
  b=t(r) %*% backsolve(temp, a)
  
  temp4 <- (temp1+temp2+b)/2
  cat("reml loglikelihood",-temp4,"\n")
  
  return(temp4)
}


# Passing the parameters

# Matern
m_fit1=nlm(REML.l.matern1, c(1,1,1,1),stepmax=2,print.level=2)
m_fit2=nlm(REML.l.matern2, c(1,1,1,1),stepmax=2,print.level=2)
m_fit3=nlm(REML.l.matern3, c(-2,-2,-2,-2),stepmax=4,print.level=2)
m_fit4=nlm(REML.l.matern4, c(1,2,2,2),stepmax=4,print.level=2)


# Gaussian
g_fit1=nlm(REML.l.gauss1, c(0.08,0,0),stepmax=2,print.level=2)
g_fit2=nlm(REML.l.gauss2, c(0,0,0),stepmax=2,print.level=2)
g_fit3=nlm(REML.l.gauss3, c(0,0,0),stepmax=2,print.level=2)
g_fit4=nlm(REML.l.gauss4, c(0,0,0),stepmax=2,print.level=2)


# Exponential
e_fit1=nlm(REML.l.exp1, c(1,1,1),stepmax=2,print.level=2)
e_fit2=nlm(REML.l.exp2, c(-1,-1,0),stepmax=2,print.level=2)
e_fit3=nlm(REML.l.exp3, c(0,0,0),stepmax=2,print.level=2)
e_fit4=nlm(REML.l.exp4, c(0,0,0),stepmax=2,print.level=2)


# Setting the parameters fromt the results of the matern model

# Matern
alpha_m1=exp(m_fit1$estimate[1])
alpha_m2=exp(m_fit2$estimate[1])
alpha_m3=exp(m_fit3$estimate[1])
alpha_m4=exp(m_fit4$estimate[1])

beta_m1=exp(m_fit1$estimate[2])
beta_m2=exp(m_fit2$estimate[2])
beta_m3=exp(m_fit3$estimate[2])
beta_m4=exp(m_fit4$estimate[2])

nu_m1=4
nu_m2=0.6140687
nu_m3=0.6864603
nu_m4=3.268812


nugget_m1=exp(m_fit1$estimate[4])
nugget_m2=exp(m_fit2$estimate[4])
nugget_m3=exp(m_fit3$estimate[4])
nugget_m4=exp(m_fit4$estimate[4])

# Gaussian

alpha_g1=exp(g_fit1$estimate[1])
alpha_g2=exp(g_fit2$estimate[1])
alpha_g3=exp(g_fit3$estimate[1])
alpha_g4=exp(g_fit4$estimate[1])

beta_g1=exp(g_fit1$estimate[2])
beta_g2=exp(g_fit2$estimate[2])
beta_g3=exp(g_fit3$estimate[2])
beta_g4=exp(g_fit4$estimate[2])

nugget_g1=exp(g_fit1$estimate[3])
nugget_g2=exp(g_fit2$estimate[3])
nugget_g3=exp(g_fit3$estimate[3])
nugget_g4=exp(g_fit4$estimate[3])

# Exponential

alpha_e1=exp(e_fit1$estimate[1])
alpha_e2=exp(e_fit2$estimate[1])
alpha_e3=exp(e_fit3$estimate[1])
alpha_e4=exp(e_fit4$estimate[1])

beta_e1=exp(e_fit1$estimate[2])
beta_e2=exp(e_fit2$estimate[2])
beta_e3=exp(e_fit3$estimate[2])
beta_e4=exp(e_fit4$estimate[2])

nugget_e1=exp(e_fit1$estimate[3])
nugget_e2=exp(e_fit2$estimate[3])
nugget_e3=exp(e_fit3$estimate[3])
nugget_e4=exp(e_fit4$estimate[3])



# Covariance matrix

# Matern

K_m1<- alpha_m1*(D1/beta_m1)^nu_m1*besselK(D1/beta_m1,nu_m1)/(2^(nu_m1-1)*gamma(nu_m1))
diag(K_m1) <- alpha_m1+nugget_m1
K_m2<- alpha_m2*(D2/beta_m2)^nu_m2*besselK(D2/beta_m2,nu_m2)/(2^(nu_m2-1)*gamma(nu_m2))
diag(K_m2) <- alpha_m2+nugget_m2
K_m3<- alpha_m3*(D3/beta_m3)^nu_m3*besselK(D3/beta_m3,nu_m3)/(2^(nu_m3-1)*gamma(nu_m3))
diag(K_m3) <- alpha_m3+nugget_m3
K_m4<- alpha_m4*(D4/beta_m4)^nu_m4*besselK(D4/beta_m4,nu_m4)/(2^(nu_m4-1)*gamma(nu_m4))
diag(K_m4) <- alpha_m4+nugget_m4


k_m1<- alpha_m1*(d1/beta_m1)^nu_m1*besselK(d1/beta_m1,nu_m1)/(2^(nu_m1-1)*gamma(nu_m1))
diag(k_m1) <- alpha_m1+nugget_m1
k_m2<- alpha_m2*(d2/beta_m2)^nu_m2*besselK(d2/beta_m2,nu_m2)/(2^(nu_m2-1)*gamma(nu_m2))
diag(k_m2) <- alpha_m2+nugget_m2
k_m3<- alpha_m3*(d3/beta_m3)^nu_m3*besselK(d3/beta_m3,nu_m3)/(2^(nu_m3-1)*gamma(nu_m3))
diag(k_m3) <- alpha_m3+nugget_m3
k_m4<- alpha_m4*(d4/beta_m4)^nu_m4*besselK(d4/beta_m4,nu_m4)/(2^(nu_m4-1)*gamma(nu_m4))
diag(k_m4) <- alpha_m4+nugget_m4


# For gaussian

K_g1<- alpha_g1*exp(-(D1/beta_g1)^2)
diag(K_g1) <- alpha_g1+nugget_g1
K_g2<- alpha_g2*exp(-(D2/beta_g2)^2)
diag(K_g2) <- alpha_g2+nugget_g2
K_g3<- alpha_g3*exp(-(D3/beta_g3)^2)
diag(K_g3) <- alpha_g3+nugget_g3
K_g4<- alpha_g4*exp(-(D4/beta_g4)^2)
diag(K_g4) <- alpha_g4+nugget_g4


k_g1<- alpha_g1*exp(-(d1/beta_g1)^2)
diag(k_g1) <- alpha_g1+nugget_g1
k_g2<- alpha_g2*exp(-(d2/beta_g2)^2)
diag(k_g2) <- alpha_g2+nugget_g2
k_g3<- alpha_g3*exp(-(d3/beta_g3)^2)
diag(k_g3) <- alpha_g3+nugget_g3
k_g4<- alpha_g4*exp(-(d4/beta_g4)^2)
diag(k_g4) <- alpha_g4+nugget_g4


# For exponenetial

K_e1<- alpha_e1*exp(-(D1/beta_e1)^2)
diag(K_e1) <- alpha_e1+nugget_e1
K_e2<- alpha_e2*exp(-(D2/beta_e2)^2)
diag(K_e2) <- alpha_e2+nugget_e2
K_e3<- alpha_e3*exp(-(D3/beta_e3)^2)
diag(K_e3) <- alpha_e3+nugget_e3
K_e4<- alpha_e4*exp(-(D4/beta_e4)^2)
diag(K_e4) <- alpha_e4+nugget_e4


k_e1<- alpha_e1*exp(-(d1/beta_e1)^2)
diag(k_e1) <- alpha_e1+nugget_e1
k_e2<- alpha_e2*exp(-(d2/beta_e2)^2)
diag(k_e2) <- alpha_e2+nugget_e2
k_e3<- alpha_e3*exp(-(d3/beta_e3)^2)
diag(k_e3) <- alpha_e3+nugget_e3
k_e4<- alpha_e4*exp(-(d4/beta_e4)^2)
diag(k_e4) <- alpha_e4+nugget_e4





# Calculating lambda

# Matern

lambda_m1=t( ( solve(K_m1) - solve(K_m1)%*% M1 %*% (solve( t(M1) %*% solve(K_m1) %*% M1))
               %*%t(M1) %*% solve(K_m1)) %*% k_m1 + solve(K_m1) %*% M1 %*% solve( t(M1) %*% solve(K_m1) %*%
                                                                                    M1) %*% t(m1))
lambda_m2=t( ( solve(K_m2) - solve(K_m2)%*% M2 %*% (solve( t(M2) %*% solve(K_m2) %*% M2))
               %*%t(M2) %*% solve(K_m2)) %*% k_m2 + solve(K_m2) %*% M2 %*% solve( t(M2) %*% solve(K_m2) %*%
                                                                                    M2) %*% t(m2))
lambda_m3=t( ( solve(K_m3) - solve(K_m3)%*% M3 %*% (solve( t(M3) %*% solve(K_m3) %*% M3))
               %*%t(M3) %*% solve(K_m3)) %*% k_m3 + solve(K_m3) %*% M3 %*% solve( t(M3) %*% solve(K_m3) %*%
                                                                                    M3) %*% t(m3))
lambda_m4=t( ( solve(K_m4) - solve(K_m4)%*% M4 %*% (solve( t(M4) %*% solve(K_m4) %*% M4))
               %*%t(M4) %*% solve(K_m4)) %*% k_m4 + solve(K_m4) %*% M4 %*% solve( t(M4) %*% solve(K_m4) %*%
                                                                                    M4) %*% t(m4))

# Gaussian

lambda_g1=t( ( solve(K_g1) - solve(K_g1)%*% M1 %*% (solve( t(M1) %*% solve(K_g1) %*% M1))
               %*%t(M1) %*% solve(K_g1)) %*% k_g1 + solve(K_g1) %*% M1 %*% solve( t(M1) %*% solve(K_g1) %*%
                                                                                    M1) %*% t(m1))
lambda_g2=t( ( solve(K_g2) - solve(K_g2)%*% M2 %*% (solve( t(M2) %*% solve(K_g2) %*% M2))
               %*%t(M2) %*% solve(K_g2)) %*% k_g2 + solve(K_g2) %*% M2 %*% solve( t(M2) %*% solve(K_g2) %*%
                                                                                    M2) %*% t(m2))
lambda_g3=t( ( solve(K_g3) - solve(K_g3)%*% M3 %*% (solve( t(M3) %*% solve(K_g3) %*% M3))
               %*%t(M3) %*% solve(K_g3)) %*% k_g3 + solve(K_g3) %*% M3 %*% solve( t(M3) %*% solve(K_g3) %*%
                                                                                    M3) %*% t(m3))
lambda_g4=t( ( solve(K_g4) - solve(K_g4)%*% M4 %*% (solve( t(M4) %*% solve(K_g4) %*% M4))
               %*%t(M4) %*% solve(K_g4)) %*% k_g4 + solve(K_g4) %*% M4 %*% solve( t(M4) %*% solve(K_g4) %*%
                                                                                    M4) %*% t(m4))
# Exponential

lambda_e1=t( ( solve(K_e1) - solve(K_e1)%*% M1 %*% (solve( t(M1) %*% solve(K_e1) %*% M1))
               %*%t(M1) %*% solve(K_e1)) %*% k_e1 + solve(K_e1) %*% M1 %*% solve( t(M1) %*% solve(K_e1) %*%
                                                                                    M1) %*% t(m1))
lambda_e2=t( ( solve(K_e2) - solve(K_e2)%*% M2 %*% (solve( t(M2) %*% solve(K_e2) %*% M2))
               %*%t(M2) %*% solve(K_e2)) %*% k_e2 + solve(K_e2) %*% M2 %*% solve( t(M2) %*% solve(K_e2) %*%
                                                                                    M2) %*% t(m2))
lambda_e3=t( ( solve(K_e3) - solve(K_e3)%*% M3 %*% (solve( t(M3) %*% solve(K_e3) %*% M3))
               %*%t(M3) %*% solve(K_e3)) %*% k_e3 + solve(K_e3) %*% M3 %*% solve( t(M3) %*% solve(K_e3) %*%
                                                                                    M3) %*% t(m3))
lambda_e4=t( ( solve(K_e4) - solve(K_e4)%*% M4 %*% (solve( t(M4) %*% solve(K_e4) %*% M4))
               %*%t(M4) %*% solve(K_e4)) %*% k_e4 + solve(K_e4) %*% M4 %*% solve( t(M4) %*% solve(K_e4) %*%
                                                                                    M4) %*% t(m4))





# Calculating mean parameters

# Matern

beta_m1= solve( t(M1) %*% solve(K_m1) %*% M1) %*% t(M1) %*% solve(K_m1) %*% Z1
beta_m2= solve( t(M2) %*% solve(K_m2) %*% M2) %*% t(M2) %*% solve(K_m2) %*% Z2
beta_m3= solve( t(M3) %*% solve(K_m3) %*% M3) %*% t(M3) %*% solve(K_m3) %*% Z3
beta_m4= solve( t(M4) %*% solve(K_m4) %*% M4) %*% t(M4) %*% solve(K_m4) %*% Z4

# Gaussian

beta_g1= solve( t(M1) %*% solve(K_g1) %*% M1) %*% t(M1) %*% solve(K_g1) %*% Z1
beta_g2= solve( t(M2) %*% solve(K_g2) %*% M2) %*% t(M2) %*% solve(K_g2) %*% Z2
beta_g3= solve( t(M3) %*% solve(K_g3) %*% M3) %*% t(M3) %*% solve(K_g3) %*% Z3
beta_g4= solve( t(M4) %*% solve(K_g4) %*% M4) %*% t(M4) %*% solve(K_g4) %*% Z4

# Exponential

beta_e1= solve( t(M1) %*% solve(K_e1) %*% M1) %*% t(M1) %*% solve(K_e1) %*% Z1
beta_e2= solve( t(M2) %*% solve(K_e2) %*% M2) %*% t(M2) %*% solve(K_e2) %*% Z2
beta_e3= solve( t(M3) %*% solve(K_e3) %*% M3) %*% t(M3) %*% solve(K_e3) %*% Z3
beta_e4= solve( t(M4) %*% solve(K_e4) %*% M4) %*% t(M4) %*% solve(K_e4) %*% Z4



# Performing kriging

# Matern
kriging_m1= lambda_m1 %*% Z1
kriging_m2= lambda_m2 %*% Z2
kriging_m3= lambda_m3 %*% Z3
kriging_m4= lambda_m4 %*% Z4

# Gaussian 

kriging_g1= lambda_g1 %*% Z1
kriging_g2= lambda_g2 %*% Z2
kriging_g3= lambda_g3 %*% Z3
kriging_g4= lambda_g4 %*% Z4


# Exponential

kriging_e1= lambda_e1 %*% Z1
kriging_e2= lambda_e2 %*% Z2
kriging_e3= lambda_e3 %*% Z3
kriging_e4= lambda_e4 %*% Z4


# Gaussian

rmse_g1<-rmse(test_S1[,5],kriging_g1)
rmse_g2<-rmse(test_S2[,5],kriging_g2)
rmse_g3<-rmse(test_S3[,5],kriging_g3)
rmse_g4<-rmse(test_S4[,5],kriging_g4)

# Exponential

rmse_e1<-rmse(test_S1[,5],kriging_e1)
rmse_e2<-rmse(test_S2[,5],kriging_e2)
rmse_e3<-rmse(test_S3[,5],kriging_e3)
rmse_e4<-rmse(test_S4[,5],kriging_e4)



actual_values<-c(test_S1[,5],test_S2[,5],test_S3[,5],test_S4[,5])

kriging_values_gauss<-c(kriging_g1,kriging_g2,kriging_g3,kriging_g4)

kriging_values_expo<-c(kriging_e1,kriging_e2,kriging_e3,kriging_e4)

rmse_gauss<-rmse(actual_values,kriging_values_gauss)

rmse_expo<-rmse(actual_values,kriging_values_expo)


longitude<-c(test_S1[,1],test_S2[,1],test_S3[,1],test_S4[,1])
latitude<-c(test_S1[,2],test_S2[,2],test_S3[,2],test_S4[,2])


par(mfrow=c(1,2))
quilt.plot(dataplot[,1],dataplot[,2],dataplot[,3],xlab="Longitude",ylab="Latitude",main="Roughness index-Cluster 2")

par(mfrow=c(1,2))
quilt.plot(test1[,1],test[,2],actual)

# For isotropic
par(mfrow=c(1,2))
quilt.plot(test1[,1],test1[,2],actual,main="Actual values",xlab="Longitude",ylab="Latitude")
quilt.plot(test1[,1],test1[,2],kriging_g,main="Kriging results",xlab="Longitude",ylab="Latitude")

# For locally stationary model
par(mfrow=c(1,2))
quilt.plot(longitude,latitude,actual_values,main="Actual values",xlab="Longitude",ylab="Latitude")
quilt.plot(longitude,latitude,kriging_values_expo,main="Kriging results",xlab="Longitude",ylab="Latitude")