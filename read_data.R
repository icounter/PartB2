##This file amis to do the copula calculation from the paper


########################calculate utility



rm(list=ls(all=TRUE))

##read data 
##combine data
library(ggplot2)

library(RODBC)
library(MASS)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk1.8.0_71/jre')
library(xlsx)
library(fTrading)
#source("http://bioconductor.org/biocLite.R"); biocLite(c("graph","RBGL","Rgraphviz"))
library(ggplot2)
library(gRbase)
library(gRain)
library(LambertW)

data1<-read.xlsx("C:/Users/czhu/Desktop/data/Friday group meeting/datasource.xls",1)

# for(i in 1:nrow(data2)-1){
#    data2[i,2]=(data2[i,2]/data2[i+1,2])-1
# }
##drop last column price
# data2<-data2[-nrow(data2),]
###merge data
#data_merge<-merge(data1,data2,by='Date')
#order as date
data<-data1[order(as.Date(data1$Date, format="%Y-%m-%d")),]
data<-data.frame(data)
colnames(data)<-c("Date","Bond","Credit","Mortgage","Equity")
data<-data[,c(1,2,3,5,4)]
# mean_return<-rep(1,4)
# mean_return=foreach(i=2:5, .combine='c') %do%mean(data[,i])
# val_return=foreach(i=2:5, .combine='c') %do%var(data[,i])
# #################shift the price
# real_vol=c(0.044,0.046,0.184,0.028)/sqrt(252)
# val_return2<-sqrt(val_return)
# for(i in rep(2:5)){
#   data[,i]=(data[,i]-mean_return[i-1])/val_return2[i-1]*real_vol[i-1]+mean_return[i-1]
# }
# real_return<-c(0.069,0.079,0.078,0.064)
# shift<-(real_return-mean_return*252)/252
# for(i in rep(2:5)){
#   data[,i]=data[,i]+shift[i-1]
# }

###draw hisogram picture
hist(data$Bond,breaks=seq(min(data$Bond)-0.001,max(data$Bond)+0.001,by=0.001))
hist(data$Credit,breaks=seq(min(data$Credit)-0.001,max(data$Credit)+0.001,by=0.001))
hist(data$Mortgage,breaks=seq(min(data$Mortgage)-0.001,max(data$Mortgage)+0.001,by=0.001))
hist(data$Equity,breaks=seq(min(data$Equity)-0.001,max(data$Equity)+0.001,by=0.001))
#write.csv(data,file="C:/Users/czhu/Desktop/data/Friday group meeting/datasource.csv")
# #delete the date column
date_MVE<-data[,1]
data_MVE<-data[,-1]
# ##MVE ro removw outlier 
# # ##naive process
# # for( i in 1:N){
# # sprintf("start to calculate %d",i)
# # #first calculate the total volume
# # cov_vector[i]=det(cov.mve(data_MVE,cor=TRUE,nsamp="best")$cov)
# # cor_vector[i]=det(cov.mve(data_MVE,cor=TRUE,nsamp="best")$cor)
# # temp_cov_vector<-rep(1:nrow(data_MVE))
# # temp_cor_vector<-rep(1:nrow(data_MVE))
# # for( k in 1:nrow(data_MVE)){
# #   print(k)
# #   temp_cov_vector[k]<-det(cov.mve(data_MVE[-k,],cor=TRUE,nsamp="best")$cov)
# #   temp_cov_vector[k]<-det(cov.mve(data_MVE[-k,],cor=TRUE,nsamp="best")$cor)
# # }
# # k=which.min(temp_cov_vector)
# # data_MVE<-data_MVE[-k,]
# # cov_vector<-temp_cov_vector[k]
# # cor_vector<-temp_cor_vector[k]
# # }
###second step
N=floor(0.05*nrow(data_MVE))
cov_vector<-rep(1:N)
cor_vector<-rep(1:N)
volume_vector<-rep(1:N)
namelist<-rep(1:N)
for( i in 1:N){
set.seed(123)
print(i)
tt<-cov.mve(data_MVE,cor=TRUE,quantile=nrow(data_MVE)-1)
#center_data=tt$center
temp<-tt$best
total<-1:nrow(data_MVE)
k=setdiff(total,temp)
#tt1<-cov.mve(data_MVE[-k,],cor=TRUE,quantile=nrow(data_MVE)-2)
cov_vector[i]=det(tt$cov)
cor_vector[i]=det(tt$cor)
volume_vector[i]=tt$crit
namelist[i]<-date_MVE[k]
data_MVE<-data_MVE[-k,]
date_MVE<-date_MVE[-k]
}
matplot(cov_vector, xlab = "number of data points removed",ylab="Determinant",main="Covariance matrix determinant",type = "s", lwd=2,col=1 )
matplot(cor_vector,xlab = "number of data points removed",ylab="Determinant",main="Correlation matrix determinant" ,type = "S",lwd=2,col=1 )
matplot(volume_vector,xlab = "number of data points removed",ylab="Volume",main="Ellipsoid volume" ,type="S",lwd=2,col=4)
###after get the outlier : how to do the 
data_body=data
#data_extreme
name<-rep(1:N)
for (g in 1:length(namelist)){
  name[g]<-which(as.numeric(data_body$Date)==namelist[g])
}
data_body=data[-name,]
extreme=data[name,]

library(rgl)
open3d()
x1<-(rbind(data_body,extreme))$Bond
y1<-(rbind(data_body,extreme))$Equity
z1<-(rbind(data_body,extreme))$Credit
plot3d(x1,y1,z1,xlab="Bond",ylab="Equity",zlab="Credit",box=TRUE,col=c(rep(4,3184),rep(2,167)))


###################################################plot histogram

####################################################
###############second step par marginal and coupla
###################################################
library(copula)

###calcualte the mean
cor(data_body[,2:5])
mean_return<-rep(1,4)
library(foreach)
mean_return=foreach(i=2:5, .combine='c') %do%mean(data_body[,i])
val_return=foreach(i=2:5, .combine='c') %do%var(data_body[,i])
#################shift the price
real_vol=c(0.044,0.046,0.184,0.028)/sqrt(252)
val_return2<-sqrt(val_return)
for(i in rep(2:5)){
  data_body[,i]=(data_body[,i]-mean_return[i-1])/val_return2[i-1]*real_vol[i-1]+mean_return[i-1]
}
real_return<-c(0.069,0.079,0.078,0.064)
shift<-(real_return-mean_return*252)/252
for(i in rep(2:5)){
  data_body[,i]=data_body[,i]+shift[i-1]
}
mean_return=foreach(i=2:5, .combine='c') %do%mean(data_body[,i])
val_return=foreach(i=2:5, .combine='c') %do%var(data_body[,i])



############estimate mariginal distribution
#normallity test
shapiro.test(data_body$Bond)
shapiro.test(data_body$Credit)
shapiro.test(data_body$Mortgage)
shapiro.test(data_body$Equity)

library(fGarch)
library(QRM)
##tdistribution test
Bond_ks_test<-ks.test.t(data_body$Bond)
#Bond_fit<-fit.st(data_body$Bond)$par.ests
Bond_fit<-stdFit(data_body$Bond)$par
Credit_ks_test<-ks.test.t(data_body$Credit)
Credit_fit<-stdFit(data_body$Credit)$par
Mortgage_ks_test<-t.test(data_body$Mortgage)
Mortgage_fit<-stdFit(data_body$Mortgage)$par
Equity_ks_test<-ks.test.t(data_body$Equity)
Equity_fit<-stdFit(data_body$Equity)$par
generate_non_central_t<<-function(n,mu,sd,df){
  return (rstd(n,mu,sd,df))
}
par(mfrow=c(1,2))
qqnorm(data_body$Bond,main="QQ plot of Sample Data versus Normal Distribution",xlab="Quantiles of Normal Distribution",ylab="Quantiles of Sample"); qqline(data_body$Bond, col = 2)
qqplot(generate_non_central_t(length(data_body$Bond),Bond_fit[1],Bond_fit[2],Bond_fit[3]),data_body$Bond,main="QQ plot of Sample Data versus student t Distribution",xlab="Quantiles of student t Distribution",ylab="Quantiles of Sample")
abline(mean(data_body$Bond),1,col="red")
par(mfrow=c(1,1))



hist(data_body$Equity,breaks=seq(min(data_body$Equity)-0.001,max(data_body$Equity)+0.001,by=0.001),freq=FALSE,main='Histogram of Equity',xlab='Equity')
Equity_fit2<-fitdistr(data_body$Equity,"normal")
lines(seq(from=min(data_body$Equity),to=max(data_body$Equity),length=1000),dnorm(seq(from=min(data_body$Equity),to=max(data_body$Equity),length=1000),mean=Equity_fit2$estimate[1],sd=Equity_fit2$estimate[2]),type='l',col='blue')
lines(seq(from=min(data_body$Equity),to=max(data_body$Equity),length=1000),dstd(seq(from=min(data_body$Equity),to=max(data_body$Equity),length=1000),mean=Equity_fit[1],sd=Equity_fit[2],nu=Equity_fit[3]),type='l',col='red')
legend('topright',
       c('normal','student t'), # puts text in the legend
       
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       
       lwd=c(2.5,2.5),col=c('blue','red')) # gives the legend lines the correct color and width




######a t-distribution will make more sense
###a work out example
get_non_central<<-function(q,mu,sd,df){
  prob<-pstd(q,mu,sd,df)
  return(prob)
}
(tCop <- tCopula(c(0.2,0.4,0.6,0.8,-0.9,0.2), dim=4, dispstr="un", df=2))
u<-data_body[,-1]
for(hh in 1:nrow(u)){
  u[hh,1]=get_non_central(u[hh,1],Bond_fit[1],Bond_fit[2],Bond_fit[3])
  u[hh,2]=get_non_central(u[hh,2],Credit_fit[1],Credit_fit[2],Credit_fit[3])
  u[hh,3]=get_non_central(u[hh,3],Equity_fit[1],Equity_fit[2],Equity_fit[3])
  u[hh,4]=get_non_central(u[hh,4],Mortgage_fit[1],Mortgage_fit[2],Mortgage_fit[3])
}
u2<-pobs(u)
(tc.ml. <- fitCopula(tCop, u2, method="mpl"))
summary(tc.ml.)
tCop_par<-tCopula(summary(tc.ml.)$coefficients[1:6,1],dim=4, dispstr="un", df=5.93)
#####################################################goodness of fit
#gofCopula(tCop_par,u,simulation="mult")

###################################################################
#############################extreme events
#####the assumption can be changed!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
extreme_stress_loss=c(-0.0008,-0.003,-0.0016,-.0006)
marigin_probability=c(0.04,0.07,0.07,0.03)
###can be generate automatically 
###bond,credit,equity,mortgage
pro_dict=list(p0=c(0.8381,0,0,0,0),p1=c(0.0208,0,0,0,1),p2=c(0.0460,0,0,1,0),p3=c(0.0446,0,1,0,0),p4=c(0.0117,1,0,0,0),p5=c(0.0011,0,0,1,1),p6=c(0.0065,0,1,0,1)
              ,p7=c(0.0025,0,1,1,0),p8=c(0.0003,1,0,0,1),p9=c(0.0117,1,0,1,0)
              ,p10=c(0.0077,1,1,0,0),p11=c(0.0004,0,1,1,1),p12=c(0.0003,1,0,1,1),p13=c(0.0003,1,1,0,1),p14=c(0.0077,1,1,1,0),p15=c(0.0003,1,1,1,1))
pro_dict<-data.frame(pro_dict)
#####autogeneration
get_non_central_t_pr<-function(p,mu,sd,df){
  dens<-qstd(p,mu,sd,df)
  return(dens)
}
N=10000
rand<-rCopula(N,tCop_par)
#rand<-pobs(rand)
rand2<<-rand
for( i in 1:N){
  rand2[i,1]=get_non_central_t_pr(rand[i,1],Bond_fit[1],Bond_fit[2],Bond_fit[3])
  rand2[i,2]=get_non_central_t_pr(rand[i,2],Credit_fit[1],Credit_fit[2],Credit_fit[3])
  rand2[i,3]=get_non_central_t_pr(rand[i,3],Equity_fit[1],Equity_fit[2],Equity_fit[3])
  rand2[i,4]=get_non_central_t_pr(rand[i,4],Mortgage_fit[1],Mortgage_fit[2],Mortgage_fit[3])
}

###scale rand2
mean_return=foreach(i=1:4, .combine='c') %do%mean(rand2[,i])
val_return=foreach(i=1:4, .combine='c') %do%var(rand2[,i])
#################shift the price
real_vol=c(0.044,0.046,0.184,0.028)/sqrt(252)
val_return2<-sqrt(val_return)
for(i in rep(1:4)){
  rand2[,i]=(rand2[,i]-mean_return[i])/val_return2[i]*real_vol[i]+mean_return[i]
}
real_return<-c(0.069,0.079,0.078,0.064)
shift<-(real_return-mean_return*252)/252
for(i in rep(1:4)){
  rand2[,i]=rand2[,i]+shift[i]
}
mean_return=foreach(i=1:4, .combine='c') %do%mean(rand2[,i])
val_return=foreach(i=1:4, .combine='c') %do%var(rand2[,i])

rand2<-rand2[sample(10000,3184),]


# serie<-1:nrow(rand2)
# serie2<-sample(serie,3350)
# rand2<-rand2[serie,]
hist(rand2[,1],breaks=seq(min(rand2[,1])-0.001,max(rand2[,1])+0.01,by=0.001))
hist(data_body$Bond,breaks=seq(min(data_body$Bond)-0.001,max(data_body$Bond)+0.001,by=0.001))
hist(rand2[,2],breaks=seq(min(rand2[,2])-0.001,max(rand2[,2])+0.01,by=0.001))
hist(data_body$Credit,breaks=seq(min(data_body$Credit)-0.001,max(data_body$Credit)+0.001,by=0.001))
hist(rand2[,3],breaks=seq(min(rand2[,3])-0.001,max(rand2[,3])+0.001,by=0.001))
hist(data_body$Equity,breaks=seq(min(data_body$Equity)-0.001,max(data_body$Equity)+0.001,by=0.001))
hist(rand2[,4],breaks=seq(min(rand2[,4])-0.001,max(rand2[,4])+0.001,by=0.001))
hist(data_body$Mortgage,breaks=seq(min(data_body$Mortgage)-0.001,max(data_body$Mortgage)+0.001,by=0.001))


par(mfrow=c(1,2))
plot(Bond~Credit,data=data_body,main='Bond~Cedit joint distribution',ylab='Bond',xlab='Credit')
plot(rand2[,1]~rand2[,2],main='Bond~Credit joint distribution(Simulation)',ylab='Bond',xlab='Credit')
plot(Equity~Mortgage,data=data_body)
plot(rand2[,3]~rand2[,4])
plot(data_body[,2:5])
rand22<-data.frame(rand2)
plot(rand22[,1:4])
par(mfrow=c(1,1))

######utility_function
power_uility<-function(x,beta){
  u<-1/(1-beta)*(x^(1-beta)-1)
  return(u)
}
log_uility<-function(x){
  u<-log(x)
  return(u)
}
get_power_ut<-function(ret,beta){
  return(power_uility(1+ret,beta))
}
get_log_ut<-function(ret){
  return(log_uility(1+ret))
}
get_extreme_power_uti<<-function(pro_dict,loss,w,beta){
  rett<-0
  for(i in 2:ncol(pro_dict)){
    rett<-rett+pro_dict[1,i]*power_uility((1+sum(pro_dict[2:5,i]*w*loss)),beta)
  }
  return(rett)
}
get_extreme_log_uti<<-function(pro_dict,loss,w,beta){
  rett<-0
  for(i in 2:ncol(pro_dict)){
    rett<-rett+pro_dict[1,i]*log_uility((1+sum(pro_dict[2:5,i]*w*loss)))
  }
  return(rett)
}
get_bounds<<-function(w1){
  w=c(w1[1],w1[2],w1[3],1-(w1[1]+w1[2]+w1[3]))
  return(w)
}


w<-c(0.25,0.25,0.25,0.25)

loss1<<-extreme_stress_loss
prodict1<<-pro_dict
power_find_w<-function(w1){
  exp_pow_ut<-(1-k)*mean(get_power_ut(rand2 %*% get_bounds(w1),beta1))+(k/(1-pro_dict$p0[1]))*get_extreme_power_uti(pro_dict,loss1,get_bounds(w1),beta1)
  return(-exp_pow_ut)
}
log_find_w<-function(w1){
  exp_log_ut<-(1-k)*mean(get_log_ut(rand2 %*% get_bounds(w1)))+k/(1-pro_dict$p0[1])*get_extreme_log_uti(prodict1,loss1,get_bounds(w1))
  return(-exp_log_ut)  
}

# power_find_w2<-function(w1){
#   exp_pow_ut<-(1-k)*mean(get_power_ut(data_body22 %*% get_bounds(w1),beta1))+(k/(1-pro_dict$p0[1]))*get_extreme_power_uti(pro_dict,loss1,get_bounds(w1),beta1)
#   return(-exp_pow_ut)
# }

k<<-0.03
beta1<<-0.08
 w1<-runif(3,max=0.3)
 ui<-rbind(c(1,0,0),c(0,1,0),c(0,0,1),c(-1,-1,-1))
 ci=c(0,0,0,-1)
 constrOptim(w1,power_find_w,NULL, ui, ci)
 #data_body22<<-as.matrix(data_body[,-1])
#  w1=c(0.00001,0.91,0.08)
# power_find_w2(w1)
# w1=c(3.589352e-08, 4.499405e-01, 5.500563e-01)


 beta1<<-0.8
a<-matrix(0,nrow=26,ncol=4)
i=1
for( k in  seq(from=.25,to=0,by=-0.01)){
print(k)
  set.seed(121)
  w1<-runif(3,max=0.3)
  ttt<-constrOptim(w1,power_find_w,NULL, ui, ci)
  a[i,1]<-ttt$par[1]
  a[i,2]<-ttt$par[2]
  a[i,3]<-ttt$par[3]
  a[i,4]<-1-sum(ttt$par)
  i=i+1
}
y<-seq(from=0.75,to=1,by=0.01)
myres<-as.data.frame(a)
colnames(myres)=c("Bond","Credit","Equity","Mortgage")

matplot(y,myres, type = c("b"),pch=1,col = 1:4,main='Optimized Portfolio Weights,beta=0.8',xlab='1-k',ylab='weights') #plot
legend("topleft", legend =c("Bond","Credit","Equity","Mortgage"), col=1:4, pch=1) # optional legend

b<-matrix(0,nrow=26,ncol=4)
i=1
for( k in  seq(from=0.25,to=0,by=-0.01)){
  print(k)
  set.seed(121)
  w1<-runif(3,max=0.3)
  ttt<-constrOptim(w1,log_find_w,NULL, ui, ci)
  b[i,1]<-ttt$par[1]
  b[i,2]<-ttt$par[2]
  b[i,3]<-ttt$par[3]
  b[i,4]<-1-sum(ttt$par)
  i=i+1
}
y<-seq(from=0.75,to=1,by=0.01)
myres2<-as.data.frame(b)
colnames(myres)=c("Bond","Credit","Equity","Mortgage")

matplot(y,myres2, type = c("b"),pch=1,col = 1:4,main='Optimized Portfolio Weights,log utility',xlab='1-k',ylab='weights') #plot
legend("topleft", legend =c("Bond","Credit","Equity","Mortgage"), col=1:4, pch=1) # optional legend

beta1<<-1.2
c<-matrix(0,nrow=26,ncol=4)
i=1
for( k in  seq(from=0.25,to=0,by=-0.01)){
  print(k)
  set.seed(121)
  w1<-runif(3,max=0.3)
  ttt<-constrOptim(w1,power_find_w,NULL, ui, ci)
  c[i,1]<-ttt$par[1]
  c[i,2]<-ttt$par[2]
  c[i,3]<-ttt$par[3]
  c[i,4]<-1-sum(ttt$par)
  i=i+1
}
y<-seq(from=0.75,to=1,by=0.01)
myres3<-as.data.frame(c)
colnames(myres)=c("Bond","Credit","Equity","Mortgage")

matplot(y,myres3, type = c("b"),pch=1,col = 1:4,main='Optimized Portfolio Weights,beta=1.2',xlab='1-k',ylab='weights') #plot
legend("topleft", legend =c("Bond","Credit","Equity","Mortgage"), col=1:4, pch=1) # optional legend


#####uility function
power_uility<-function(x,beta){
  u<-1/(1-beta)*(x^(1-beta)-1)
  return(u)
}
x=seq(from=1,to=100,seq=1000)
par(mfrow=c(1,3))
y1=power_uility(x,0.6)
plot(x,y1,pch=1,col=1,type='l',main='power utility  beta=0.6',ylab='utility',xlab='input')
y2=log(x)
plot(x,y2,pch=1,col=1,type='l',main='log utility function',ylab='utility',xlab='input')
y=power_uility(x,1.4)
plot(x,y,pch=1,col=1,type='l',main='power utility  beta=1.4',ylab='utility',xlab='input')