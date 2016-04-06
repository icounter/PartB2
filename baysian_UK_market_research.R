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
  
  ######baysian network setup
  yn <- c("1","0")
  a <- cptable(~soevereigncrisis, values=c(1,99),levels=yn)
  b.a<- cptable(~disseurobank|soevereigncrisis, values=c(2,8,1,9),levels=yn)
  c.ab<-cptable(~dissukbank|soevereigncrisis:disseurobank, values=c(9,1,7,3,8,2,1,9),levels=yn)
  d.c<-cptable(~qeuk|dissukbank,values=c(2,8,1,9),levels=yn)
  e.bca<-cptable(~major|disseurobank:dissukbank:soevereigncrisis,values=c(9,1,7,3,8,2,1,9,99,1,77,23,88,12,11,89),levels=yn)
  f.c<-cptable(~govresec|dissukbank,values=c(2,8,1,9),levels=yn)
  g.c<-cptable(~restcredit|dissukbank,values=c(9,1,5,5),levels=yn)
  h.e<-cptable(~ukcorspr|major,values=c(9,1,5,5),levels=yn)
  k.deg<-cptable(~ukequitypricefall|qeuk:major:restcredit,values=c(9,1,7,3,8,2,1,9,99,1,77,23,88,12,11,89),levels=yn)
  l.def<-cptable(~giltspricefall|qeuk:major:govresec,values=c(9,1,7,3,8,2,1,9,99,1,77,23,88,12,11,89),levels=yn)
  plist <- compileCPT(list(a,b.a,c.ab,d.c,e.bca,f.c,g.c,h.e,k.deg,l.def))
  net1 <- grain(plist)
  bbb<-querygrain(net1,nodes=c("soevereigncrisis","disseurobank","dissukbank","qeuk","major","govresec","restcredit","ukcorspr","ukequitypricefall","giltspricefall"), type="joint")
  ddd1<-data.frame(bbb)
  n=10
  pro_dict<-matrix(0,nrow=2^n,ncol=n+1)
  k<-1
  stringsplit<-function(string){
    b<-seq(1,(length(a)+1)/2)
    a<-strsplit(string,NULL)[[1]]
    ll<-(length(a)+1)/2
    for(i in 1:ll){
      b[i]<-as.numeric(a[2*i-1])
    }
    return(b)
  }
  for(i in 1:ncol(ddd1)){
    for(j in 1:2){
     string<-paste0(row.names(ddd1)[j],".",substr(colnames(ddd1)[i],2,nchar(colnames(ddd1)[i])))
     value<-ddd1[j,i]
     pro_dict[k,1]=value
     pro_dict[k,2:11]=stringsplit(string)
    k=k+1 
    }
  }
  pro_dict<-data.frame(pro_dict)
  library(data.table)
  joint_table<-aggregate((pro_dict[,1]), as.list(pro_dict[,9:11]), FUN = sum)
  joint_table=t(joint_table[,c(4,1,2,3)])

  ############################get three asstes joint pro table
  ##############next step is to clean data
  data1<-read.xlsx("C:/Users/czhu/Desktop/data/Friday group meeting/FTSE GUGBK.xlsx",2)
  data1<-data.frame(data1)
  data<-data1[,c(1,2,4,3)]
  data<-na.omit(data)
  colnames(data)<-c("Date","credit","Equity","Gilts")
  
  # mean_return<-rep(1,3)
   mean_return=foreach(i=2:4, .combine='c') %do%mean(data[,i])
   val_return=foreach(i=2:4, .combine='c') %do%var(data[,i]) 
  date_MVE<-data[,1]
  data_MVE<-data[,-1]
  N=floor(0.1*nrow(data_MVE))
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
  matplot(cor_vector, type = "S",lwd=2,col=3 )
  matplot(volume_vector,type="S",lwd=2,col=4)
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
  x1<-(rbind(data_body,extreme))$credit
  y1<-(rbind(data_body,extreme))$Equity
  z1<-(rbind(data_body,extreme))$Gilts
  plot3d(x1,y1,z1,xlab="Credit",ylab="Equity",zlab="Gilt",box=TRUE,col=c(rep(4,4094),rep(2,215)))
 
  
  
  
  
  
  library(copula)
  qqnorm(data_body$Equity); qqline(data_body$Equity, col = 2)
  ###calcualte the mean
  cor(data_body[,2:4])
#   mean_return=foreach(i=2:4, .combine='c') %do%mean(data_body[,i])
#   val_return=foreach(i=2:4, .combine='c') %do%var(data_body[,i])
  #################shift the price
  shapiro.test(data_body$credit)
  shapiro.test(data_body$Gilts)
  shapiro.test(data_body$Equity)
  library(fGarch)
  library(QRM)
  #Credit_ks_test<-ks.test.t(data_body$credit)
  #Bond_fit<-fit.st(data_body$Bond)$par.ests
  Credit_fit<-stdFit(data_body$credit)$par
  Equity_ks_test<-ks.test.t(data_body$Equity)
  Equity_fit<-stdFit(data_body$Equity)$par
  Gilts_ks_test<-t.test(data_body$Gilts)
  Gilts_fit<-stdFit(data_body$Gilts)$par
  #########################################################
  hist(data_body$Equity,breaks=seq(min(data_body$Equity)-0.001,max(data_body$Equity)+0.001,by=0.001),freq=FALSE,main='Histogram of Equity',xlab='Equity')
  Equity_fit2<-fitdistr(data_body$Equity,"normal")
  lines(seq(from=min(data_body$Equity),to=max(data_body$Equity),length=1000),dnorm(seq(from=min(data_body$Equity),to=max(data_body$Equity),length=1000),mean=Equity_fit2$estimate[1],sd=Equity_fit2$estimate[2]),type='l',col='blue')
  lines(seq(from=min(data_body$Equity),to=max(data_body$Equity),length=1000),dstd(seq(from=min(data_body$Equity),to=max(data_body$Equity),length=1000),mean=Equity_fit[1],sd=Equity_fit[2],nu=Equity_fit[3]),type='l',col='red')
  legend('topright',
         c('normal','student t'), # puts text in the legend
         
         lty=c(1,1), # gives the legend appropriate symbols (lines)
         
         lwd=c(2.5,2.5),col=c('blue','red')) # gives the legend lines the correct color and width
  
  
  
  
  
  
  #####################################################
  get_non_central<<-function(q,mu,sd,df){
    prob<-pstd(q,mu,sd,df)
    return(prob)
  }
  (tCop <- tCopula(c(0.2,0.4,0.6), dim=3, dispstr="un", df=2))
  u<-data_body[,-1]
  for(hh in 1:nrow(u)){
    u[hh,1]=get_non_central(u[hh,1],Credit_fit[1],Credit_fit[2],Credit_fit[3])
    u[hh,2]=get_non_central(u[hh,2],Equity_fit[1],Equity_fit[2],Equity_fit[3])
    u[hh,3]=get_non_central(u[hh,3],Gilts_fit[1],Gilts_fit[2],Gilts_fit[3])
  }
  u2<-pobs(u)
  (tc.ml. <- fitCopula(tCop, u2, method="mpl"))
  summary(tc.ml.)
  tCop_par<-tCopula(summary(tc.ml.)$coefficients[1:3,1],dim=3, dispstr="un", df=summary(tc.ml.)$coefficients[4,1])
  get_non_central_t_pr<-function(p,mu,sd,df){
    dens<-qstd(p,mu,sd,df)
    return(dens)
  }
  N=10000
  rand<-rCopula(N,tCop_par)
  rand<-pobs(rand)
  rand2<<-rand
  for( i in 1:N){
    rand2[i,1]=get_non_central_t_pr(rand[i,1],Credit_fit[1],Credit_fit[2],Credit_fit[3])
    rand2[i,2]=get_non_central_t_pr(rand[i,2],Equity_fit[1],Equity_fit[2],Equity_fit[3])
    rand2[i,3]=get_non_central_t_pr(rand[i,3],Gilts_fit[1],Gilts_fit[2],Gilts_fit[3])
  }
  real_vol<-sqrt(val_return)
  mean_return2=foreach(i=1:3, .combine='c') %do%mean(rand2[,i])
  val_return2=foreach(i=1:3, .combine='c') %do%var(rand2[,i])
  val_return3<-sqrt(val_return2)
  for(i in rep(1:3)){
    rand2[,i]=(rand2[,i]-mean_return2[i])/val_return3[i]*real_vol[i]+mean_return2[i]
  }
  real_return<-c(0.0799,0.061,-0.046)
  shift<-(real_return-mean_return2*252)/252
  for(i in rep(1:3)){
    rand2[,i]=rand2[,i]+shift[i]
  }
  
  mean_return=foreach(i=1:3, .combine='c') %do%mean(rand2[,i])
  val_return=foreach(i=1:3, .combine='c') %do%var(rand2[,i])
  par(mfrow=c(1,2))
  plot(Gilts~Equity,data=data_body,main='Equity~Gilts joint distribution',ylab='Gilts',xlab='Equity')
  rand22<-rand2[sample(nrow(rand2),nrow(data_body)),]
  plot(rand22[,3]~rand22[,2],xlim=c(-0.03,0.03),ylim=c(-0.04,0.04),main='Equity~Gilts joint distribution(Simulation)',ylab='Gilts',xlab='Equity')
  par(mfrow=c(1,1))
  rand22<-rand2[sample(nrow(rand2),nrow(data_body)),]
  hist(rand22[,2],breaks=seq(min(rand22[,2])-0.001,max(rand22[,2])+0.001,by=0.001),main='histogram of Equity(simulation)',xlab='Equity')
  hist(data_body$Equity,breaks=seq(min(data_body$Equity)-0.001,max(data_body$Equity)+0.001,by=0.001),main='histogram of Equity',xlab='Equity')
  
  ###########################
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
      rett<-rett+pro_dict[1,i]*power_uility((1+sum(pro_dict[2:4,i]*w*loss)),beta)
    }
    return(rett)
  }
  get_extreme_log_uti<<-function(pro_dict,loss,w,beta){
    rett<-0
    for(i in 2:ncol(pro_dict)){
      rett<-rett+pro_dict[1,i]*log_uility((1+sum(pro_dict[2:4,i]*w*loss)))
    }
    return(rett)
  }
  get_bounds<<-function(w1){
    w=c(w1[1],w1[2],1-(w1[1]+w1[2]))
    return(w)
  }
  w<-c(0.25,0.25,0.25)
  pro_dict<<-data.frame(joint_table)
  colnames(pro_dict)<-c("p0","p1","p2","p3","p4","p5","p6","p7")
  extreme_stress_loss<-c(-0.4/252,-0.1/252,-0.05/252)
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
  k<<-0.03
  beta1<<-0.08
  w1<-runif(2,max=0.5)
  ui<-rbind(c(1,0),c(0,1),c(-1,-1))
  ci=c(0,0,-1)
  constrOptim(w1,power_find_w,NULL, ui, ci)
  
  beta1<<-0.8
  a<-matrix(0,nrow=26,ncol=3)
  i=1
  for( k in  seq(from=.25,to=0,by=-0.01)){
    print(k)
    set.seed(121)
    w1<-runif(2,max=0.5)
    ttt<-constrOptim(w1,power_find_w,NULL, ui, ci)
    a[i,1]<-ttt$par[1]
    a[i,2]<-ttt$par[2]
    a[i,3]<-1-sum(ttt$par)
    i=i+1
  }
  y<-seq(from=0.25,to=0,by=-0.01)
  myres<-as.data.frame(a)
  colnames(myres)=c("Credit","Equity","Gilts")
  
  matplot(y,myres, type = c("b"),pch=1,col = 1:3) #plot
  legend("topright", legend =c("Credit","Equity","Gilts"), col=1:3, pch=1) # optional legend
  
  
  
