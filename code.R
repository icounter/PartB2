library(bnlearn)
data(learning.test)
res = gs(learning.test)
plot(res)
res2 = iamb(learning.test)
plot(res2)
all.equal(res, res2)
ntests(res)
res = hc(learning.test)
plot(res)
data(gaussian.test)
res = gs(gaussian.test)
plot(res)
blacklist = data.frame(from = c("B", "F"), to = c("F", "B"))
blacklist
res3 = gs(learning.test, blacklist = blacklist)
plot(res3)
whitelist = data.frame(from = c("E"), to = c("F"))
whitelist
###############################################################
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
library(bnlearn)
data1<-read.xlsx("C:/Users/czhu/Desktop/data/Friday group meeting/Brexit_data_sample.xlsx",10)

for( i in 1:ncol(data1)){
  data1[,i]<-factor(data1[,i])  
}
data1<-data1[,-1]
#data1<-data1[1:20,]
whitelist = data.frame(from = c("LEAVE","LEAVE","LEAVE"), to = c("GBPUSD.Curncy","GBPEUR.Curncy","UKX.Index"))
res = hc(data1,whitelist=whitelist)
plot(res)
res2 = iamb(data1,whitelist=whitelist)
plot(res2)
res3=hc(data1,whitelist=whitelist)
plot(res3)
res5=hc(data1)
plot(res5)
res = gs(data1, debug = TRUE)
fitted=bn.fit(res,data1)

####################################
data(learning.test)
# learn the network structure.
res = gs(learning.test)
# set the direction of the only undirected arc, A - B.
res = set.arc(res, "A", "B")
# estimate the parameters of the Bayesian network.
fitted = bn.fit(res, learning.test)
# replace the parameters of the node B.
new.cpt = matrix(c(0.1, 0.2, 0.3, 0.2, 0.5, 0.6, 0.7, 0.3, 0.1),
                 byrow = TRUE, ncol = 3,
                 dimnames = list(B = c("a", "b", "c"), A = c("a", "b", "c")))
fitted$B = as.table(new.cpt)
# the network structure is still the same.
all.equal(res, bn.net(fitted))
# learn the network structure.
res = hc(gaussian.test)
# estimate the parameters of the Bayesian network.
fitted = bn.fit(res, gaussian.test)
# replace the parameters of the node F.
fitted$F = list(coef = c(1, 2, 3, 4, 5), sd = 3)
# set again the original parameters
fitted$F = lm(F ~ A + D + E + G, data = gaussian.test)


