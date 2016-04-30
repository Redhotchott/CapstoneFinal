rm(list=ls())
library(fields)

load('predictors.Rdata')


##########################Covariance Plots
p.types<-c('RA', 'SN', 'FZRA', 'IP')

make.cov.plot<-function(z){
  impt.rows<-which(ptype==z)
  Twb.red<-Twb.prof[impt.rows,1:16]
  cov.mat<-cov(Twb.red)
  image.plot(1:16,1:16,t(cov.mat[16:1,]),main=paste(z, ' Covariance' ),xlab="",ylab="")
}

par(mfrow=c(1,4), mai = c(.7, 0.4, 0.4, .7))
cov.mats<-lapply(p.types,make.cov.plot)
?mai
?lapply
??image.plot

train.rows<-list()
test.labels<-list()
train.lables<-list()
test.rows<-list()



##########################Normality Density Plots
for(i in 1:12){
  train.years=1996:2000+i-1
  test.years=2000+i
  
  print(paste('Training Set: ', i))
  
  train.labels=head(which((years>=train.years[1] & months >8)),1):tail(which(years<=train.years[5]+1 & months <6),1)
  test.labels=which((years==test.years & months>8) | (years==test.years+1 & months < 6))
  
  
  train.rows[[i]]=which(date.ind%in%train.labels)
  test.rows[[i]]=which(date.ind%in%test.labels)
}
#list order RA, SN, FZRA, IP
ptype.rows<-list()
colors<-c('green','blue', 'red', 'orange')


#graphhist <- function(){
for(i in 1:16) {
  
  par(mfrow=c(1,4))
  prof.rain = Twb.prof[ptype=='RA',col=i]
  mean.rain = mean(prof.rain)
  std.rain = sd(prof.rain)
  
  hist(prof.rain, breaks ="FD",xlab="Temperature", main= paste("Rain at Level",i),prob=T, ylim=c(0,0.08))#;axis(4,at=mean.rain,label=round(mean.rain,4))
  curve(dnorm(x, mean=mean.rain, sd=std.rain), col=colors[1], lwd=2, add=TRUE, yaxt="n")
  abline(v=mean.rain,col=colors[1], lwd=3)
  
  prof.snow = Twb.prof[ptype=='SN',col=i]
  mean.snow = mean(prof.snow)
  std.snow = sd(prof.snow)
  
  hist(prof.snow, breaks ="FD",xlab="Temperature", main= paste("Snow at Level",i),prob=T)
  curve(dnorm(x, mean=mean.snow, sd=std.snow), col=colors[2], lwd=2, add=TRUE, yaxt="n")
  abline(v=mean.snow, col=colors[2], lwd=3)
  
  prof.frzrain = Twb.prof[ptype=='FZRA',col=i]
  mean.frzrain = mean(prof.frzrain)
  std.frzrain = sd(prof.frzrain)
  
  hist(prof.frzrain, breaks ="FD",xlab="Temperature", main= paste("Freezing Rain at Level",i),prob=T)
  curve(dnorm(x, mean=mean.frzrain, sd=std.frzrain), col=colors[3], lwd=2, add=TRUE, yaxt="n")
  abline(v=mean.frzrain, col=colors[3], lwd=3)
  
  prof.ip = Twb.prof[ptype=='IP',col=i]
  mean.ip = mean(prof.ip)
  std.ip = sd(prof.ip)
  
  hist(prof.ip, breaks ="FD",xlab="Temperature", main= paste("Ice Pellets at Level",i),prob=T)
  curve(dnorm(x, mean=mean.ip, sd=std.ip), col=colors[4], lwd=2, add=TRUE, yaxt="n")
  abline(v=mean.ip, col=colors[4], lwd=3)
 
  par(mfrow=c(1,4))
  qqnorm(prof.rain, main = paste("Rain at Level ", i),col = "blue"); qqline(prof.rain)
  qqnorm(prof.snow, main = paste("Snow at Level ", i), col = "darkgreen"); qqline(prof.snow)
  qqnorm(prof.frzrain, main = paste("Freezing Rain at Level ", i), col = "purple"); qqline(prof.frzrain)
  qqnorm(prof.ip, main = paste("Ice Pellets at Level ", i), col = "red"); qqline(prof.ip)

}



#sample weather plot for each 
ra<-sample(which(ptype=='RA'),1)
sn<-sample(which(ptype=='SN'),1)
fzra<-sample(which(ptype=='FZRA'),1)
ip<-sample(which(ptype=='IP'),1)

par(mfrow=c(1,1))
step.size=seq(0,3000,by=100)
step.size.matrix
ptype.list<-list()
ptype.list[[3]]<-which(ptype=='FZRA')
ptype.list[[4]]<-which(ptype=='IP')
ptype.list[[2]]<-which(ptype=='SN')
ptype.list[[1]]<-which(ptype=='RA')

chosen<-sapply(ptype.list,sample,size=1, simplify='array')
plot(Twb.prof[1,],step.size,xlim=c(240,290),type='n',xlab='Temperature',ylab='m AGL', main='Sample Precipitation Plot')
for ( i in 1:4){
  lines(Twb.prof[chosen[i],],step.size,col=colors[i])
}
abline(v=273)
legend('bottomleft', legend=c('RA','SN','FZRA','IP'),col=colors, lty=1)
