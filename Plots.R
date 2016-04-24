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

par(mfrow=c(2,2))
cov.mats<-lapply(p.types,make.cov.plot)

?lapply
??image.plot


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

make.dens.plot<-function(p.rows){
  means<-apply(Twb.prof[p.rows,],2,mean)
  covs<-apply(Twb.prof[p.rows,],2, cov)
  
  
  
}
