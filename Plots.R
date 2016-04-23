library(fields)

load('predictors.Rdata')


#Covariance Plots
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
