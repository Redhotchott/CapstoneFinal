#PRINCOMP

rm(list=ls())
load("predictors.RData")
library(ggplot2)
library(caret)
library(plot3D)
library(e1071)

cols=1:31      #Columns (i.e. levels) of the temperature profiles to be used in the
years=as.numeric(substr(dates,1,4))
months=as.numeric(substr(dates,5,6))
all.months=as.numeric(substr(dates[date.ind],5,6))

S<-cov(Twb.prof)
e.vals<- eigen(S)$values
tot.var<-sum(e.vals)
round(prop<-e.vals/tot.var,3)
# This appears to indicate that we may only need 3 eigenvalues to describe everything.

par(mfrow=c(1,1))
total<-princomp(Twb.prof,cor=T)
names(total)
loadings(total)
S<-cov(Twb.prof)
eigen(S)$vectors[,1:2]
mean(eigen(S)$values)
summary(total)
print(total)
plot(total, type='l')

par(mfrow=c(2,2))
ra.rows<-which(ptype=="RA") #3 prin
ra<-princomp(Twb.prof[ra.rows,],cor=T)
Sra<-cov(Twb.prof[ra.rows,])
mean(eigen(Sra)$values) #36, not helpful
summary(ra)
print(ra)
plot(ra, type='l')

sn.rows<-which(ptype=="SN") #4 prin
sn<-princomp(Twb.prof[sn.rows,],cor=T)
Ssn<-cov(Twb.prof[sn.rows,])
mean(eigen(Ssn)$values)
summary(sn)
print(sn)
plot(sn, type='l')

fz.rows<-which(ptype=="FZRA") #4 prin
fz<-princomp(Twb.prof[fz.rows,],cor=T)
Sfz<-cov(Twb.prof[fz.rows,])
mean(eigen(Sfz)$values)
summary(fz)
print(fz)
plot(fz, type='l')

ip.rows<-which(ptype=="IP") #4 prin
ip<-princomp(Twb.prof[ip.rows,],cor=T)
Sip<-cov(Twb.prof[ip.rows,])
mean(eigen(Sip)$values)
summary(ip)
print(ip)
plot(ip, type='l')


par(mfrow=c(1,4), mai=c(1,0.5,0.5,1))
plot(ra,type='l')
plot(sn,type='l')
plot(fz,type='l')
plot(ip,type='l')

log.Twb.prof <- log(Twb.prof[,])
Twb.prof.type <-as.factor(ptype)
Tp.pca <-prcomp(log.Twb.prof, center=T, scale. =T)
print(Tp.pca)
plot(Tp.pca,type='l')
summary(Tp.pca)
predict(Tp.pca, newdata=tail(log.Twb.prof,2))
g <- ggbiplot(Tp.pca, obs.scale = 1, var.scale = 1,
              groups = Twb.prof.type, ellipse = TRUE,
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)


data(iris)
head(iris, 3)
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
# print method
print(ir.pca)

# plot method
plot(ir.pca, type = "l")
# summary method
summary(ir.pca)
# Predict PCs
predict(ir.pca, newdata=tail(log.ir, 2))

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1,
              groups = ir.species, ellipse = TRUE,
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)