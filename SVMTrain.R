rm(list=ls())

library( 'e1071' )
library('rpart')
library('dplyr')
library('parallel')
library(sn)
library(fields)
library(mvtnorm)
library(foreach)
library(doSNOW)
#setwd('/Users/tchott/Documents/CapstoneFinal')
load('Predictors.Rdata')

ptype.fac<-as.factor(ptype)

Twb.type<-cbind(Twb.prof,ptype.fac) %>% as.data.frame
colnames(Twb.type)<-c("H0","H1","H2", "H3","H4","H5","H6","H7", "H8","H9","H10","H11","H12","H13","H14","H15","H16","H17","H18","H19","H20","H21","H22","H23","H24","H25","H26","H27","H28","H29","H30","ptype.df")
attach(Twb.type)


years=as.numeric(substr(dates,1,4))
months=as.numeric(substr(dates,5,6))
all.months=as.numeric(substr(dates[date.ind],5,6))

test.nn=array()
train.nn=array()
truth<- array()
pred<- array()
zz<-array(NA, c(4,4,12))
model<-list()
res<-list()


cl<-makeCluster(6)
registerDoSNOW(cl)
list2<-foreach (i =1:1) %dopar%{
  library( 'e1071' )
  library('rpart')
  library('dplyr')
  library('parallel')
  library(sn)
  library(fields)
  library(mvtnorm)
  library(foreach)
  library(doSNOW)
  train.years=1996:2000+i-1
  test.years=2000+i
  
  print(i)
  
  train.labels=head(which((years>=train.years[1] & months >8)),1):tail(which(years<=train.years[5]+1 & months <6),1)
  test.labels=which((years==test.years & months>8) | (years==test.years+1 & months < 6))
  
  
  train.rows=which(date.ind%in%train.labels)
  test.rows=which(date.ind%in%test.labels)
  
  train.nn[i]=length(train.rows)
  test.nn[i]=length(test.rows)
  
  #######################################################
  ##Computing means and covariances for each precip type
  #######################################################
  rain.rows=which(ptype[train.rows]=="RA")
  snow.rows=which(ptype[train.rows]=="SN")
  pellet.rows=which(ptype[train.rows]=="IP")
  ice.rows=which(ptype[train.rows]=="FZRA")
  
  r.l<-length(rain.rows)
  s.l<-length(snow.rows)
  p.l<-length(pellet.rows)
  i.l<-length(ice.rows)
  #in order F, I, R, S
  t.w<-c(p.l/i.l,1,p.l/r.l, p.l/s.l)
  
  #implement the SVM
  model[[i]]<- svm( ptype.df~., data=Twb.type[train.rows,], probability=T, kernel='poly', type='C-classification', class.weights=c("1"=t.w[1], "2"=t.w[2], "3"=t.w[3], "4"=t.w[4]))
  res[[i]] <- predict( model[[i]], newdata=Twb.type[test.rows,1:31])
  zz[,,i]<-table(pred = res[[i]], true = Twb.type[test.rows,32])
}
stopCluster(cl)
save(list2, file='tables1-rep.RData')
detach(Twb.type)



cl<-makeCluster(6)
registerDoSNOW(cl)
for(i in 1:12){
  train.years=1996:2000+i-1
  test.years=2000+i
  
  print(i)
  
  train.labels=head(which((years>=train.years[1] & months >8)),1):tail(which(years<=train.years[5]+1 & months <6),1)
  test.labels=which((years==test.years & months>8) | (years==test.years+1 & months < 6))
  
  
  train.rows=which(date.ind%in%train.labels)
  test.rows=which(date.ind%in%test.labels)
  
  train.nn[i]=length(train.rows)
  test.nn[i]=length(test.rows)
  
  #######################################################
  ##Computing means and covariances for each precip type
  #######################################################
  rain.rows=which(ptype[train.rows]=="RA")
  snow.rows=which(ptype[train.rows]=="SN")
  pellet.rows=which(ptype[train.rows]=="IP")
  ice.rows=which(ptype[train.rows]=="FZRA")
  
  r.l<-length(rain.rows)
  s.l<-length(snow.rows)
  p.l<-length(pellet.rows)
  i.l<-length(ice.rows)
  #in order F, I, R, S
  t.w<-c(p.l/i.l,1,p.l/r.l, p.l/s.l)
  
  #implement the SVM
  model[[i]]<- svm( ptype.df~., data=Twb.type[train.rows,], probability=T, type='C-classification',kernel='sigmoid', class.weights=c("1"=t.w[1], "2"=t.w[2], "3"=t.w[3], "4"=t.w[4]))
  res[[i]] <- predict( model[[i]], newdata=Twb.type[test.rows,1:31])
  zz[,,i]<-table(pred = res[[i]], true = Twb.type[test.rows,32])
}
stopCluster(cl)
save(zz, file='tables1.RData')
save(model, file='model1.RData')
detach(Twb.type)

acc=0
tot=0
for( i in 1:1){
  acc=acc+sum(diag(list2[[i]]))
  tot=tot+sum(list2[[i]])
}
acc/tot
#V1 - weighted proportionally, gamma default (1/(datadim)), kernel=default (unknown),  cost default (1) 92.78%  --- 86.324%
#V2 - weighted proportionally, gamma default (1/(datadim)), kernel=sigmoid,  cost default (1) 65.6% 
#V2 - weighted proportionally, gamma default (1/(datadim)), kernel=poly  cost default (1) 65.6% 
