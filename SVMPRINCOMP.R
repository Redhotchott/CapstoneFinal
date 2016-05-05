rm(list=ls())
# this model combines SVM and Dim Reduction 
load('predictors.Rdata')

library(sn)
library(fields)
library(mvtnorm)
library(ggplot2)
library(caret)
library(plot3D)
library(e1071)

# Function doing the princ comp dim red of the observations
princ.comp<-function(p.rows){
  lister<-list()
  pc.rain<-princomp(Twb.prof[p.rows,],cor=T)
  S<-cov(Twb.prof[p.rows,])
  lin.combo<-eigen(S)$vectors[,1:4]
  new.dim<-Twb.prof[p.rows,]%*%lin.combo
  lister[[1]]<-new.dim
  lister[[2]]<-lin.combo
  return(lister)
}

my.BS.func<-function(prob.hats){
  classes=c('RA','SN', 'IP','FZRA')
  
  BS=0
  for(i in 1:4){
    matches=which(prob.hats[,5]==classes[i])
    o.ik=rep(0,length(prob.hats[,5]))
    o.ik[matches]=rep(1,length(matches))
    p.ik=prob.hats[,i]
    BS=BS+sum((p.ik-o.ik)^2,na.rm=T)
  }
  return(BS/length(prob.hats[,5]))
  
}

years=as.numeric(substr(dates,1,4))
months=as.numeric(substr(dates,5,6))
all.months=as.numeric(substr(dates[date.ind],5,6))

ptype.fac<-as.factor(ptype)
Twb.type<-cbind(Twb.prof,ptype.fac) %>% as.data.frame
colnames(Twb.type)<-c("H0","H1","H2", "H3","H4","H5","H6","H7", "H8","H9","H10","H11","H12","H13","H14","H15","H16","H17","H18","H19","H20","H21","H22","H23","H24","H25","H26","H27","H28","H29","H30","ptype.df")
Twb.type[,32]<-as.factor(Twb.type[,32])


#preallocatting used vectors
test.nn=array()
train.nn=array()
model<-list()
model.mon<-list()
res<-list()
res.mon<-list()
comp.true<-array()
comp.pred<-array()
comp.prob<-matrix(NA, nrow=1,ncol=4)

# Building a Prior Probability Diagram to Use in Future
prior.probs=array(0,dim=c(length(stations),length(unique(months)),4))
for(i in 1:length(stations)){
  print(i)	
  for(j in 1:length(unique(months))){
    mon=sort(unique(months))[j]
    #Finding the right stations	
    station.i=which(station.ind==i)
    #Finding the right months
    month.labels=which(months==mon)
    month.rows=which(date.ind%in%month.labels)
    #Getting the right stations AND months
    rows.needed=intersect(station.i,month.rows)
    
    rain.nn=length(which(ptype[rows.needed]=="RA"))
    snow.nn=length(which(ptype[rows.needed]=="SN"))
    pellet.nn=length(which(ptype[rows.needed]=="FZRA"))
    ice.nn=length(which(ptype[rows.needed]=="IP"))
    
    prior.probs[i,j,1:4]=c(rain.nn,snow.nn,pellet.nn,ice.nn)/length(rows.needed)		
  }
}


test.nn=array()
ALL.testing.rows=NULL
for(i in 1:12){
  test.years=2000+i
  test.labels=which(years==test.years & months>8 | (years==test.years+1 & months < 6))
  test.rows=which(date.ind%in%test.labels)
  test.nn[i]=length(test.rows)
  ALL.testing.rows=c(ALL.testing.rows,test.rows)
}

train.nn=array()
test.nn=array()

ind=0
i=1
j=11

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
  for (k in unique(all.months)){
    print(paste('Month: ', k))
    train.rows.mon=train.rows[which(all.months[train.rows]==k)]
    test.rows.mon=test.rows[which(all.months[test.rows]==k)]
    comp.true<-c(comp.true,Twb.type[test.rows.mon,32])
    
    new.dim<-princ.comp(train.rows.mon)
    
    #in order F, I, R, S

    new.data<-cbind(new.dim[[1]],Twb.type[train.rows.mon,32]) %>% as.data.frame
    new.data[,5]<-as.factor(new.data[,5])
    
    model.mon[[k]]<- best.tune(svm,V5~.,data=new.data,
                           probability=T, type='C-classification',  ranges = list(cost = 2^(0:4)))
    res.mon[[k]] <- predict( model.mon[[k]], newdata=as.matrix(Twb.type[test.rows.mon,1:31])%*%new.dim[[2]], probability=T)
    xx<-attr(res.mon[[k]], "probabilities") #pull out the probabilities
    if(dim(xx)[2]>3){
      comp.prob<-rbind(comp.prob, xx[,order(colnames(xx))])
    } else {
      yy<-matrix(0,nrow=dim(xx)[1],ncol=4)
      yy[,order(colnames(xx))]<-xx[,order(colnames(xx))]
      comp.prob<-rbind(comp.prob, yy)  
    }
    
    tt<-table(pred = res.mon[[k]], true = Twb.type[test.rows.mon,32])
    comp.pred<-c(comp.pred,res.mon[[k]])
  }
  model[[i]]<-model.mon
  res[[i]]<-res.mon
}



(conf.mat<-table(pred = comp.pred[-1], true = comp.true[-1]))
sum(diag(conf.mat))/sum(conf.mat)
comp.prob<-cbind(comp.prob[-1,],comp.true[-1])

(BS<-my.BS.func(comp.prob)) # 
1-BS/c(0.2200711,0.2223) #



