rm(list=ls())
# Dimension Reduction. Broken down by month within each training period within each precipitation type
load('predictors.Rdata')
#Can't do it. i=1, k=11 -> only 7 obs of IP... Therefore, princomp spits out error as it can only be done with more units than variables.

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


cols=1:31
years=as.numeric(substr(dates,1,4))
months=as.numeric(substr(dates,5,6))
all.months=as.numeric(substr(dates[date.ind],5,6))

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




prob.hats=data.frame(matrix(0,nrow=sum(test.nn),ncol=5))
colnames(prob.hats)=c("prob.ra","prob.sn","prob.fzra","prob.ip","observed")

train.nn=array()
test.nn=array()


mean.train=list()
mean.train[[1]]=matrix(0,nrow=4,ncol=12)
mean.train[[2]]=matrix(0,nrow=4,ncol=12)
mean.train[[3]]=matrix(0,nrow=4,ncol=12)
mean.train[[4]]=matrix(0,nrow=4,ncol=12)
names(mean.train)=c("ra","sn","fzra","ip")

cov.train=list()
cov.train[[1]]=list()
cov.train[[2]]=list()
cov.train[[3]]=list()
cov.train[[4]]=list()
names(cov.train)=c("ra","sn","fzra","ip")

mean.train.mon<-list()
cov.train.mon<-list()
for(i in (unique(all.months))){
  mean.train.mon[[i]]<-mean.train
  cov.train.mon[[i]]<-cov.train
}

riq<-list() #order R S F I (Rows in question)
new.dim.mon<-list()

ind=0


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
  
  
  for ( k in unique(all.months)){
    print(paste('Month: ', k))
    train.rows.mon=train.rows[which(all.months[train.rows]==k)]
    test.rows.mon=test.rows[which(all.months[test.rows]==k)]
    
    #######################################################
    ##Computing means and covariances for each precip type
    #######################################################
    riq[[1]]<-which(ptype[train.rows.mon]=="RA")
    riq[[2]]<-which(ptype[train.rows.mon]=="SN")
    riq[[3]]<-which(ptype[train.rows.mon]=="FZRA")
    riq[[4]]<-which(ptype[train.rows.mon]=="IP")
    
  
    new.dim.mon[[k]]<-lapply(riq,princ.comp)
  
    mean.train.mon[[k]][[1]][,i]=apply(new.dim.mon[[k]][[1]][[1]],2,mean)
    mean.train.mon[[k]][[2]][,i]=apply(new.dim.mon[[k]][[2]][[1]],2,mean)
    mean.train.mon[[k]][[3]][,i]=apply(new.dim.mon[[k]][[3]][[1]],2,mean)
    mean.train.mon[[k]][[4]][,i]=apply(new.dim.mon[[k]][[4]][[1]],2,mean)  
  
    cov.train.mon[[k]][[1]][[i]]=cov(new.dim.mon[[k]][[1]][[1]])
    cov.train.mon[[k]][[2]][[i]]=cov(new.dim.mon[[k]][[2]][[1]])
    cov.train.mon[[k]][[3]][[i]]=cov(new.dim.mon[[k]][[3]][[1]])
    cov.train.mon[[k]][[4]][[i]]=cov(new.dim.mon[[k]][[4]][[1]])
  
  }
 
  
 
  
  
  
  #######################################################
  ##Computing probabilities of observations belonging to
  ##each of the 4 groups
  #######################################################
  
  for(j in 1:test.nn[i]){
    if(j%%1000==0){print(j)}
    ind=ind+1
    
    station.j=station.ind[test.rows[j]]
    mon.j=months[date.ind[test.rows[j]]]
    mon.col=which(sort(unique(months))==mon.j)
    
    #baseline
    pi.smk=prior.probs[station.j,mon.col,]
    pi.den.rain=pi.smk[1]*dmvnorm((Twb.prof[test.rows[j],cols]%*%new.dim.mon[[mon.j]][[1]][[2]]), mean.train.mon[[mon.j]][[1]][,i], cov.train.mon[[mon.j]][[1]][[i]])
    pi.den.snow=pi.smk[2]*dmvnorm((Twb.prof[test.rows[j],cols]%*%new.dim.mon[[mon.j]][[2]][[2]]), mean.train.mon[[mon.j]][[2]][,i], cov.train.mon[[mon.j]][[2]][[i]])
    pi.den.fzra=pi.smk[3]*dmvnorm((Twb.prof[test.rows[j],cols]%*%new.dim.mon[[mon.j]][[3]][[2]]), mean.train.mon[[mon.j]][[3]][,i], cov.train.mon[[mon.j]][[3]][[i]])
    pi.den.ip=pi.smk[4]*dmvnorm((Twb.prof[test.rows[j],cols]%*%new.dim.mon[[mon.j]][[4]][[2]]), mean.train.mon[[mon.j]][[4]][,i], cov.train.mon[[mon.j]][[4]][[i]])

    collection=c(pi.den.rain,pi.den.snow,pi.den.fzra,pi.den.ip)
    prob.hats[ind,1:4]=collection/sum(collection)
    prob.hats[ind,5]=ptype[test.rows[j]]
  }
}

pred<-apply(prob.hats[,1:4], 1, which.max)

observed=prob.hats[,5]
observed[which(prob.hats[,5]=="RA")]=rep(1,length(which(prob.hats[,5]=="RA")))
observed[which(prob.hats[,5]=="SN")]=rep(2,length(which(prob.hats[,5]=="SN")))
observed[which(prob.hats[,5]=="FZRA")]=rep(3,length(which(prob.hats[,5]=="FZRA")))
observed[which(prob.hats[,5]=="IP")]=rep(4,length(which(prob.hats[,5]=="IP")))
observed=as.numeric(observed)


(conf.mat<-table(pred = pred, true = observed))
sum(diag(conf.mat))/sum(conf.mat)


BS=0
(BS<-my.BS.func(prob.hats)) # 
1-BS/c(0.2200711,0.2223) #
