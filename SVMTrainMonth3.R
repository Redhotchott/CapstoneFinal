rm(list=ls())
# SVM by Month. Radial Basis Function. Classes weighted evenly. 
# Overall Accuracy = 
library( 'e1071' )
library('rpart')
library('dplyr')
library('parallel')
library(sn)
library(fields)
library(mvtnorm)
library(foreach)
library(doSNOW)
load('Predictors.Rdata')


#FUNCTION 1 - this function creates a wts vector to use as the prior probs
create.wt<-function(train.rows.mon){
  rain.rows=which(ptype[train.rows.mon]=="RA")
  snow.rows=which(ptype[train.rows.mon]=="SN")
  pellet.rows=which(ptype[train.rows.mon]=="IP")
  ice.rows=which(ptype[train.rows.mon]=="FZRA")
  
  r.l<-length(rain.rows)
  s.l<-length(snow.rows)
  p.l<-length(pellet.rows)
  i.l<-length(ice.rows)
  p.lengths<-c(i.l,p.l,r.l,s.l)
  p.class<-p.lengths!=0
  
  ref<-which(p.lengths==min(p.lengths[p.lengths!=0]))
  class.wts<-p.lengths[ref]/p.lengths[p.class]
  return(class.wts)
}

#preformating the data to be accepted by the svm function. 
ptype.fac<-as.factor(ptype)
Twb.type<-cbind(Twb.prof,ptype.fac) %>% as.data.frame
colnames(Twb.type)<-c("H0","H1","H2", "H3","H4","H5","H6","H7", "H8","H9","H10","H11","H12","H13","H14","H15","H16","H17","H18","H19","H20","H21","H22","H23","H24","H25","H26","H27","H28","H29","H30","ptype.df")
Twb.type[,32]<-as.factor(Twb.type[,32])


years=as.numeric(substr(dates,1,4))
months=as.numeric(substr(dates,5,6))
all.months=as.numeric(substr(dates[date.ind],5,6))

#preallocatting used vectors
test.nn=array()
train.nn=array()
model<-list()
model.mon<-list()
res<-list()
res.mon<-list()
comp.true<-array()
comp.pred<-array()

for ( i in 1:12){
  train.years=1996:2000+i-1
  test.years=2000+i
  
  print(paste('Training Set: ', i))
  
  train.labels=head(which((years>=train.years[1] & months >8)),1):tail(which(years<=train.years[5]+1 & months <6),1)
  test.labels=which((years==test.years & months>8) | (years==test.years+1 & months < 6))
  
  
  train.rows=which(date.ind%in%train.labels)
  test.rows=which(date.ind%in%test.labels)
  
  train.nn[i]=length(train.rows)
  test.nn[i]=length(test.rows)
  
  #######################################################
  ##Computing means and covariances for each precip type
  #######################################################
  for (j in unique(all.months)){
    print(paste('Month: ', j))
    train.rows.mon=train.rows[which(all.months[train.rows]==j)]
    test.rows.mon=test.rows[which(all.months[test.rows]==j)]
    comp.true<-c(comp.true,Twb.type[test.rows.mon,32])
    
    
    #in order F, I, R, S
    t.w<-create.wt(train.rows.mon)
    
    if(length(t.w)==3){
      if(any(ptype[train.rows.mon]=='IP')){model.mon[[j]]<- svm( ptype.df~., data=Twb.type[train.rows.mon,],
                                                                 probability=T, type='C-classification', 
                                                                 class.weights=c("2"=t.w[1], "3"=t.w[2], "4"=t.w[3]))
      } else {model.mon[[j]]<- svm( ptype.df~., data=Twb.type[train.rows.mon,],
                                    probability=T, type='C-classification', 
                                    class.weights=c("1"=t.w[1], "3"=t.w[2], "4"=t.w[3]))}
    } else {model.mon[[j]]<- svm( ptype.df~., data=Twb.type[train.rows.mon,],
                                  probability=T, type='C-classification', 
                                  class.weights=c("1"=t.w[1], "2"=t.w[2], "3"=t.w[3],"4"=t.w[4]))
    }
    res.mon[[j]] <- predict( model.mon[[j]], newdata=Twb.type[test.rows.mon,1:31], decision.values = T)
    tt<-table(pred = res.mon[[j]], true = Twb.type[test.rows.mon,32])
    comp.pred<-c(comp.pred,res.mon[[j]])
  }
  model[[i]]<-model.mon
  res[[i]]<-res.mon
}
conf.mat<-table(pred = comp.pred[-1], true = comp.true[-1])
