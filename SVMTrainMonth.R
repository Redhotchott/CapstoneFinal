rm(list=ls())
# This is the rbf script creating svm's by month in the training period
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

conf.mat.create<-function(tt, test.rows.mon,zz.set){
  if(dim(tt)[1]==4 & dim(tt)[2]==4){
    zz.set<-zz.set+tt
  } else if (dim(tt)[1]==3 &dim(tt)[2]==3){
    if(any(ptype[test.rows.mon]=='IP')){
      if(any(rownames(tt)=='2')){
        zz.set[2:4,2:4]<-zz.set[2:4,2:4]+tt
      } else {
        zz.set[c(1,3:4),2:4]<-zz.set[c(1,3:4),2:4]+tt
      }
    } else {
      if(any(rownames(tt)=='2')){
        zz.set[2:4,c(1,3:4)]<-zz.set[2:4,c(1,3:4)]+tt
      } else {
        zz.set[c(1,3:4),c(1,3:4)]<-zz.set[c(1,3:4),c(1,3:4)]+tt
      }
    }
  } else if (dim(tt)[1]==3&dim(tt)[2]==2){
    if(any(rownames(tt)=='1')){
      zz.set[c(1,3:4),3:4]<-zz.set[c(1,3:4),3:4]+tt
    } else {
      zz.set[c(2:4),3:4]<-zz.set[c(2:4),3:4]+tt
    }
  } else if (dim(tt)[1]==4 & dim(tt)[2]==2){
    zz.set[1:4,3:4]<-zz.set[1:4,3:4]+tt
  } else if (dim(tt)[1]==4& dim(tt)[2]==3){
    if(any(ptype[test.rows.mon]=='IP')){
      zz.set[,2:4]<-zz.set[,2:4]+tt
    } else {
      zz.set[,c(1,3:4)]<-zz.set[,c(1,3:4)]+tt
    }
  }
  return(zz.set)
}

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
model.mon<-list()
res<-list()
res.mon<-list()

cl<-makeCluster(6)
registerDoSNOW(cl)
list2<-foreach (i =1:12) %dopar%{
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
  for (j in unique(all.months)){
    train.rows.mon=train.rows[(all.months[train.rows]==j)]
    test.rows.mon=test.rows[(all.months[train.rows]==j)]
  
    
    
    #in order F, I, R, S
    t.w<-create.wt(train.rows.mon)
    
    if(length(t.w)==3){model.mon[[j]]<- svm( ptype.df~., data=Twb.type[train.rows.mon,],
                                             probability=T, type='C-classification', 
                                             class.weights=c("1"=t.w[1], "2"=t.w[2], "3"=t.w[3]))
    }
    if(length(t.w)==4){model.mon[[j]]<- svm( ptype.df~., data=Twb.type[train.rows.mon,],
                                             probability=T, type='C-classification', 
                                             class.weights=c("1"=t.w[1], "2"=t.w[2], "3"=t.w[3],"4"=t.w[4]))}
    model<-model.mon[[j]]
    res.mon[[j]] <- predict( model, newdata=Twb.type[test.rows.mon,1:31])
    zz[,,i]<-zz[,,i]+table(pred = res.mon[[j]], true = Twb.type[test.rows,32])
  }
  model[[i]]<-model.mon
  res[[i]]<-res.mon
  zz[,,i]
  
  #implement the SVM
}
stopCluster(cl)
detach(Twb.type)

acc=0
tot=0
for( i in 1:12){
  acc=acc+sum(diag(list2[[i]]))
  tot=tot+sum(list2[[i]])
}
acc/tot
#V1 - weighted proportionally, gamma default (1/(datadim)), kernel=default (unknown),  cost default (1) 92.78%  --- 86.324%
#V2 - weighted proportionally, gamma default (1/(datadim)), kernel=sigmoid,  cost default (1) 65.6% 
#V2 - weighted proportionally, gamma default (1/(datadim)), kernel=poly  cost default (1) 65.6% 

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
    if(j==11&i!=9){
      zz[,,i]<-tt
    } else if (i==9&j==11){
      zz[,,i]<-matrix(0,nrow=4,ncol=4)
      zz[,,i]<-conf.mat.create(tt,test.rows.mon,zz[,,i])
      
    } else {
      zz[,,i]<-conf.mat.create(tt,test.rows.mon,zz[,,i])
    }
#     if(j==11){
#       zz[,,i]<-tt
#     } else if (dim(tt)[1]==3 & dim(tt)[2]==2){
#       zz[c(1,3:4),3:4,i]<-zz[c(1,3:4),3:4,i]+tt
#     } else if (dim(tt)[2]<4 & any(ptype[train.rows.mon]=='IP')){
#       zz[2:4,2:4,i]<-zz[2:4,2:4,i]+tt
#     } else if (dim(tt)[2]<4 & any(ptype[train.rows.mon]=='FZRA')){
#       zz[c(1,3:4),c(1,3:4),i]<-zz[c(1,3:4),c(1,3:4),i]+tt
#     } else {zz[,,i]<-zz[,,i]+tt}
    
    model[[i]]<-model.mon
    res[[i]]<-res.mon
    
    # print(tt)
    
    ##FUNCTIONIZE zz
  }
  print(zz[,,i])
}


######WHICH ARE MISSING STUFF
for(i in 1:12){
  train.years=1996:2000+i-1
  test.years=2000+i
  
  print(paste('Training Set: ', i))
  
  train.labels=head(which((years>=train.years[1] & months >8)),1):tail(which(years<=train.years[5]+1 & months <6),1)
  test.labels=which((years==test.years & months>8) | (years==test.years+1 & months < 6))
  
  
  train.rows=which(date.ind%in%train.labels)
  test.rows=which(date.ind%in%test.labels)
  
  train.nn[i]=length(train.rows)
  test.nn[i]=length(test.rows)
  for(j in unique(months)){
    print(paste('Month: ', j))
    train.rows.mon=train.rows[which(all.months[train.rows]==j)]
    test.rows.mon=test.rows[which(all.months[test.rows]==j)]
    
    if(length(unique(ptype[train.rows.mon]))<4|length(unique(ptype[test.rows.mon]))<4){
      print(paste('TRAIN: ', unique(ptype[train.rows.mon])))
      print(paste('TEST: ', unique(ptype[test.rows.mon])))
    }
  }
}

zz<-matrix(c(42, 10,432,  127,41,   14,  685,  367,9,    0, 5032,   20, 20,    7,  221, 2949), nrow=4,ncol=4, byrow=T)

