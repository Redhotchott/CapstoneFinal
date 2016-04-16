rm(list=ls())

library( 'e1071' )
library('rpart')
library('dplyr')
library('parallel')
setwd('/Users/tchott/Documents/CapstoneFinal')
load('Predictors.Rdata')

ptype.fac<-as.factor(ptype)

Twb.type<-cbind(Twb.prof,ptype.fac) %>% as.data.frame
colnames(Twb.type)<-c("H0","H1","H2", "H3","H4","H5","H6","H7", "H8","H9","H10","H11","H12","H13","H14","H15","H16","H17","H18","H19","H20","H21","H22","H23","H24","H25","H26","H27","H28","H29","H30","ptype.df")
attach(Twb.type)

index <- 1:nrow(Twb.type)
set.seed(12)
indexes <- sample(index, 200)
testset <- Twb.type[indexes[1:100],]
trainset <- Twb.type[indexes[101:200],]
#class.weights
###TUNE! LOOK INTO IT> 
model<- svm( ptype.df~., data=trainset, probability=T, type='C-classification',class.weights=c("1"=1, "3"=2, "4"=8))
plot(model, trainset, H0~H1, slice=list(H3=270,H4=270), xlim=c(270,280),fill=T, ylim=c(270,280))

res <- predict( model, newdata=testset)
head(res)
table(pred = res, true = testset[,32])

tune.out <- tune(svm, ptype.df~., data = trainset, probabilty=T,ranges = list(gamma=c(0.1,0.5,1,2,4),kernel=c("radial", "linear", "sigmoid"),  cost = c(0.1,1,10,100,1000)), class.weights= c("0" = 1, "1" = 10))

unique(ptype.df)


tune.out <- tune(svm, RESPONSE~., data = train, kernel="radial", ranges = list(gamma=c(0.1,0.5,1,2,4), cost = c(0.1,1,10,100,1000)), class.weights= c("0" = 1, "1" = 10))
