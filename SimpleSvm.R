library( 'e1071' )
library('rpart')
rm(list=ls())
load('Predictors.Rdata')


ptype.fac<-as.factor(ptype)

Twb.type<-cbind(Twb.prof,ptype.fac)

index <- 1:nrow(Twb.type)
testindex <- sample(index, trunc(length(index/3)))
testset<-Twb.type[testindex,]
trainset<-Twb.type[-testindex,]


model<- svm( ptype.fac~., data=trainset, cost=100, gamma=1)
res <- predict( model, newdata=test)








data( iris )
model <- svm( iris$Species~., iris )
res <- predict( model, newdata=iris )

library(e1071)
library(rpart)
data(Glass, package="mlbench")
## split data into a train and test set
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]


## svm
svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[,-10])
