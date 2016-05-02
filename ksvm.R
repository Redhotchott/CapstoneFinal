type='probabilities' # for the 
prob.model=T

library(kernlab)
load('predictors.Rdata')

Twb.red<-Twb.prof[1:100,]
ptype.red<-as.factor(ptype[1:100])


data(iris)

## Create a kernel function using the build in rbfdot function
rbf <- rbfdot(sigma=0.1)
rbf

## train a bound constraint support vector machine
irismodel <- ksvm(Species~.,data=iris,type="C-bsvc",
                  kernel=rbf,C=10,prob.model=TRUE)

ptypemodel <- ksvm(x=Twb.red,y=ptype.red,type="C-svc",
                   kernel=rbf,C=10,prob.model=TRUE)

# this is not working. 

irismodel

## get fitted values
fitted(irismodel)

## Test on the training set with probabilities as output
predict(irismodel, iris[,-5], type="probabilities")

