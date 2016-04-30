###SVM TAKE 2 ###
rm(list=ls())
# Training SVM Models
library(caret)
library(dplyr)         # Used by caret
library(kernlab)       # support vector machine 
library(pROC)	       # plot the ROC curves
 
load('predictors.RData')

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

ptype.fac<-as.factor(ptype)

Twb.type<-cbind(Twb.prof,ptype.fac) %>% as.data.frame
colnames(Twb.type)<-c("H0","H1","H2", "H3","H4","H5","H6","H7", "H8","H9","H10","H11","H12","H13","H14","H15","H16","H17","H18","H19","H20","H21","H22","H23","H24","H25","H26","H27","H28","H29","H30","ptype.df")
attach(Twb.type)

set.seed(1492)

years=as.numeric(substr(dates,1,4))
months=as.numeric(substr(dates,5,6))
all.months=as.numeric(substr(dates[date.ind],5,6))

test.nn=array()
train.nn=array()

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
    Twb.type.red<-Twb.type[train.rows.mon,]
    attach(Twb.type.red)
    ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
                         repeats=5,		    # do 5 repititions of cv
                         summaryFunction=multiClassSummary,	# Use AUC to pick the best model
                         classProbs=TRUE)
    
    svm.tune <- train(ptype.df~ . , data=Twb.type.red,
                      method = "svmRadial",   # Radial kernel
                      tuneLength = 3,					# 9 values of the cost function
                      preProc = c("center","scale"),  # Center and scale data #I CAN ADD PCA ON THIS
                      trControl=ctrl)
    
    detach(Twb.type.red)
  }
}




### Get the Data
# Load the data and construct indices to divide it into training and test data sets.
data(segmentationData)  	# Load the segmentation data set
trainIndex <- createDataPartition(segmentationData$Case,p=.5,list=FALSE)
trainData <- segmentationData[trainIndex,]
testData  <- segmentationData[-trainIndex,]
trainX <-trainData[,4:61]        # Pull out the variables for training
sapply(trainX,summary)           # Look at a summary of the training data


## SUPPORT VECTOR MACHINE MODEL
# First pass
set.seed(1492)
# Setup for cross validation
ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
                     repeats=5,		    # do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE)



#Train and Tune the SVM
svm.tune <- train(x=trainX,
                  y= trainData$Class,
                  method = "svmRadial",   # Radial kernel
                  tuneLength = 9,					# 9 values of the cost function
                  preProc = c("center","scale"),  # Center and scale data
                  metric="ROC",
                  trControl=ctrl)

svm.tune
# Support Vector Machines with Radial Basis Function Kernel 
# 
# 1010 samples
# 58 predictor
# 2 classes: 'PS', 'WS' 
# 
# Pre-processing: centered, scaled 
# Resampling: Cross-Validated (10 fold, repeated 5 times) 
# Summary of sample sizes: 908, 909, 909, 909, 909, 909, ... 
# Resampling results across tuning parameters:
#   
#   C      ROC        Sens       Spec       ROC SD      Sens SD     Spec SD   
#  0.25  0.8695054  0.8540559  0.6690476  0.03720951  0.04389913  0.07282584
#  0.50  0.8724147  0.8592774  0.6912857  0.03618794  0.04234003  0.07830249
#  1.00  0.8746137  0.8718648  0.6968254  0.03418700  0.04204607  0.06918850
#  2.00  0.8709825  0.8755478  0.6969048  0.03345607  0.03927223  0.06714838
#  4.00  0.8609396  0.8795478  0.6702063  0.03437846  0.04189803  0.06597494
#  8.00  0.8456799  0.8703357  0.6310635  0.03610988  0.04105803  0.07540066
# 16.00  0.8293339  0.8666667  0.5943492  0.03717344  0.04773906  0.08006023
# 32.00  0.8220839  0.8636131  0.5759365  0.03622665  0.04531028  0.07587914
# 64.00  0.8123889  0.8605315  0.5541746  0.03795353  0.04494173  0.07140262
# 
# Tuning parameter 'sigma' was held constant at a value of 0.01521335
# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were sigma = 0.01521335 and C = 1. 

# Second pass
# Look at the results of svm.tune and refine the parameter space

set.seed(1492)
# Use the expand.grid to specify the search space	
grid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25)
)

#Train and Tune the SVM
svm.tune <- train(x=trainX,
                  y= trainData$Class,
                  method = "svmRadial",
                  preProc = c("center","scale"),
                  metric="ROC",
                  tuneGrid = grid,
                  trControl=ctrl)

svm.tune
# Support Vector Machines with Radial Basis Function Kernel 
# 
# 1010 samples
# 58 predictor
# 2 classes: 'PS', 'WS' 
# 
# Pre-processing: centered, scaled 
# Resampling: Cross-Validated (10 fold, repeated 5 times) 
# Summary of sample sizes: 909, 909, 908, 910, 909, 909, ... 
# Resampling results across tuning parameters:
#   
#   sigma  C     ROC        Sens       Spec       ROC SD      Sens SD     Spec SD   
# 0.010  0.75  0.8727381  0.8614685  0.6817619  0.04096223  0.04183900  0.07664910
# 0.010  0.90  0.8742107  0.8633193  0.6878889  0.04066995  0.04037202  0.07817537
# 0.010  1.00  0.8748389  0.8630023  0.6873016  0.04079094  0.04061032  0.08189960
# 0.010  1.10  0.8747998  0.8642378  0.6884444  0.04076756  0.04004827  0.07892234
# 0.010  1.25  0.8749384  0.8657762  0.6923492  0.04083294  0.03911751  0.08070616
# 0.015  0.75  0.8726557  0.8660793  0.6923333  0.04171842  0.04324822  0.08203598
# 0.015  0.90  0.8727037  0.8688531  0.6945714  0.04164810  0.04082448  0.08379649
# 0.015  1.00  0.8727079  0.8713147  0.6906667  0.04184851  0.04273855  0.08174494
# 0.015  1.10  0.8724013  0.8719301  0.6895556  0.04197524  0.04108930  0.08377854
# 0.015  1.25  0.8721560  0.8722331  0.6900952  0.04207802  0.04096501  0.08639355
# 0.200  0.75  0.8193497  0.8863263  0.4478413  0.04695531  0.04072159  0.08061632
# 0.200  0.90  0.8195377  0.8903263  0.4405397  0.04688797  0.04091728  0.07844983
# 0.200  1.00  0.8193307  0.8915478  0.4361111  0.04719399  0.04004779  0.07815045
# 0.200  1.10  0.8195696  0.8958508  0.4333651  0.04694670  0.04026003  0.08252021
# 0.200  1.25  0.8198250  0.8983077  0.4271905  0.04705685  0.03900879  0.07945602
# 
# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were sigma = 0.01 and C = 1.25. 



