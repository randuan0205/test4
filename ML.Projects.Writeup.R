library(caret)
library(parallel)
library(doParallel)
library(rpart)
library(ggplot2)
library(tree)
library(rattle)

# read csv files
rawdata<-read.csv("pml-training.csv",header=T)
finaltestdata<-read.csv("pml-testing.csv",header=T)
dim(rawdata)
dim(finaltestdata)

# pick up predictors
   # exclude variables with missing values and keep major predictors (belt,arm,dumbbell,forearm)
Missingdata <- sapply(rawdata, function (x) any(is.na(x) | x == ""))  
potentialpredictor<-!Missingdata & grepl("belt|[^(fore)]arm|dumbbell|forearm", names(Missingdata))
predictorlist <- names(Missingdata)[potentialpredictor]
filterdata<-rawdata[,c("classe", predictorlist)]
dim(filterdata)
finaltest<-finaltestdata[,predictorlist]
dim(finaltest)

# data preprocess 1 - split training and testing set
   #split the training data into 60% training and 40% testing, which is purposed for out-of-sample error estimate
set.seed (123)
intrain<-createDataPartition(filterdata$classe,p=0.6,list=FALSE)
datatraining<-filterdata[intrain,]
datatesting<-filterdata[-intrain,]

# data preprocess 2 - center&scaling
preobj<-preProcess(datatraining[,-1],method=c("center","scale"))
datatrainingS<-predict(preobj,datatraining[,-1])
datatrainingS<-data.frame(datatraining$classe,datatrainingS)
datatestingS<-predict(preobj,datatesting[,-1])
datatestingS<-data.frame(datatesting$classe,datatestingS)
finaltestS<-predict(preobj,finaltest)

# model training_1 - random forrest - model fit (allowing parallel to accelerate model building)
  ##model used 35 reps bootstrapp and tried 27 variables for each split decision
  ##Boostraping results shows OOB error rate is only 0.87%
c1 <- makeCluster(detectCores() - 1)
registerDoParallel(c1)
ctrl <- trainControl(classProbs=TRUE,
                     savePredictions=TRUE,
                     allowParallel=TRUE)
system.time(rffit <- train(datatraining.classe ~ ., data=datatrainingS, method="rf"))
stopCluster(c1)
rffit
trainpred <- predict(rffit, datatrainingS)
confusionMatrix(trainpred, datatrainingS[, "datatraining.classe"])


# model training_2 - decison tree 
##(results show 33.9% out of sample error rate)
dtfit<-tree(datatraining.classe ~ .,datatrainingS)
dtfitcv<-cv.tree(dtfit,FUN=prune.misclass)
dtfitcv
OOS_error<-(3993/11776)
OOS_error
##Visulatioin of dicision tree
dtfitprune<-prune.misclass(dtfit,best=17)
summary(dtfitprune)
plot(dtfitprune)
text(dtfitprune,pretty=0)

*Random forrest has the lower estimated out-of-sample error rate based on resampling methdology. So it will be applied to the test data set

# Apply random forests to 40% test data set to validate out-of-sample error rate
#random forests achieved 99% accuracy rate on testing data set, 
testpred <- predict(rffit, datatestingS)
confusionMatrix(testpred, datatestingS[, "datatesting.classe"])

#final random forrest model
rffit$finalModel
varImp(rffit)

#Apply to assigned test data
finaltestpred <- predict(rffit, finaltestS)
finaltestpred

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#results submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)

# model training_2 - decison tree (results show 32.7% training error rate)
dtfit <-train(datatraining.classe ~ ., data=datatrainingS, method="rpart")
dtfit <-train(datatraining.classe ~ ., data=datatrainingS, method="rpart",trControl=trainControl(method="boot"))

# model training_3 - Logistic Regression
lgfit <-train(datatraining.classe ~ ., data=datatrainingS, method="logreg")