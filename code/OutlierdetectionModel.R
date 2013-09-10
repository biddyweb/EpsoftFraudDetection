############## import the data

#raw_data1 <- read.csv('data//高血压药品完整版3.txt',sep='|',header=T)
#write.csv(raw_data1,'data//高血压.csv')

raw_data <- read.table('data//门诊.txt',header=T,sep='|')
raw_data <- raw_data[,-1]
colnames(raw_data)
train_X <- raw_data[,c(3,4,5,6,8,9,11)]
train_X_numeric <- data.frame(lapply(train_X,as.integer))
train_Y1 <- raw_data$总费用
train_Y2 <- raw_data$药品总费用
train_Y3 <- raw_data$诊疗总费用

library(dummies)
tmp <- dummy(train_X$在职状态)
#######
library(DMwR)
tmp <- outlier.scores <- lofactor(train_X_numeric, k=5)########## Robust linear regression
RLMmodel1 <- rlm(x=train_X_numeric,y=train_Y1)
diff_pre_true4 <- RLMmodel1$residuals
### univariate outlier test
library(outliers)
tmp <- scores(train_X_numeric)

### TODO one-classification SVM
library(e1071)

SVMModel1 <- svm(x=train_X_numeric,y=train_Y1,type='one-classification')
tmp <- predict(SVMModel1,train_X_numeric)
#tmp <- ifelse(lapply(train_X,class) == 'factor',train_X,as.integer(train_X))


####### randomforest model predict 3 kind of expense
library(randomForest)
library(caret)
train_data <- cbind(train_X,train_Y1)
RFmodel1 <- randomForest(train_Y1~.,data=train_data,proximity=T)
diff_pre_true1 <- predict(RFmodel1,train_data)-train_data$train_Y1

train_data <- cbind(train_X,train_Y2)
RFmodel2 <- randomForest(train_Y2~.,data=train_data[which(!is.na(train_data$train_Y2)),],proximity=T)
diff_pre_true2 <- predict(RFmodel2,train_data[which(!is.na(train_data$train_Y2)),])-train_data[which(!is.na(train_data$train_Y2)),]$train_Y2
#diff_pre_true2 <- append(diff_pre_true2,0,which(is.na(train_data$train_Y2))[1]-1)
#diff_pre_true2 <- append(diff_pre_true2,0,which(is.na(train_data$train_Y2))[2]-1)



train_data <- cbind(train_X,train_Y3)
RFmodel3 <- randomForest(train_Y3~.,data=train_data[which(!is.na(train_data$train_Y3)),],proximity=T)
diff_pre_true3 <- predict(RFmodel3,train_data[which(!is.na(train_data$train_Y3)),])-train_data[which(!is.na(train_data$train_Y3)),]$train_Y3
#diff_pre_true3 <- append(diff_pre_true3,0,which(is.na(train_data$train_Y3))[1]-1)
#diff_pre_true3 <- append(diff_pre_true3,0,which(is.na(train_data$train_Y3))[2]-1)

rm(RFmodel1,RFmodel2,RFmodel3)