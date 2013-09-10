raw_data1 <- read.csv('data//高血压药品完整版3.txt',sep='|',header=T)
train_X <- raw_data1[,c(3,4,5,6,8,9,11,12)]
train_Y1 <- raw_data1$总费用
train_Y2 <- raw_data1$药品总费用
train_Y3 <- raw_data1$诊疗总费用


# predictive model

library(randomForest)
library(caret)
train_data <- cbind(train_X,train_Y1)
RFmodel1 <- randomForest(train_Y1~.,data=train_data,proximity=T)
diff_pre_true1 <- predict(RFmodel1,train_data)-train_data$train_Y1

train_data <- cbind(train_X,train_Y2)
RFmodel2 <- randomForest(train_Y2~.,data=train_data[which(!is.na(train_data$train_Y2)),],proximity=T)
diff_pre_true2 <- predict(RFmodel2,train_data[which(!is.na(train_data$train_Y2)),])-train_data[which(!is.na(train_data$train_Y2)),]$train_Y2
diff_pre_true2 <- append(diff_pre_true2,0,which(is.na(train_data$train_Y2))[1]-1)
diff_pre_true2 <- append(diff_pre_true2,0,which(is.na(train_data$train_Y2))[2]-1)



train_data <- cbind(train_X,train_Y3)
RFmodel3 <- randomForest(train_Y3~.,data=train_data[which(!is.na(train_data$train_Y3)),],proximity=T)
diff_pre_true3 <- predict(RFmodel3,train_data[which(!is.na(train_data$train_Y3)),])-train_data[which(!is.na(train_data$train_Y3)),]$train_Y3
diff_pre_true3 <- append(diff_pre_true3,0,which(is.na(train_data$train_Y3))[1]-1)
diff_pre_true3 <- append(diff_pre_true3,0,which(is.na(train_data$train_Y3))[2]-1)

write.csv(cbind(diff_pre_true1,diff_pre_true2,diff_pre_true3,raw_data1),file='result1.csv')
