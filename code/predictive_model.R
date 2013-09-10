# predictive model

library(randomForest)
train_data <- raw_data[,c(2,3,4,5,7,8,9,12,10)]


RFmodel <- randomForest(住院总费用~.,data=train_data,proximity=T)
RMSE(predict(RFmodel,train_data),train_data$住院总费用)
tmp <- sort((predict(RFmodel,train_data)-train_data$住院总费用),index.return=T)