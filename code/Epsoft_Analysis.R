library(arules)
library(randomForest)

# extraction
str_name_join <- function(a){
  a <- as.character(a)
  str <- paste(unlist(lapply(strsplit(unlist(strsplit(substr(a,2,nchar(a)-1),split=';')),split=':'),function(x){x[1]})),collapse=',')
  size <- length(unlist(strsplit(substr(a,2,nchar(a)-1),split=';')))
  return(list(str,size))
}


# import the data
raw_data <- read.csv('data//肺感染住院信息-20130902.txt',header=T,sep='|')
raw_data$医院编码 <- factor(raw_data$医院编码)

# Case 1 Frequent Item Set Mining

#remove the 
tmp <- t(matrix(unlist(apply(matrix(raw_data$西药清单,ncol=1),1,str_name_join)),nrow=2))
pattern <- gsub('阿司匹林 |葡萄糖 |氯化钠 ','',tmp[,1])

write(pattern, file = "tmp_basket")
trans <- read.transactions("tmp_basket",format = "basket", sep=",",rm.duplicates=T)
trans@transactionInfo <- data.frame(transactionID=raw_data$就诊ID)

#build the model
closed <- eclat(trans, 
                  parameter = list(minlen=2,support=0.05,tidLists=TRUE,
                                   target = "closed frequent itemsets"))

FrequentMedicineSet <- subset(sort(closed,by = 'support'))
FrequentMedicineSetList <- as(FrequentMedicineSet@tidLists,'list')
result <- cbind(as(FrequentMedicineSet,'data.frame'),unlist(lapply(FrequentMedicineSetList,function(x){paste(x,collapse=' ')})))

# export to a file
#write(result,file='闭合频繁项集.csv',sep=',')



# Case 2 hierarchical cluster

# calculate the distance of each pair 
cluster <- dissimilarity(trans[sapply(train_list,function(x){length(x)})!=0],method = 'jaccard')

#cluster
result <- hclust(cluster,method='average')

#TODO : number of cluster @shiwei
groups <- cutree(result, k=10)


# Case 3 predictive model
train_data <- raw_data[,c(3,4,5,6,8,9,10,11,12,13)]

RFmodel <- randomForest(总费用~.,data=train_data,proximity=T)

# model plot
plot(RFmodel)
varImpPlot(RFmodel)

anomaly.score.all <- predict(RFmodel,train_data)-train_data$总费用


train_data <- raw_data[,c(3,4,5,6,8,9,10,11,12,14)]
RFmodel <- randomForest(药品总费用~.,data=train_data[which(!is.na(train_data$药品总费用)),],proximity=T)
anomaly.score.medicine <- predict(RFmodel,train_data[which(!is.na(train_data$药品总费用)),])-train_data[which(!is.na(train_data$药品总费用)),]$药品总费用
if(sum(is.na(train_data$药品总费用))!=0){
  for(i in 1:sum(is.na(train_data$药品总费用))){
    anomaly.score.medicine <- append(anomaly.score.medicine,0,which(is.na(train_data$药品总费用))[i]-1)
  }
}


train_data <- raw_data[,c(3,4,5,6,8,9,10,11,12,15)]
RFmodel <- randomForest(诊疗总费用~.,data=train_data[which(!is.na(train_data$诊疗总费用)),],proximity=T)
anomaly.score.diagnosis <- predict(RFmodel,train_data[which(!is.na(train_data$诊疗总费用)),])-train_data[which(!is.na(train_data$诊疗总费用)),]$诊疗总费用
if(sum(is.na(train_data$诊疗总费用))!=0){
  for(i in 1:sum(is.na(train_data$诊疗总费用))){
    anomaly.score.diagnosis <- append(anomaly.score.diagnosis,0,which(is.na(train_data$诊疗总费用))[i]-1)
  }
}
result <- cbind(anomaly.score.all,anomaly.score.diagnosis,anomaly.score.medicine)
result