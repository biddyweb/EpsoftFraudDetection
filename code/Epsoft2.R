raw_data <- read.csv('data//高血压住院药品-完整版1.txt',header=T,sep='\t')
colnames(raw_data)

library(arules)
library(ggplot2)
p1 <- ggplot(raw_data,aes(x=住院天数,y=住院总费用))
print(p1+geom_point(aes(color=性别))+opts(title='住院天数与住院总费用关系',plot.title=theme_text(face="bold",size=10,hjust=0.5,vjust=2,colour="black"))+stat_smooth())

p2 <- ggplot(raw_data,aes(x=年龄,y=住院总费用))
print(p2+geom_point(position = "jitter",aes(color=在职.退休))+opts(title='年龄与住院总费用关系',plot.title=theme_text(face="bold",size=10,hjust=0.5,vjust=2,colour="black"))+stat_smooth())

p3 <- ggplot(raw_data,aes(x=身份类别,y=住院总费用))
print(p3+geom_point(position = "jitter")+opts(title='身份类别与住院总费用关系',plot.title=theme_text(face="bold",size=10,hjust=0.5,vjust=2,colour="black")))

p3 <- ggplot(raw_data,aes(x=身份类别,y=住院天数))
print(p3+geom_point(position = "jitter")+opts(title='身份类别与住院天数',plot.title=theme_text(face="bold",size=10,hjust=0.5,vjust=2,colour="black")))



p4 <- ggplot(raw_data,aes(x=单位性质,y=住院天数))
print(p4+geom_point(position = "jitter")+opts(title='住院天数与住院总费用关系',plot.title=theme_text(face="bold",size=10,hjust=0.5,vjust=2,colour="black"))+stat_smooth())

p4 <- ggplot(raw_data,aes(x=单位性质,y=住院总费用))
print(p4+geom_point(position = "jitter")+opts(title='住院天数与住院总费用关系',plot.title=theme_text(face="bold",size=10,hjust=0.5,vjust=2,colour="black"))+stat_smooth())


p5 <- ggplot(raw_data,aes(x=单位经济类型,y=住院天数))
print(p5+ geom_point(position = "jitter")+opts(title='住院天数与住院总费用关系',plot.title=theme_text(face="bold",size=10,hjust=0.5,vjust=2,colour="black")))

p5 <- ggplot(raw_data,aes(x=单位经济类型,y=住院总费用))
print(p5+ geom_point(position = "jitter")+opts(title='住院天数与住院总费用关系',plot.title=theme_text(face="bold",size=10,hjust=0.5,vjust=2,colour="black")))


p6 <- ggplot(raw_data,aes(x=医院等级,y=住院总费用))
print(p6+geom_point(position = "jitter"))

p6 <- ggplot(raw_data,aes(x=医院等级,y=住院天数))
print(p6+geom_point(position = "jitter"))

))
drugs1 <- data.frame(raw_data$西药清单)
drugs2 <- data.frame(raw_data$中成药清单)
drugs3 <- data.frame(raw_data$中药清单)

tmp <- gsub('[,]阿司匹林|[,]葡萄糖|[,]氯化钠','',raw_data$西药清单)
drugs11 <- data.frame(tmp) 

train_list <- apply(drugs3,1,function(x){unlist(strsplit(x,','))})

names(train_list) <- paste("Tr",c(1:length(train_list)), sep = "")
trans <- as(train_list[1:935], "transactions")
trans
summary(trans)
itemFrequencyPlot(trans,cex.names=0.6,topN=50)

train_list1 <- as(trans,'list')
tmp <- sapply(train_list1,length)
tmp1 <-data.frame(tmp)
p <- ggplot(tmp1,aes(x=tmp))
p+geom_bar()+xlab('用药品种数量')+ylab('计数')+opts(title='用药品种数量直方图')

summary(tmp1)
quantile(tmp,0.95)
closed <- apriori(trans, 
                  parameter = list(minlen=2,support=0.05,
                      target = "closed frequent itemsets"))
a <- subset(sort(closed,by = 'support'))
write(a,file='闭合频繁项集.csv',sep=',',col.names=NA)

cluster <- dissimilarity(trans[sapply(train_list,function(x){length(x)})!=0],method = 'jaccard')
result <- hclust(cluster,method='average')
plot(result,main='记录药品层次化聚类')

#how to choose the number of cluster
y <- array(50)
for(i in 1:50){
    groups <- cutree(result, k=i)
    a <- sort(table(groups))
    y[i]  <- sum(a)-max(a)
}
cluster_result <- data.frame(x=1:50,y)

ggplot(cluster_result,aes(x=x,y=y))+geom_point()+stat_smooth()+xlab('聚类个数')+ylab('非最大聚簇点数目')


groups <- cutree(result, k=10)
sort(table(groups))
trans1 <- trans[sapply(train_list,function(x){length(x)})!=0]
tmp <- as(trans1[which((groups!=2)&(groups!=3)&(groups!=4))],'list')
a1 <- sapply(tmp,function(x){paste(x,collapse=',')})
write.table(a1,'1.csv')


# predictive model

library(gbm)
library(caret)
library(randomForest)
train_data <- raw_data[,c(2,3,4,5,7,8,9,12,10)]


a <- createFolds(train_data$住院总费用,5)
for (i in 1:5)
{
    train_set <- train_data[-a[[i]],]
    test_set <- train_data[a[[i]],]
    RFmodel <- randomForest(住院总费用~.,data=train_set,proximity=T)
    RMSE(predict(RFmodel,test_set),test_set$住院总费用)
}
sort(abs(predict(RFmodel,test_set)-test_set$住院总费用))

result <- data.frame(pre=predict(RFmodel,test_set),real=test_set$住院总费用)
ggplot(result,aes(x=pre,y=real))+geom_point()+xlab('预测费用')+ylab('实际费用')