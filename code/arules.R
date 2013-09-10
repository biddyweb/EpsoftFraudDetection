library(arules)
train <- read.csv('高血压住院药品.csv',colClasses='character')


train_list <- apply(train,1,function(x){unlist(strsplit(x,','))})

names(train_list) <- paste("Tr",c(1:length(train_list)), sep = "")
trans <- as(train_list[1:921], "transactions")

closed <- apriori(trans, 
                 parameter = list(
                                  target = "frequent itemsets"))
a <- subset(sort(closed,by = 'support'))
write(a,file='频繁项集不小于四.csv',sep=',',col.names=NA)


trans_tidlists <- as(trans,'tidLists')
item_trans_list <- as(trans_tidlists,'list')
trans_item_list <- as(trans,'list')
as(trans,'list')

cluster <- dissimilarity(trans,method = 'jaccard')
result <- hclust(cluster)
plot(result,main='记录药品层次化聚类')
groups <- cutree(result, k=20)
sort(table(groups))
clusterby <- by(as(trans,'matrix'),groups,mean)
by(as(trans,'matrix'),groups,mean)
# cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 

rect.hclust(result, k=50, border="red")