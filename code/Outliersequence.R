# transform to the proper data format
#tmp <- order(raw_data$人员编码,partial=raw_data$就诊日期)
#raw_data1 <-raw_data[tmp,]
#write.csv(raw_data1,'糖尿病_sequence.csv')
library(arulesSequences)
raw_data1 <- read.csv('data//糖尿病_sequence.csv')
str_name_join <- function(a){
    a <- as.character(a)
    str <- paste(unlist(lapply(strsplit(unlist(strsplit(substr(a,2,nchar(a)-1),split=';')),split=':'),function(x){x[1]})),collapse=' ')
    size <- length(unlist(strsplit(substr(a,2,nchar(a)-1),split=';')))
    return(list(str,size))
}



tmp4 <- t(matrix(unlist(apply(matrix(raw_data1$诊疗,ncol=1),1,str_name_join)),nrow=2))
arulesequence <- data.frame(sequenceID = raw_data1$人员编码,eventID=raw_data1$就诊日期,size=as.integer(as.character(tmp4[,2])),itemset=tmp4[,1])
arulesequence1<-arulesequence[which(arulesequence$size!=0),]
write.table(arulesequence1,'test4.txt',row.names = F,quote=F,col.names=F)


# need to cut this data file to package file
x4 <- read_baskets(con  = system.file("misc", "test4.txt", package = 
                                         "arulesSequences"),
                  info = c("sequenceID","eventID","SIZE"))

s1 <- cspade(x4, parameter = list(support = 0.05))

a <- subset(sort(s1 ,by= 'support'))
write(a,file='频繁序列项集诊疗.csv',sep=',',col.names=NA)
