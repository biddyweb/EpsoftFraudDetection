library(arules)
StrNameJoin <- function(a){
    a <- as.character(a)
    str <- paste(unlist(lapply(strsplit(unlist(strsplit(substr(a,2,nchar(a)-1),split=';')),split=':'),function(x){x[1]})),collapse=',')
    size <- length(unlist(strsplit(substr(a,2,nchar(a)-1),split=';')))
    return(list(str,size))
}

GenerateStrsizematrixFromRawdata<-function(x){
    return (t(matrix(unlist(apply(matrix(x,ncol=1),1,StrNameJoin)),nrow=2)))
}

GenerateTransFromStrsizematrix <- function(x){
    write(x, file = "tmp_basket")
    trans <- read.transactions("tmp_basket",format = "basket", sep=",",rm.duplicates=T)
    trans@transactionInfo <- data.frame(transactionID=raw_data$就诊ID)
    if(file.exists('tmp_basket')) file.remove('tmp_basket')
    return (trans)
}

rawdata <- read.csv('data//糖尿病_sequence.csv')

xiyao <- generate_str_size_matrix(raw_data$西药清单)
zhongchengyao <- generate_str_size_matrix(raw_data$中成药清单)
zhongyao <- generate_str_size_matrix(raw_data$中药清单)
zhenliao <- generate_str_size_matrix(raw_data$诊疗)


zhenliao.trans <- GenerateTransFromStrsizematrix(zhenliao[,1])
tmp <- sort(itemFrequency(zhenliao.trans,type='absolute'),decreasing=T)
itemFrequencyPlot(zhenliao.trans,cex.names=0.6,topN=20,type='absolute')