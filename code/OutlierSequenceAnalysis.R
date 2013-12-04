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
    trans@transactionInfo <- data.frame(transactionID=rawdata$就诊ID)
    if(file.exists('tmp_basket')) file.remove('tmp_basket')
    return (trans)
}

rawdata <- read.csv('data//糖尿病_sequence.csv')

xiyao <- GenerateStrsizematrixFromRawdata(rawdata$西药清单)
zhongchengyao <- GenerateStrsizematrixFromRawdata(rawdata$中成药清单)
zhongyao <- GenerateStrsizematrixFromRawdata(rawdata$中药清单)
zhenliao <- GenerateStrsizematrixFromRawdata(rawdata$诊疗)

yaopin.all <- cbind(xiyao,zhongyao,zhongchengyao,zhenliao)
zhenliao.trans <- GenerateTransFromStrsizematrix(zhenliao[,1])
tmp <- sort(itemFrequency(zhenliao.trans,type='absolute'),decreasing=T)

itemFrequencyPlot(zhenliao.trans,type='absolute')

