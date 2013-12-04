raw_data1 <- read.csv('data//糖尿病_sequence.csv')

totalfeeByperson <- tapply(raw_data1$总费用,raw_data1$人员编码,sum)
totalrecordByperson <- tapply(raw_data1$总费用,raw_data1$人员编码,length)

totalfeeByhospital <- tapply(raw_data1$总费用,raw_data1$医院编码,sum)
totalrecordByhospital <- tapply(raw_data1$总费用,raw_data1$医院编码,length)

library(ggplot2)
totalfeeByperson <- data.frame(fee = totalfeeByperson)
totalrecordByperson <- data.frame(record = totalrecordByperson)
m <- ggplot(totalrecordByperson, aes(x=record))
m + geom_histogram(binwidth = 0.5)