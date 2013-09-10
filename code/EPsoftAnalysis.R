#import the data
raw_data <- read.csv('data//高血压住院药品-完整版1.txt',header=T,sep='\t')
p <- ggplot(raw_data,aes(x))


head(raw_data)
colnames(raw_data)
raw_data$西药清单[1]
#static summary
library(ggplot2)
summary(raw_data)
pie <- ggplot(raw_data,aes(x=factor(住院天数)))+geom_bar(aes(fill=factor(身份类别)))+facet_wrap
pie

p <- ggplot(raw_data,aes(x=住院天数,y=住院总费用))
p+stat_density2d(geom = "polygon")

p <- p + theme_bw()+geom_point(color='red')+scale_y_reverse()+stat_smooth()
p