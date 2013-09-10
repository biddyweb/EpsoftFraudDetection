library(TraMinR)


raw_data <- read.csv('data//糖尿病_sequence.csv')
raw_data$医院编码 <- factor(raw_data$医院编码)

summary(factor(raw_data$医院编码))