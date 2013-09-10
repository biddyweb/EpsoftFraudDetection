library(ggplot2)

weatherAUS <- read.csv('weatherAUS.csv')

cities <- c('Canberra','Darwin','Melbourne','Sydney')

g <- ggplot(subset(weatherAUS,Location %in% cities &! is.na(Temp3pm)),
            aes(Temp3pm,colour = Location, fill = Location))

g <- g + geom_density(alpha = 0.5)

print(g)

library(ggplot2)
p <- ggplot(data=mpg,mapping=aes(x=cty,y=hwy))
p + geom_point()

p <- ggplot(mpg,aes(x=hwy))
p+geom_histogram(aes(fill = factor(year)),y=density,alpha=1,colour='black')+stat_density(geom='line',position='identity',aes(color=factor(year)))+facet_wrap(~year,ncol=1)

p<-ggplot(mpg,aes(x=factor(1),fill=factor(class)))+geom_bar(width=1)
p+coord_polar(theta='y')
p<-ggplot(mpg,aes(class,hwy,fill=class))
p+geom_boxplot()
p+geom_violin(alpha=0.3,width=0,9)+geom_jitter(shape=21)