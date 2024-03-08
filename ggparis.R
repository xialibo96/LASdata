
library(GGally)
library(corrplot)
LASdata1<-LASdata[c(1:64),]
LASdata1<-LASdata1[,-4]
LASdata2<-LASdata[c(65:128),]
LASdata2<-LASdata2[,-4]
LASdata3<-LASdata[c(129:192),]
LASdata3<-LASdata3[,-4]


ggpairs(data = LASdata1, columns = c(11, 1:5))
ggpairs(data = LASdata1, columns = c(11, 6:10))

ggpairs(data = LASdata1, columns = c(12, 1:5))
ggpairs(data = LASdata1, columns = c(12, 6:10))

ggpairs(data = LASdata1, columns = c(13, 1:5))
ggpairs(data = LASdata1, columns = c(13, 6:10))





ggpairs(data = LASdata2, columns = c(11, 1:6))
ggpairs(data = LASdata2, columns = c(11, 7:10))




LASdata11<-LASdata1[,c(1:11)]
LASdata12<-LASdata1[,c(1:10,12)]
LASdata13<-LASdata1[,c(1:10,13)]
dataa<-cor(LASdata13)
corrplot(dataa, order = 'hclust', addrect = 2)

plot(LASdata$InfNH4,LASdata$EffR1LAS,type='l')


###########第二次的ggpairs   micro

library(GGally)
library(corrplot)
LASdata1<-LASdata[c(1:64),]
LASdata2<-LASdata[c(65:128),]
ggpairs(data = LASdata1, columns = c(10, 1:5))
ggpairs(data = LASdata1, columns = c(10, 6:9))

ggpairs(data = LASdata1, columns = c(12, 1:5))
ggpairs(data = LASdata1, columns = c(12, 6:10))

ggpairs(data = LASdata1, columns = c(13, 1:5))
ggpairs(data = LASdata1, columns = c(13, 6:10))





ggpairs(data = LASdata2, columns = c(11, 1:6))
ggpairs(data = LASdata2, columns = c(11, 7:10))




LASdata11<-LASdata1[,c(1:11)]
LASdata12<-LASdata1[,c(1:10,12)]
LASdata13<-LASdata1[,c(1:10,13)]
dataa<-cor(LASdata13)
corrplot(dataa, order = 'hclust', addrect = 2)

plot(LASdata$InfNH4,LASdata$EffR1LAS,type='l')
