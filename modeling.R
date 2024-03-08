####### ?????????LAS????????????
library(h2o)
Sys.setenv(JAVA_HOME='D:/Program Files/Java/jdk-18.0.1.1')
h2o.init()

library(caret)
library(readxl)

LASdata <- read_excel("data1.xlsx")
LASdata<-LASdata[,-10]
lmfit<-lm(LAS~.,data=LASdata)
summary(lmfit)
laspre<-predict(lmfit,data=LASdata)
plot(laspre,LASdata$LAS)
data.frame(laspre,LASdata$LAS)

LASdata <- read_excel("data.xlsx")

LASdata<-as.data.frame(LASdata)
set.seed(22222)

yy<-LASdata$LAS
hist(yy)

#####???????????????????????????

set.seed(22222)
inTrain <- createDataPartition(yy, p = 3/4, list = FALSE)
traindata <- LASdata[inTrain,]
testdata <- LASdata[-inTrain,]
hist(traindata$LAS)
hist(testdata$LAS)

preProcValues <- preProcess(traindata, method = c( "center","scale"))
testdata<-predict(preProcValues, testdata)
traindata <- predict(preProcValues, traindata)

train<- traindata
test<-testdata
train<-as.h2o(train)
test<-as.h2o(test)
# Identify predictors and response
y <- "LAS"
x <- setdiff(names(train), y)

aml <- h2o.automl(x = x, y = y,
                  training_frame = train,
                  max_models = 10,###?????????20
                  seed = 1)
aml
exa <- h2o.explain(aml, test)
exa
aml@leader
lasRF <- h2o.randomForest(x = x, y = y,
                          training_frame = train,
                          seed = 1)
lasdl<-h2o.deeplearning(x = x, y = y,
                        training_frame = train,
                        seed = 1)

lasglm<-h2o.glm(x = x, y = y,
                training_frame = train,
                seed = 1)
lasgbm<-h2o.gbm(x = x, y = y,
                training_frame = train,
                seed = 1)

lasgbm<-lasglm ### 0.8188  0.278 0.439           0.896  0.233 0.347
lasgbm<-lasRF #### 0.9856  0.064 0.120           0.973  0.117 0.196
lasgbm<-lasdl #### 0.9113 0.180  0.304           0.960  0.162 0.215
pred <- h2o.predict(lasgbm, train)
datap<-as.data.frame(pred)
plot(traindata$LAS,datap$predict)
R2(traindata$LAS,datap$predict)
MAE(traindata$LAS,datap$predict)
RMSE(traindata$LAS,datap$predict)
dataout<-data.frame(traindata$LAS,datap$predict)
data1<-dataout*stdlas+meanlas
write.csv(data1,'trainlas.csv')

pred <- h2o.predict(lasgbm, test)
datap<-as.data.frame(pred)
dataouttest<-data.frame(datap$predict,testdata$LAS)

R2(testdata$LAS,datap$predict)
MAE(testdata$LAS,datap$predict)
RMSE(testdata$LAS,datap$predict)

data2<-dataouttest*stdlas+meanlas
write.csv(data2,'testlas.csv')


######将因变量改为y
y<-traindata$EffR1LAS

traindata$y<-y
traindata<-traindata[,-12]

y<-testdata$EffR1LAS
testdata$y<-y
testdata<-testdata[,-12]
#########支持向量机  和  最近邻 


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10,
  search = 'random')

########支持向量机
#### svmLinear  svmPoly  svmRadial 

set.seed(111111)

svmrad<- train(y ~ ., data = traindata, 
               method = "svmRadial", 
               trControl = fitControl,
               verbose = FALSE,
               tuneLength=10)

svmrad


rpartlas<- train(y ~ ., data = traindata, 
                 method = "xgbTree", 
                 trControl = fitControl,
                 verbose = FALSE,
                 tuneLength=10)

rpartlas
# kmax, distance, kernel
### "rectangular" "triangular", "epanechnikov" , "biweight" 
###   "triweight" , "cos", "inv", "gaussian", "rank" and "optimal".


kknnGrid3 <- expand.grid(kernel = c("cos", "inv", "gaussian","rectangular" ,"epanechnikov","triangular"), 
                         kmax=c(1:10), 
                         distance=c(1:10))

kknn3<- train(y ~ ., data = traindata, 
              method = "kknn", 
              trControl = fitControl,
              verbose = FALSE,
              tuneGrid=kknnGrid3)
kknn3

rffit<-kknn3 ####       1  0  0    0.9771  0.0995  0.159 
rffit <- svmrad ### 0.9778  0.0922   0.149     0.984 0.105 0.145
rffit<-rpartlas
prefitdata <- predict(rffit,traindata[,-12])
plot(prefitdata,traindata$y)
R2(prefitdata,traindata$y)
MAE(prefitdata,traindata$y)
RMSE(prefitdata,traindata$y)

dataout<-data.frame(prefitdata,traindata$y)
data1<-dataout*stdlas+meanlas
write.csv(data1,'trainlas.csv')

prefitdata<-predict(rffit,testdata[,-12])
plot(prefitdata,testdata$y)
R2(prefitdata,testdata$y)
MAE(prefitdata,testdata$y)
RMSE(prefitdata,testdata$y)
dataout<-data.frame(prefitdata,testdata$y)
data1<-dataout*stdlas+meanlas
write.csv(data1,'testlas.csv')













