###### data output
meanlas<-preProcValues[["mean"]][["LAS"]]
stdlas<-preProcValues[["std"]][["LAS"]]
HRTmean<-preProcValues[["mean"]][["HRT"]]
HRTstd<-preProcValues[["std"]][["HRT"]]
DOmean<-preProcValues[["mean"]][["DO"]]
DOstd<-preProcValues[["std"]][["DO"]]
densitymean<-preProcValues[["mean"]][["density"]]
densitystd<-preProcValues[["std"]][["density"]]
thicknessmean<-preProcValues[["mean"]][["thickness"]]
thicknessstd<-preProcValues[["std"]][["thickness"]]
SOCRmean<-preProcValues[["mean"]][["SOCR"]]
SOCRstd<-preProcValues[["std"]][["SOCR"]]
SOPRmean<-preProcValues[["mean"]][["SOPR"]]
SOPRstd<-preProcValues[["std"]][["SOPR"]]
bactermean<-preProcValues[["mean"]][["bacter"]]
bacterstd<-preProcValues[["std"]][["bacter"]]
micromean<-preProcValues[["mean"]][["micro"]]
microstd<-preProcValues[["std"]][["micro"]]

###### 进水LAS
meanLAS<-preProcValues[["mean"]][["InfLAS"]]
stdLAS<-preProcValues[["std"]][["InfLAS"]]


ANFISfit; bayfit; gamfit; gbmfit; kknn3;xgboostfit;
llmfit; nnetfit; rffit; svmrad; gbmfit;mlpKerasDropoutfit

svmrad1<-svmrad
svmrad<-mlpKerasDropoutfit
pred <- predict(svmrad, traindata)
datap<-as.data.frame(pred)
plot(traindata$y,datap$pred)
R2(traindata$y,datap$pred)
MAE(traindata$y,datap$pred)
RMSE(traindata$y,datap$pred)

dataout<-data.frame(traindata$y,datap$pred)
data1<-dataout*stdlas+meanlas
write.csv(data1,'trainlas.csv')

pred <- predict(svmrad, testdata)
datap<-as.data.frame(pred)
plot(testdata$y,datap$pred)
R2(testdata$y,datap$pred)
MAE(testdata$y,datap$pred)
RMSE(testdata$y,datap$pred)

dataout<-data.frame(testdata$y,datap$pred)
data2<-dataout*stdlas+meanlas
write.csv(data2,'testlas.csv')


datares<-exa[["residual_analysis"]][["plots"]][["GBM_5_AutoML_1_20231208_121359"]][["data"]]
datares$predict1<-datares$predict*stdlas+meanlas

write.csv(datares,'datares.csv')

learbcurve <- exa[["learning_curve"]][["plots"]][["GBM_5_AutoML_1_20231208_121359"]][["data"]]
write.csv(learbcurve,'learbcurve.csv')

vimp<-exa[["varimp"]][["plots"]][["GBM_5_AutoML_1_20231208_121359"]][["data"]]
write.csv(vimp,'vimp.csv')
vipheamp <- exa[["varimp_heatmap"]][["plots"]][[1]][["data"]]
write.csv(vipheamp,'vipheamp.csv')

shapdata<-exa[["shap_summary"]][["plots"]][["GBM_5_AutoML_1_20231208_121359"]][["data"]]
shapdata1<-shapdata[,c(2,3,5,7)]

write.csv(shapdata1,'shapdata.csv')

NO3<-exa[["pdp"]][["plots"]][["InfNO3"]][["data"]]
NO3data<-NO3[1:20,c(1,3)]
TN<-exa[["pdp"]][["plots"]][["InfTN"]][["data"]]
TNdata<-TN[1:20,c(1,3)]
COD<-exa[["pdp"]][["plots"]][["InfCOD"]][["data"]]
CODdata<-COD[1:20,c(1,3)]
las<-exa[["pdp"]][["plots"]][["InfLAS"]][["data"]]
lasdata<-las[1:20,c(1,3)]
ON<-exa[["pdp"]][["plots"]][["InfON"]][["data"]]
ONdata<-ON[1:20,c(1,3)]

pdpdata<-data.frame(NO3data,TNdata,CODdata,lasdata,ONdata)

pdpdata1<-pdpdata
pdpdata1$InfNO3<-pdpdata1$InfNO3*stdNO3+meanNO3
pdpdata1$InfTN<-pdpdata1$InfTN*stdTN+meanTN
pdpdata1$InfCOD<-pdpdata1$InfCOD*stdCOD+meanCOD
pdpdata1$InfLAS<-pdpdata1$InfLAS*stdLAS+meanLAS
pdpdata1$InfON<-pdpdata1$InfON*stdON+meanON
pdpdata1$values<-pdpdata1$values*stdlas+meanlas
pdpdata1$values.1<-pdpdata1$values.1*stdlas+meanlas
pdpdata1$values.2<-pdpdata1$values.2*stdlas+meanlas
pdpdata1$values.3<-pdpdata1$values.3*stdlas+meanlas
pdpdata1$values.4<-pdpdata1$values.4*stdlas+meanlas

write.csv(pdpdata1,'pdp.csv')

iceNO3<-exa[["ice"]][["plots"]][["InfNO3"]][["GBM_5_AutoML_1_20231208_121359"]][["data"]]
iceTN<-exa[["ice"]][["plots"]][["InfTN"]][["GBM_5_AutoML_1_20231208_121359"]][["data"]]
iceCOD<-exa[["ice"]][["plots"]][["InfCOD"]][["GBM_5_AutoML_1_20231208_121359"]][["data"]]
icelas<-exa[["ice"]][["plots"]][["InfLAS"]][["GBM_5_AutoML_1_20231208_121359"]][["data"]]
iceON<-exa[["ice"]][["plots"]][["InfON"]][["GBM_5_AutoML_1_20231208_121359"]][["data"]]

iceNO3data<-iceNO3[,1:3]
iceTNdata<-iceTN[,1:3]
iceCODdata<-iceCOD[,1:3]
icelasdata<-icelas[,1:3]
iceONdata<-iceON[,1:3]

iceNO3data1<-iceNO3data
iceTNdata1<-iceTNdata
iceCODdata1<-iceCODdata
icelasdata1<-icelasdata
iceONdata1<-iceONdata

iceNO3data1$InfNO3<-iceNO3data1$InfNO3*stdNO3+meanNO3
iceNO3data1$mean_response<-iceNO3data1$mean_response*stdlas+meanlas
iceTNdata1$InfTN<-iceTNdata1$InfTN*stdTN+meanTN
iceTNdata1$mean_response<-iceTNdata1$mean_response*stdlas+meanlas
iceCODdata1$InfCOD<-iceCODdata1$InfCOD*stdCOD+meanCOD
iceCODdata1$mean_response<-iceCODdata1$mean_response*stdlas+meanlas
icelasdata1$InfLAS<-icelasdata1$InfLAS*stdLAS+meanLAS
icelasdata1$mean_response<-icelasdata1$mean_response*stdlas+meanlas
iceONdata1$InfON<-iceONdata1$InfON*stdON+meanON
iceONdata1$mean_response<-iceONdata1$mean_response*stdlas+meanlas

write.csv(iceTNdata1,'icetn.csv')
write.csv(iceONdata1,'iceon.csv')
write.csv(iceNO3data1,'iceno3.csv')
write.csv(iceCODdata1,'icecod.csv')
write.csv(icelasdata1,'icenh4.csv')









