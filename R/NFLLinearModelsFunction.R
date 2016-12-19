NFLPositionalLinearModelSetup <- function(model='RB2015SeasonAllBinded'){
model.cor <-  cor(model[,sapply(model,is.numeric)])

model.cor <- na.zero(model.cor)
lincomb = caret::findLinearCombos(model.cor)
model.corNew <-lapply(lincomb$linearCombos, function(x) colnames(model.cor)[x])
model.corNew1 <- model.cor[,-lincomb$remove]
findCorr <- findCorrelation(model.corNew1)
findCorr2 <- lapply(findCorr, function(y) colnames(model.corNew1)[y])
findCorr3 <- model.corNew1[,-findCorr]
NewDF <- model[,colnames(findCorr3)]

dataPartition <- NewDF$ActualPoints
datamodelPredictors <- NewDF[,-1]
datamodelPredictorsLinCom <- findLinearCombos(datamodelPredictors)
datamodelPredictors1 <- datamodelPredictors[,-datamodelPredictorsLinCom$remove]
dataResults <- NewDF[,"ActualPoints"]
lm1 <- lm(dataResults~.,data = datamodelPredictors1)
lm1 <- stepAIC(lm1)
return(lm1)
}

