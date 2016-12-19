#' NFL Linear Model Fitting Function
#'
#' @param NFLModel 
#'
#' @return LM for NFL 
#' @export
#'
#' @examples ...
linearModelNFLSetup <- function(NFLModel = DST2015SeasonAllBinded){
NFLModel.cor <- cor(NFLModel[,sapply(NFLModel,is.numeric)])
NFLModel.cor <- na.zero(NFLModel.cor)
lincomb = caret::findLinearCombos(NFLModel.cor)
NFLModel.corNew <-lapply(lincomb$linearCombos, function(x) colnames(NFLModel.cor)[x])
NFLModel.corNew1 <- NFLModel.cor[,-lincomb$remove]
findCorr <- findCorrelation(NFLModel.corNew1)
findCorr2 <- lapply(findCorr, function(y) colnames(NFLModel.corNew1)[y])
findCorr3 <- NFLModel.corNew1[,-findCorr]
NewDF <- NFLModel[,colnames(findCorr3)]

dataResults <- NewDF[,"ActualPoints"]
datamodelPredictors <- NewDF[,-1]
datamodelPredictors$ActualPoints[]<-0
datamodelPredictorsLinCom <- findLinearCombos(datamodelPredictors)
datamodelPredictors1 <- datamodelPredictors[,-datamodelPredictorsLinCom$remove]

lm1 <- lm(dataResults~.,data = datamodelPredictors1)

return(lm1)
}
