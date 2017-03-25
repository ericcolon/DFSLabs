#' NBA model Correlations Comparison Function
#'
#' @param modelDate "1_1_2017"
#' @param model1 nYearLM1AIC
#' @param model2 nYearGLM1
#' @param m1Name "nYearLM1AIC"
#' @param m2Name "nYearGLM1"
#' @param fullModel "nYearLM1AIC_and_nYearGLM1"
#'
#' @return df of each models actual Points correlation of projections 20:20
#' @export
#'
#' @examples nba1_1_2017models.Corr <- nbaCorrelations(modelDate="1_1_2017",model1=nYearLM1AIC,model2=nYearGLM1,m1Name="nYearLM1AIC",m2Name="nYearGLM1",fullModel="nYearLM1AIC_and_nYearGLM1")
nbaCorrelations<- function(modelDate="2_12_2017",model1=nYearLM1AIC,model2=nYearGLM1,m1Name="nYearLM1AIC",m2Name="nYearGLM1",fullModel="nYearLM1AIC_and_nYearGLM1"){
nbaFullRun(modelDate=modelDate, noStep=model1, step=model2, model1name=m1Name, model2name=m2Name, fullModelName=fullModel)
model1.cor <- read.csv(file=paste0("~/Desktop/NBA_Daily/",modelDate,m1Name,"_LinRegFitProjections.csv"))
model2.cor <- read.csv(file=paste0("~/Desktop/NBA_Daily/",modelDate,m2Name,"_LinRegFitProjections.csv"))
fullModel.cor <- read.csv(file=paste0("~/Desktop/NBA_Daily/",modelDate,fullModel,"_LinRegFitProjections.csv"))

model1.cor <- cor(numericOnly(model1.cor))
model2.cor <- cor(numericOnly(model2.cor))
fullModel.cor <- cor(numericOnly(fullModel.cor))

modelCorrs <- data.frame(model1.cor[2,6],model2.cor[2,6],fullModel.cor[2,6],modelDate)
names(modelCorrs)<-list(m1Name,m2Name,fullModel,"Date")
return(modelCorrs)

}
