#' nba Regression Correlations
#'
#' @return df indicating R^2 correlation value for each fit model
#' @export
#'
#' @examples nbaFitCorrs()
nbaFitCorrs <- function(){
nbaFit5_5Step_R2<-fit1<-summary(nbaFit5_5Step)$r.squared
nbaFit5_5_R2<-summary(nbaFit5_5)$r.squared
nYearGLM1_R2<-glmRSqaured(nYearGLM1)
nYearLM1AIC_R2<-summary(nYearLM1AIC)$r.squared
nba16_17AllStep_R2<-summary(nba16_17AllStep)$r.squared
nYearGLM2_R2<-glmRSqaured(nYearGLM2)
nbaDFDit1_R2<-summary(nbaFDFit1)$r.squared
nbaFDFit1Step_R2<-summary(nbaFDFit1Step)$r.squared
nbaFDFit8_R2<-summary(nbaFDFit8)$r.squared
nbaFDFit8Step_R2<-summary(nbaFDFit8Step)$r.squared
nYearGLM3_R2<-glmRSqaured(nYearGLM3)
nbaLM12_R2<-summary(nbaLM12)$r.squared
nbalmer10_R2<-summary(lmer10)$r.squared
glmfit4_R2<-glmRSquared(glmfit4)
bbmLMNums1_R2<-summary(bbmLMNums1)$r.squared

fits <- data.frame(c(nbaFit5_5Step_R2,nbaFit5_5_R2,nYearGLM1_R2,nYearLM1AIC_R2,nba16_17AllStep_R2,nYearGLM2_R2,nbaDFDit1_R2,nbaFDFit1Step_R2,nbaFDFit8_R2,nbaFDFit8Step_R2,nYearGLM3_R2,nbaLM12_R2,nbalmer10_R2,glmfit4_R2,bbmLMNums1_R2))
row.names(fits)<-list("nbaFit5_5Step_R2","nbaFit5_5_R2","nYearGLM1_R2","nYearLM1AIC_R2","nba16_17AllStep_R2","nYearGLM2_R2","nbaDFDit1_R2","nbaFDFit1Step_R2","nbaFDFit8_R2","nbaFDFit8Step_R2","nYearGLM3_R2","nbaLM12_R2","nbalmer10_R2","glmfit4_R2","bbmLMNums1_R2")
names(fits)<-"r2"
return(fits)
}
