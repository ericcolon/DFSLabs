#' NBA Full Run
#'
#' @param modelDate "1_1_2017" daily games date
#' @param noStep nbaFit5_5 fit model
#' @param step lmer10 fit model
#'
#' @return data frame for daily Projections
#' @export
#'
#' @examples nbaFullRun(modelDate = "1_1_2017",noStep = nYearLM1AIC,step = nYearGLM1, model1name = "nYearLM1AIC", model2name = "nYearGLM2", fullModelName = "LatestProjFit")
nbaFullRun <- function(modelDate = "1_1_2017",noStep = nYearLM1AIC,step = nYearGLM2, model1name = "nYearLM1AIC", model2name = "nYearGLM2", fullModelName = "LatestProjFit"){
getNBAPlayerModel(modelDate=modelDate)

nbaLinRegProjections(modelDate,lmFit = noStep,lmModel = model1name)
nbaLinRegProjections(modelDate,lmFit = step,lmModel = model2name)
noStepCSV <- read.csv(paste0("~/Desktop/NBA_Daily/",modelDate,model1name,"_LinRegFitProjections.csv"),stringsAsFactors = FALSE)
stepCSV <- read.csv(paste0("~/Desktop/NBA_Daily/",modelDate,model2name,"_LinRegFitProjections.csv"),stringsAsFactors = FALSE)
noStepCSV <- noStepCSV %>% arrange(Name)
stepCSV <- stepCSV %>% arrange(Name)
averaged <- ((noStepCSV$Projection + stepCSV$Projection)/2)
combinedDf <- data.frame(noStepCSV$Team,noStepCSV$Name,averaged,noStepCSV$Position,noStepCSV$Salary,noStepCSV$Salary/averaged,noStepCSV$Ownership,noStepCSV$ActualPoints,noStepCSV$Salary/noStepCSV$ActualPoints)
names(combinedDf) <- c("Team","Name","Projection","Position","Salary","$/Pt.","Ownership","ActualPoints","Actual$/Pt.")
combinedDf <- combinedDf %>% arrange(combinedDf[,6])
write.csv(combinedDf,file=paste0("~/Desktop/NBA_Daily/",modelDate,fullModelName,"_LinRegFitProjections.csv"))
return(combinedDf)
}

