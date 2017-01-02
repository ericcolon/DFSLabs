#' NBA Full Run
#'
#' @param modelDate "1_1_2017" daily games date
#' @param noStep nbaFit5_5 fit model
#' @param step lmer10 fit model
#'
#' @return data frame for daily Projections
#' @export
#'
#' @examples nbaFullRun(modelDate = "12_1_2016",noStep = nbaFit5_5,step = nbaFit5_5Step, model1name = "nbaFit5_5", model2name = "lmer10", fullModelName = "Averaged")
nbaFullRun <- function(modelDate = "12_1_2016",noStep = nbaFit5_5,step = lmer10, model1name = "nbaFit5_5", model2name = "lmer10", fullModelName = "Averaged"){
projectNBA(modelDate)

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

