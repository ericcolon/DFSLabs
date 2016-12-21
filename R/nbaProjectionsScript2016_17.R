#' NBA Full Run
#'
#' @param modelDate
#' @param noStep
#' @param step
#'
#' @return data frame for daily Projections
#' @export
#'
#' @examples nbaFullRun(modelDate = "12_1_2016",noStep = nbaFit5_5,step = nbaFit5_5Step)
nbaFullRun <- function(modelDate = "12_1_2016",noStep = nbaFit5_5,step = nbaFit5_5Step){
projectNBA()
nbaLinRegProjections(modelDate,lmFit = noStep,lmModel = "noStep")
nbaLinRegProjections(modelDate,lmFit = step,lmModel = "step")
noStepCSV <- read.csv(paste0("~/Desktop/NBA_Daily/",modelDate,"noStep","_LinRegFitProjections.csv"),stringsAsFactors = FALSE)
stepCSV <- read.csv(paste0("~/Desktop/NBA_Daily/",modelDate,"step","_LinRegFitProjections.csv"),stringsAsFactors = FALSE)
noStepCSV <- noStepCSV %>% arrange(Name)
stepCSV <- stepCSV %>% arrange(Name)
averaged <- ((noStepCSV$Projection + stepCSV$Projection)/2)
combinedDf <- data.frame(noStepCSV$Team,noStepCSV$Name,averaged,noStepCSV$Position,noStepCSV$Salary,noStepCSV$Salary/averaged,noStepCSV$Ownership,noStepCSV$ActualPoints,noStepCSV$Salary/noStepCSV$ActualPoints)
names(combinedDf) <- c("Team","Name","Projection","Position","Salary","$/Pt.","Ownership","ActualPoints","Actual$/Pt.")
combinedDf <- combinedDf %>% arrange(combinedDf[,6])
write.csv(combinedDf,file=paste0("~/Desktop/NBA_Daily/",modelDate,"Averaged","_LinRegFitProjections.csv"))
return(combinedDf)
}

