#' NBA Full Run
#'
#' @param modelDate "1_1_2017" daily games date
#' @param noStep nbaFit5_5 fit model
#' @param step lmer10 fit model
#' @param modelSite Draftkings or FanDuel
#' @param model1name name of first Regression fitting model
#' @param model2name name of second Regression fitting model
#' @param fullModelName name of combined Regression fitting models
#'
#' @return data frame for daily Projections
#' @export
#'
#' @examples nbaFullRun(modelDate = "1_1_2017",modelSite="Draftkings",noStep = nYearLM1AIC,step = nYearGLM1, model1name = "nYearLM1AIC", model2name = "nYearGLM2", fullModelName = "LatestProjFit")
nbaFullRun <-   function(modelDate = "1_1_2017",modelSite="Draftkings",noStep = nba16_17AllStep,step = nYearGLM2, model1name = "nba16_17AllStep", model2name = "nYearGLM2", fullModelName = "FULL"){
if(modelSite=="Draftkings"){
getNBAPlayerModel(modelDate=modelDate,modelSite=modelSite)
options(warn=-1)
nbaLinRegProjections(modelDate,modelSite=modelSite,lmFit = noStep,lmModel = model1name)
nbaLinRegProjections(modelDate,modelSite=modelSite,lmFit = step,lmModel = model2name)
noStepCSV <- read.csv(paste0("~/Desktop/NBA_Daily/",modelDate,model1name,"_LinRegFitProjections.csv"),stringsAsFactors = FALSE)
stepCSV <- read.csv(paste0("~/Desktop/NBA_Daily/",modelDate,model2name,"_LinRegFitProjections.csv"),stringsAsFactors = FALSE)
noStepCSV <- noStepCSV %>% arrange(Name)
stepCSV <- stepCSV %>% arrange(Name)
averaged <- ((noStepCSV$Projection + stepCSV$Projection)/2)
combinedDf <- data.frame(noStepCSV$Team,noStepCSV$Name,averaged,noStepCSV$Position,noStepCSV$Salary,noStepCSV$Salary/averaged,noStepCSV$Ownership,noStepCSV$ActualPoints,noStepCSV$Salary/noStepCSV$ActualPoints,noStepCSV$OppPlusMinus)
names(combinedDf) <- c("Team","Name","Projection","Position","Salary","$/Pt.","Ownership","ActualPoints","Actual$/Pt.","Opp.+/-")
combinedDf <- combinedDf %>% arrange(combinedDf[,6])
write.csv(combinedDf,file=paste0("~/Desktop/NBA_Daily/",modelDate,fullModelName,"_LinRegFitProjections.csv"))
}
if(modelSite=="FanDuel"){
    getNBAPlayerModel(modelDate=modelDate,modelSite=modelSite)
    options(warn=-1)
    nbaLinRegProjections(modelDate,modelSite=modelSite,lmFit = noStep,lmModel = model1name)
    nbaLinRegProjections(modelDate,modelSite=modelSite,lmFit = step,lmModel = model2name)
    noStepCSV <- read.csv(paste0("~/Desktop/NBA_Daily/",modelDate,model1name,"FanDuel_LinRegFitProjections.csv"),stringsAsFactors = FALSE)
    stepCSV <- read.csv(paste0("~/Desktop/NBA_Daily/",modelDate,model2name,"FanDuel_LinRegFitProjections.csv"),stringsAsFactors = FALSE)
    noStepCSV <- noStepCSV %>% arrange(Name)
    stepCSV <- stepCSV %>% arrange(Name)
    averaged <- ((noStepCSV$Projection + stepCSV$Projection)/2)
    combinedDf <- data.frame(noStepCSV$Team,noStepCSV$Name,averaged,noStepCSV$Position,noStepCSV$Salary,noStepCSV$Salary/averaged,noStepCSV$Ownership,noStepCSV$ActualPoints,noStepCSV$Salary/noStepCSV$ActualPoints,noStepCSV$OppPlusMinus)
    names(combinedDf) <- c("Team","Name","Projection","Position","Salary","$/Pt.","Ownership","ActualPoints","Actual$/Pt.","Opp.+/-")
    combinedDf <- combinedDf %>% arrange(combinedDf[,6])
    write.csv(combinedDf,file=paste0("~/Desktop/NBA_Daily/",modelDate,fullModelName,"FanDuel_LinRegFitProjections.csv"))
  }
return(combinedDf)
options(warn=0)
}

