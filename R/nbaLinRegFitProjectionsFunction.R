#' NBA Linear Regression Model Projections Function
#'
#' @param modelDate "12_27_2016"
#' @param lmFit lmer10
#' @param lmModel "lmer10"
#' @param modelSite Draftkings or Fanduel
#'
#' @return nba data frame of daily projections using a linear model to predict values
#' @export
#'
#' @examples nbaLinRegProjections(modelDate="11_15_2016",modelSite="Draftkings",lmFit=lmer10,lmModel = "lmer10")
nbaLinRegProjections <- function(modelDate="2_12_2017",modelSite="Draftkings",lmFit=lmer10,lmModel = "lmer10",errorHandleProp=NULL,errorHandleValue=NULL){
if(modelSite=="Draftkings"){
#load(file ="~/Desktop/NBA_Daily/nbaFit4.rda")
#load(file ="~/Desktop/NBA_Daily/nbaFit10noStep.rda")
#load(file ="~/Desktop/NBA_Daily/nbaFit10Step.rda")
#load(file ="~/Desktop/NBA_Daily/nbalmerFit1.rda")
#load("~/Desktop/NBA_Daily/glmfit4.rda")
#nba_modelDateFD <- read.csv(paste0("~/Desktop/NBA_Daily/",modelDate,"_FD.csv"),stringsAsFactors=FALSE)
#nba_modelDateFD <- nba_modelDateFD %>% arrange(Name)
nba_modelDate <- read.csv(paste0("~/Desktop/NBA_Daily/",modelDate,".csv"))
nba_modelDate <- na.zero(nba_modelDate)
#nba_modelDateOwnP <- data.frame(FOO=c(nba_modelDate$Projected_Ownership))
#nba_modelDateOwnP <- separate(data = nba_modelDateOwnP,col = FOO, into = c("minOwn","maxOwn"),sep = "-")
if(!is.null(errorHandleProp)){
  rowError <- which(nba_modelDate$errorHandleProp == errorHandleValue)
  nba_modelDate <- nba_modelDate[-rowError[1],]
  }
#IF error with type in nbaFullRun add new line below similar to line 24....
nba_modelDate$Properties.p_own <- as.character(nba_modelDate$Properties.p_own)
nba_modelDate$Properties.B2B <- as.character(nba_modelDate$Properties.B2B)

suppressWarnings(nba_modelDate <- na.zero(nba_modelDate))
#nba_modelDate <- nba_modelDate %>% arrange(Properties.Player_Name)
nba_modelDatePred <- predict(lmFit, nba_modelDate)
nba_modelDateOwnP <- data.frame(FOO=c(nba_modelDate$Properties.p_own))
nba_modelDateOwnP <- tidyr::separate(data = nba_modelDateOwnP,col = FOO, into = c("minOwn","maxOwn"),sep = "-")
#nba_modelDateNew <- data.frame(nba_modelDate$TeamName,nba_modelDate$Properties.Player_Name,nba_modelDatePred,nba_modelDate$Position,nba_modelDate$Salary,nba_modelDate$Salary/nba_modelDatePred,nba_modelDateOwnP$minOwn,nba_modelDate$ActualPoints,nba_modelDate$Salary/nba_modelDate$ActualPoints)
nba_modelDateNew <- data.frame(nba_modelDate$TeamName,nba_modelDate$Properties.Player_Name,nba_modelDatePred,nba_modelDate$Position,nba_modelDate$Salary,nba_modelDate$Salary/nba_modelDatePred,nba_modelDateOwnP$minOwn,nba_modelDate$ActualPoints,nba_modelDate$Salary/nba_modelDate$ActualPoints,nba_modelDate$Properties.OppPlusMinus)

names(nba_modelDateNew) <- c("Team","Name","Projection","Position","Salary","$/Pt.","Ownership","ActualPoints","Actual$/Pt.","OppPlusMinus")
nba_modelDateNew <- nba_modelDateNew %>% arrange(nba_modelDateNew[,6])
write.csv(nba_modelDateNew,file=paste0("~/Desktop/NBA_Daily/",modelDate,lmModel,"_LinRegFitProjections.csv"))
nba_Actual_Dollar_per <- data.frame(nba_modelDateNew$Name,nba_modelDateNew$`Actual$/Pt.`)
names(nba_Actual_Dollar_per)<-c("Properties.Player_Name","Actual$/Pt.")
suppressMessages(nba_modelDate_Actual <- full_join(nba_modelDate,nba_Actual_Dollar_per))
nba_modelDate_Actual <- nba_modelDate_Actual %>% arrange(`Actual$/Pt.`)
write.csv(nba_modelDate_Actual,file=paste0("~/Desktop/NBA_Daily/",modelDate,lmModel,"LabsWithActualValue.csv"))
}
if(modelSite=="FanDuel"){
    #load(file ="~/Desktop/NBA_Daily/nbaFit4.rda")
    #load(file ="~/Desktop/NBA_Daily/nbaFit10noStep.rda")
    #load(file ="~/Desktop/NBA_Daily/nbaFit10Step.rda")
    #load(file ="~/Desktop/NBA_Daily/nbalmerFit1.rda")
    #load("~/Desktop/NBA_Daily/glmfit4.rda")
    #nba_modelDateFD <- read.csv(paste0("~/Desktop/NBA_Daily/",modelDate,"_FD.csv"),stringsAsFactors=FALSE)
    #nba_modelDateFD <- nba_modelDateFD %>% arrange(Name)
    nba_modelDate <- read.csv(paste0("~/Desktop/NBA_Daily/",modelDate,"FanDuel.csv"))
    nba_modelDate <- na.zero(nba_modelDate)

    #nba_modelDateOwnP <- data.frame(FOO=c(nba_modelDate$Projected_Ownership))
    #nba_modelDateOwnP <- separate(data = nba_modelDateOwnP,col = FOO, into = c("minOwn","maxOwn"),sep = "-")

    #IF error with type in nbaFullRun add new line below similar to line 24....
    nba_modelDate$Properties.p_own <- as.character(nba_modelDate$Properties.p_own)
    nba_modelDate$Properties.B2B <- as.character(nba_modelDate$Properties.B2B)

    suppressWarnings(nba_modelDate <- na.zero(nba_modelDate))
    #nba_modelDate <- nba_modelDate %>% arrange(Properties.Player_Name)
    nba_modelDatePred <- predict(lmFit, nba_modelDate)
    nba_modelDateOwnP <- data.frame(FOO=c(nba_modelDate$Properties.p_own))
    nba_modelDateOwnP <- tidyr::separate(data = nba_modelDateOwnP,col = FOO, into = c("minOwn","maxOwn"),sep = "-")
    #nba_modelDateNew <- data.frame(nba_modelDate$TeamName,nba_modelDate$Properties.Player_Name,nba_modelDatePred,nba_modelDate$Position,nba_modelDate$Salary,nba_modelDate$Salary/nba_modelDatePred,nba_modelDateOwnP$minOwn,nba_modelDate$ActualPoints,nba_modelDate$Salary/nba_modelDate$ActualPoints)
    nba_modelDateNew <- data.frame(nba_modelDate$TeamName,nba_modelDate$Properties.Player_Name,nba_modelDatePred,nba_modelDate$Position,nba_modelDate$Salary,nba_modelDate$Salary/nba_modelDatePred,nba_modelDateOwnP$minOwn,nba_modelDate$ActualPoints,nba_modelDate$Salary/nba_modelDate$ActualPoints,nba_modelDate$Properties.OppPlusMinus)

    names(nba_modelDateNew) <- c("Team","Name","Projection","Position","Salary","$/Pt.","Ownership","ActualPoints","Actual$/Pt.","OppPlusMinus")
    nba_modelDateNew <- nba_modelDateNew %>% arrange(nba_modelDateNew[,6])
    write.csv(nba_modelDateNew,file=paste0("~/Desktop/NBA_Daily/",modelDate,lmModel,"FanDuel_LinRegFitProjections.csv"))
    nba_Actual_Dollar_per <- data.frame(nba_modelDateNew$Name,nba_modelDateNew$`Actual$/Pt.`)
    names(nba_Actual_Dollar_per)<-c("Properties.Player_Name","Actual$/Pt.")
    suppressMessages(nba_modelDate_Actual <- full_join(nba_modelDate,nba_Actual_Dollar_per))
    nba_modelDate_Actual <- nba_modelDate_Actual %>% arrange(`Actual$/Pt.`)
    write.csv(nba_modelDate_Actual,file=paste0("~/Desktop/NBA_Daily/",modelDate,lmModel,"FanDuel_LabsWithActualValue.csv"))
  }
return(nba_modelDateNew)
}
