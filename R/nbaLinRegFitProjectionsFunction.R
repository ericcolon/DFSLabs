#' NBA Linear Regression Model Projections Function
#'
#' @param modelDate 
#' @param lmFit 
#' @param lmModel 
#'
#' @return nba data frame of daily projections using a linear model to predict values
#' @export
#'
#' @examples nbaLinRegProjections(modelDate="11_15_2016",lmFit=nbaFit5_5,lmModel = "nbaFit5_5")
nbaLinRegProjections <- function(modelDate="11_15_2016",lmFit=nbaFit5_5,lmModel = "nbaFit5_5"){
#load(file ="~/Desktop/NBA_Daily/nbaFit4.rda")
#load("/Users/Ben/Desktop/nbaFit10noStep.rda")
#load("/Users/Ben/Desktop/nbaFit10Step.rda")
nba_modelDate <- read.csv(paste0("~/Desktop/NBA_Daily/",modelDate,".csv"),stringsAsFactors = FALSE)
nba_modelDate <- na.zero(nba_modelDate)
nba_modelDatePred <- predict(lmFit, nba_modelDate)
nba_modelDateOwnP <- data.frame(FOO=c(nba_modelDate$Properties.p_own))
nba_modelDateOwnP <- separate(data = nba_modelDateOwnP,col = FOO, into = c("minOwn","maxOwn"),sep = "-")
nba_modelDateNew <- data.frame(nba_modelDate$TeamName,nba_modelDate$Properties.Player_Name,nba_modelDatePred,nba_modelDate$Position,nba_modelDate$Salary,nba_modelDate$Salary/nba_modelDatePred,nba_modelDateOwnP$minOwn,nba_modelDate$ActualPoints,nba_modelDate$Salary/nba_modelDate$ActualPoints)
names(nba_modelDateNew) <- c("Team","Name","Projection","Position","Salary","$/Pt.","Ownership","ActualPoints","Actual$/Pt.")
nba_modelDateNew <- nba_modelDateNew %>% arrange(nba_modelDateNew[,6])
write.csv(nba_modelDateNew,file=paste0("~/Desktop/NBA_Daily/",modelDate,lmModel,"_LinRegFitProjections.csv"))
nba_Actual_Dollar_per <- data.frame(nba_modelDateNew$Name,nba_modelDateNew$`Actual$/Pt.`)
names(nba_Actual_Dollar_per)<-c("Properties.Player_Name","Actual$/Pt.")
nba_modelDate_Actual <- full_join(nba_modelDate,nba_Actual_Dollar_per)
nba_modelDate_Actual <- nba_modelDate_Actual %>% arrange(`Actual$/Pt.`)
write.csv(nba_modelDate_Actual,file=paste0("~/Desktop/NBA_Daily/",modelDate,lmModel,"LabsWithActualValue.csv"))
return(nba_modelDateNew)
}