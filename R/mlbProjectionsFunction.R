#' mlb Linear Regression Model Projections Function
#'
#' @param modelDate "4_2_2017"
#' @param modelSite Draftkings or Fanduel
#' @param hitterFit mlbAllHittersLm1
#' @param pitcherFit mlbAllPitchersLm1
#' @param hName "mlbAllHittersLm1"
#' @param pName "mlbAllPitchersLm1"
#' @param errorHandleProp NULL
#' @param errorHandleValue NULL
#' @param cookie labsC
#'
#' @return mlb data frame of daily projections using a linear model to predict values
#' @export
#'
#' @examples mlbProjections(modelDate="4_2_2017",modelSite="Draftkings",hitterFit=mlbAllHittersLm1,pitcherFit=mlbAllPitchersLm1,hName = "mlbAllHittersLm1",pName = "mlbAllPitchersLm1",errorHandleProp=NULL,errorHandleValue=NULL,cookie=labsC)
mlbProjections <- function(modelDate="4_2_2017",modelSite="Draftkings",hitterFit=mlbHitterAdvancedLm1,pitcherFit=mlbPitcherAdvancedLm1,hName="mlbHitterAdvancedLm1",pName="mlbPitcherAdvancedLm1",errorHandleProp=NULL,errorHandleValue=NULL,cookie=mlbCookies){
  options(warn=-1)
  suppressWarnings(DFSLabs::getMlbModel(modelDate=modelDate,cookie=cookie))
  if(modelSite=="Draftkings"){
    #load(file ="~/Desktop/mlb_Daily/mlbFit4.rda")
    #load(file ="~/Desktop/mlb_Daily/mlbFit10noStep.rda")
    #load(file ="~/Desktop/mlb_Daily/mlbFit10Step.rda")
    #load(file ="~/Desktop/mlb_Daily/mlblmerFit1.rda")
    #load("~/Desktop/mlb_Daily/glmfit4.rda")
    #mlb_modelDateFD <- read.csv(paste0("~/Desktop/mlb_Daily/",modelDate,"_FD.csv"),stringsAsFactors=FALSE)
    #mlb_modelDateFD <- mlb_modelDateFD %>% arrange(Name)
    mlb_modelDateH <- read.csv(paste0("~/Desktop/MLB_Daily/MLB_",modelDate,"_DK_Hitters.csv"))
    mlb_modelDateP <- read.csv(paste0("~/Desktop/MLB_Daily/MLB_",modelDate,"_DK_Pitchers.csv"))

    suppressWarnings(mlb_modelDateH <- na.zero(mlb_modelDateH))
    suppressWarnings(mlb_modelDateP <- na.zero(mlb_modelDateP))

    #mlb_modelDateH$Properties.MyTrends <- 0
    #mlb_modelDateOwnP <- data.frame(FOO=c(mlb_modelDate$Projected_Ownership))
    #mlb_modelDateOwnP <- separate(data = mlb_modelDateOwnP,col = FOO, into = c("minOwn","maxOwn"),sep = "-")
    #if(!is.null(errorHandleProp)){
     # rowError <- which(mlb_modelDate$errorHandleProp == errorHandleValue)
     # mlb_modelDate <- mlb_modelDate[-rowError[1],]
    #}
    #IF error with type in mlbFullRun add new line below similar to line 24....
    #mlb_modelDate$Properties.p_own <- as.character(mlb_modelDate$Properties.p_own)
    #mlb_modelDate$Properties.B2B <- as.character(mlb_modelDate$Properties.B2B)

    #suppressWarnings(mlb_modelDate <- na.zero(mlb_modelDate))
    #mlb_modelDate <- mlb_modelDate %>% arrange(Properties.Player_Name)
    #mlb_modelDateP$Properties.OppSOPerABAvg <- 0


    mlb_modelDatePredH <- data.frame(predict(hitterFit, mlb_modelDateH))
    names(mlb_modelDateH) <- c("Projection")
    mlb_modelDatePredP <- data.frame(predict(pitcherFit, mlb_modelDateP))
    names(mlb_modelDatePredP) <- c("Projection")



######start using when Ownership projections start to sjow in the models...
    #mlb_modelDateOwnPH <- data.frame(FOO=c(mlb_modelDateH$Properties.p_own))
    #mlb_modelDateOwnPH <- tidyr::separate(data = mlb_modelDateOwnPH,col = FOO, into = c("minOwn","maxOwn"),sep = "-")
    #mlb_modelDateOwnPP <- data.frame(FOO=c(mlb_modelDateP$Properties.p_own))
    #mlb_modelDateOwnPP <- tidyr::separate(data = mlb_modelDateOwnPP,col = FOO, into = c("minOwn","maxOwn"),sep = "-")
    #mlb_modelDateNew <- data.frame(mlb_modelDate$TeamName,mlb_modelDate$Properties.Player_Name,mlb_modelDatePred,mlb_modelDate$Position,mlb_modelDate$Salary,mlb_modelDate$Salary/mlb_modelDatePred,mlb_modelDateOwnP$minOwn,mlb_modelDate$ActualPoints,mlb_modelDate$Salary/mlb_modelDate$ActualPoints)
    #mlb_modelDateNewH <- data.frame(mlb_modelDateH$TeamName,mlb_modelDateH$Properties.Player_Name,mlb_modelDatePredH$Projection,mlb_modelDateH$Position,mlb_modelDateH$Salary,mlb_modelDateH$Salary/mlb_modelDatePredH$Projection,mlb_modelDateOwnPH$minOwn,mlb_modelDateH$ActualPoints,mlb_modelDateH$Salary/mlb_modelDateH$ActualPoints,mlb_modelDateH$distance_r,mlb_modelDateH$distance_diff,mlb_modelDateH$DistPct15,mlb_modelDateH$ev_r,mlb_modelDateH$evd,mlb_modelDateH$ExitVelocityPct15,mlb_modelDateH$hh_r,mlb_modelDateH$hh_diff,mlb_modelDateH$HHPct15,mlb_modelDateH$airtime_r,mlb_modelDateH$airtime_l,mlb_modelDateH$airtime_r-mlb_modelDateH$airtime_l,mlb_modelDateH$fb_r,mlb_modelDateH$gb_r,mlb_modelDateH$GBPct_Slate,mlb_modelDateH$BattedBallLuck)
    #mlb_modelDateNewP <- data.frame(mlb_modelDateP$TeamName,mlb_modelDateP$Properties.Player_Name,mlb_modelDatePredP$Projection,mlb_modelDateP$Position,mlb_modelDateP$Salary,mlb_modelDateP$Salary/mlb_modelDatePredP$Projection,mlb_modelDateOwnPP$minOwn,mlb_modelDateP$ActualPoints,mlb_modelDateP$Salary/mlb_modelDateP$ActualPoints)


    mlb_modelDateNewH <- data.frame(mlb_modelDateH$TeamName,mlb_modelDateH$Properties.Player_Name,mlb_modelDatePredH$Projection,mlb_modelDateH$Position,mlb_modelDateH$Salary,mlb_modelDateH$Salary/mlb_modelDatePredH$Projection,mlb_modelDateH$ActualPoints,mlb_modelDateH$Salary/mlb_modelDateH$ActualPoints,mlb_modelDateH$distance_r,mlb_modelDateH$distance_diff,mlb_modelDateH$DistPct15,mlb_modelDateH$ev_r,mlb_modelDateH$evd,mlb_modelDateH$ExitVelocityPct15,mlb_modelDateH$hh_r,mlb_modelDateH$hh_diff,mlb_modelDateH$HHPct15,mlb_modelDateH$airtime_r,mlb_modelDateH$airtime_l,mlb_modelDateH$airtime_r-mlb_modelDateH$airtime_l,mlb_modelDateH$fb_r,mlb_modelDateH$gb_r,mlb_modelDateH$GBPct_Slate,mlb_modelDateH$BattedBallLuck)
    mlb_modelDateNewP <- data.frame(mlb_modelDateP$TeamName,mlb_modelDateP$Properties.Player_Name,mlb_modelDatePredP$Projection,mlb_modelDateP$Position,mlb_modelDateP$Salary,mlb_modelDateP$Salary/mlb_modelDatePredP$Projection,mlb_modelDateP$ActualPoints,mlb_modelDateP$Salary/mlb_modelDateP$ActualPoints)

    names(mlb_modelDateNewH) <- c("Team","Name","Projection","Position","Salary","$/Pt.","ActualPoints","Actual$/Pt.","dist_r","dist_Diff","DistPct15","ev_r","evd","EVPct15","hh_r","hh_diff","HHPct15","airtime_r","airtime_l","airtime_Diff","fb_r","gb_r","GBPct_Slate","BattedBallLuck")
    names(mlb_modelDateNewP) <- c("Team","Name","Projection","Position","Salary","$/Pt.","ActualPoints","Actual$/Pt.")

    mlb_modelDateNewH <- mlb_modelDateNewH %>% arrange(mlb_modelDateNewH[,6])
    write.csv(mlb_modelDateNewH,file=paste0("~/Desktop/MLB_Daily/MLB",modelDate,"_Hitter_Projections.csv"))
    mlb_modelDateNewP <- mlb_modelDateNewP %>% arrange(mlb_modelDateNewP[,6])
    write.csv(mlb_modelDateNewP,file=paste0("~/Desktop/MLB_Daily/MLB",modelDate,"_Pitcher_Projections.csv"))
    #mlb_Actual_Dollar_per <- data.frame(mlb_modelDateNew$Name,mlb_modelDateNew$`Actual$/Pt.`)
    #names(mlb_Actual_Dollar_per)<-c("Properties.Player_Name","Actual$/Pt.")
    #suppressMessages(mlb_modelDate_Actual <- full_join(mlb_modelDate,mlb_Actual_Dollar_per))
    #mlb_modelDate_Actual <- mlb_modelDate_Actual %>% arrange(`Actual$/Pt.`)
    #write.csv(mlb_modelDate_Actual,file=paste0("~/Desktop/MLB_Daily/",modelDate,lmModel,"LabsWithActualValue.csv"))
  }
  #if(modelSite=="FanDuel"){
    #load(file ="~/Desktop/mlb_Daily/mlbFit4.rda")
    #load(file ="~/Desktop/mlb_Daily/mlbFit10noStep.rda")
    #load(file ="~/Desktop/mlb_Daily/mlbFit10Step.rda")
    #load(file ="~/Desktop/mlb_Daily/mlblmerFit1.rda")
    #load("~/Desktop/mlb_Daily/glmfit4.rda")
    #mlb_modelDateFD <- read.csv(paste0("~/Desktop/mlb_Daily/",modelDate,"_FD.csv"),stringsAsFactors=FALSE)
    #mlb_modelDateFD <- mlb_modelDateFD %>% arrange(Name)
    #mlb_modelDate <- read.csv(paste0("~/Desktop/mlb_Daily/",modelDate,"FanDuel.csv"))
    #mlb_modelDate <- na.zero(mlb_modelDate)

    #mlb_modelDateOwnP <- data.frame(FOO=c(mlb_modelDate$Projected_Ownership))
    #mlb_modelDateOwnP <- separate(data = mlb_modelDateOwnP,col = FOO, into = c("minOwn","maxOwn"),sep = "-")

    #IF error with type in mlbFullRun add new line below similar to line 24....
    #mlb_modelDate$Properties.p_own <- as.character(mlb_modelDate$Properties.p_own)
    #mlb_modelDate$Properties.B2B <- as.character(mlb_modelDate$Properties.B2B)

    #suppressWarnings(mlb_modelDate <- na.zero(mlb_modelDate))
    #mlb_modelDate <- mlb_modelDate %>% arrange(Properties.Player_Name)
    #mlb_modelDatePred <- predict(lmFit, mlb_modelDate)
    #mlb_modelDateOwnP <- data.frame(FOO=c(mlb_modelDate$Properties.p_own))
    #mlb_modelDateOwnP <- tidyr::separate(data = mlb_modelDateOwnP,col = FOO, into = c("minOwn","maxOwn"),sep = "-")
    #mlb_modelDateNew <- data.frame(mlb_modelDate$TeamName,mlb_modelDate$Properties.Player_Name,mlb_modelDatePred,mlb_modelDate$Position,mlb_modelDate$Salary,mlb_modelDate$Salary/mlb_modelDatePred,mlb_modelDateOwnP$minOwn,mlb_modelDate$ActualPoints,mlb_modelDate$Salary/mlb_modelDate$ActualPoints)
    #mlb_modelDateNew <- data.frame(mlb_modelDate$TeamName,mlb_modelDate$Properties.Player_Name,mlb_modelDatePred,mlb_modelDate$Position,mlb_modelDate$Salary,mlb_modelDate$Salary/mlb_modelDatePred,mlb_modelDateOwnP$minOwn,mlb_modelDate$ActualPoints,mlb_modelDate$Salary/mlb_modelDate$ActualPoints,mlb_modelDate$Properties.OppPlusMinus)

    #names(mlb_modelDateNew) <- c("Team","Name","Projection","Position","Salary","$/Pt.","Ownership","ActualPoints","Actual$/Pt.","OppPlusMinus")
    #mlb_modelDateNew <- mlb_modelDateNew %>% arrange(mlb_modelDateNew[,6])
    #write.csv(mlb_modelDateNew,file=paste0("~/Desktop/mlb_Daily/",modelDate,lmModel,"FanDuel_LinRegFitProjections.csv"))
    #mlb_Actual_Dollar_per <- data.frame(mlb_modelDateNew$Name,mlb_modelDateNew$`Actual$/Pt.`)
    #names(mlb_Actual_Dollar_per)<-c("Properties.Player_Name","Actual$/Pt.")
    #suppressMessages(mlb_modelDate_Actual <- full_join(mlb_modelDate,mlb_Actual_Dollar_per))
    #mlb_modelDate_Actual <- mlb_modelDate_Actual %>% arrange(`Actual$/Pt.`)
    #write.csv(mlb_modelDate_Actual,file=paste0("~/Desktop/mlb_Daily/",modelDate,lmModel,"FanDuel_LabsWithActualValue.csv"))
  #}
  return(c(hName,pName))
}
