#' NHL Projections Function
#'
#' @param modelDate
#'
#' @return saves .csv of projections for desired Daily games
#' @export
#'
#' @examples nhlProjections(modelDate)
nhlProjections <- function(modelDate="12_22_2016"){
  load(file="~/Desktop/nhl_Daily/nhlSkaterFit.rda")
  load(file="~/Desktop/nhl_Daily/nhlGoalieFit.rda")

  getNhlPlayerModel(modelDate=modelDate)

  nhlSkater <- readNHLCSVs(modelDate=modelDate,modelPosition="S")
  nhlGoalie <- readNHLCSVs(modelDate=modelDate,modelPosition="G")

  nhlSkater <- nhlSkater[nhlSkater$Properties.PositionId == 401,]
  nhlGoalie <- nhlGoalie[nhlGoalie$Properties.PositionId == 402,]

  nhlSkater <- na.zero(nhlSkater)
  nhlGoalie <- na.zero(nhlGoalie)

  nhlSkaterPredictions <- predict(nhlSkaterFit,nhlSkater)
  nhlSkaterActual_Predictions <- data.frame(nhlSkater$Properties.Player_Name,nhlSkaterPredictions,nhlSkater$ActualPoints,nhlSkater$Salary,nhlSkater$Salary/nhlSkaterPredictions,nhlSkater$Salary/nhlSkater$ActualPoints)
  names(nhlSkaterActual_Predictions) <- c("Name","Projection","Actual","Salary","Proj$/Pt.","Actual$/Pt.")
  write.csv(nhlSkaterActual_Predictions,file=paste0("~/Desktop/nhl_Daily/",modelDate,"_S_Projections.csv"))

  nhlGoaliePredictions <- predict(nhlGoalieFit,nhlGoalie)
  nhlGoalieActual_Predictions <- data.frame(nhlGoalie$Properties.Player_Name,nhlGoaliePredictions,nhlGoalie$ActualPoints,nhlGoalie$Salary,nhlGoalie$Salary/nhlGoaliePredictions,nhlGoalie$Salary/nhlGoalie$ActualPoints)
  names(nhlGoalieActual_Predictions) <- c("Name","Projection","Actual","Salary","Proj$/Pt.","Actual$/Pt.")
  write.csv(nhlGoalieActual_Predictions,file=paste0("~/Desktop/nhl_Daily/",modelDate,"_G_Projections.csv"))

  nhlG <- read.csv(file=paste0("~/Desktop/nhl_Daily/",modelDate,"_G_Projections.csv"),stringsAsFactors=FALSE)
  nhlS <- read.csv(file=paste0("~/Desktop/nhl_Daily/",modelDate,"_S_Projections.csv"),stringsAsFactors=FALSE)

  nhlAll <- bind_rows(nhlS,nhlG)
  write.csv(nhlAll,file=paste0("~/Desktop/nhl_Daily/",modelDate,"_All_Projections.csv"))

  return(nhlAll)
}
