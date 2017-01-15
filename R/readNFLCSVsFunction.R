#' Read NFl csv saved Models
#'
#' @param modelYear "2016"
#' @param modelWeek 17
#' @param PositionID "QB"
#' @param modelSite Draftkings or Fanduel
#' @param modelId FantasyLabs modelID(Part of name of saved csv file to retrieve)
#'
#' @return pulls model into r
#' @export
#'
#' @examples QBM <- readNFLCSVs(modelYear = "2015",modelSite="Draftkings",modelWeek = NULL ,mod = "", PositionID = NULL)
readNFLCSVs <- function(modelYear = "2015",modelSite="Draftkings",modelWeek = NULL ,modelId = NULL, PositionID = NULL){
if(modelSite=="Draftkings"){
csvFile <- read.csv(file = paste0("~/Desktop/NFL_Daily/NFL_",modelYear,"_Week",modelWeek,"/NFL",modelYear,"Week",modelWeek,PositionID,modelId,".csv"))
}
if(modelSite=="FanDuel"){
csvFile <- read.csv(file = paste0("~/Desktop/NFL_Daily/NFL_",modelYear,"_Week",modelWeek,"/NFL",modelYear,"Week",modelWeek,PositionID,modelId,"FanDuel.csv"))
}
return(csvFile)
}
