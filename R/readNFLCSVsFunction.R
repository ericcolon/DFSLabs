#' Read NFl csv saved Models
#'
#' @param modelYear "2016"
#' @param modelWeek 17
#' @param mod NULL
#' @param PositionID "QB"
#'
#' @return pulls model into r
#' @export
#'
#' @examples QBM <- readNFLCSVs(modelYear = "2015",modelWeek = NULL ,mod = "", PositionID = NULL)
readNFLCSVs <- function(modelYear = "2015",modelWeek = NULL ,modelId = NULL, PositionID = NULL){
csvFile <- read.csv(file = paste0("~/Desktop/NFL_Daily/NFL_",modelYear,"_Week",modelWeek,"/NFL",modelYear,"Week",modelWeek,PositionID,modelId,".csv"))
return(csvFile)
}
