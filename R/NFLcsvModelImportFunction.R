#' NFL CSV Model Import
#'
#' @param modelYear 
#' @param modelWeek 
#' @param PositionID 
#' @param mod 
#'
#' @return converted csv model to dataframe in current r session
#' @export
#'
#' @examples nflModelImportfromCSV(modelYear = "2015",modelWeek = "NULL",PositionID = "QB",mod = NULL)
nflModelImportfromCSV <- function(modelYear = "2015",modelWeek = "NULL",PositionID = "QB",mod = NULL){
  nflCsvmodelYearWeekmodelWeekPositionIDmod <- read.csv(file = paste0("~/Desktop/NFL_Daily/NFL_",modelYear,"_Week",modelWeek,"/NFL",modelYear,"Week",modelWeek,PositionID,mod,".csv"), stringsAsFactors = FALSE)
  return(nflCsvmodelYearWeekmodelWeekPositionIDmod)
}