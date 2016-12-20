#' Get NBA Labs Model for a model otber than 'Finding Value' Model
#'
#' @param modelDate
#' @param other
#' @param mName
#'
#' @return newPlayerModelof Desired choice
#' @export
#'
#' @examples getNbaPlayerModels_Other(modelDate="12_18_2016",other = "705700",mName = "Russell_and_Flow")
getNbaPlayerModels_Other <- function(modelDate="12_18_2016",other = "705700",mName = "Russell_and_Flow"){
  nbaNewModel <- getNBAPlayerModel(modelDate = modelDate ,switch = other)
  write.csv(nbaNewModel, file = paste0("~/Desktop/NBA_Daily/",modelDate,mName,".csv"))
  newCsvFile <- read.csv(file = paste0("~/Desktop/NBA_Daily/",modelDate,mName,".csv"), stringsAsFactors = FALSE)
  return(newCsvFile)
}