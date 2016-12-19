library(rio)

#' Export Fantasy Labs player model correlated data as csv
#' 
#' Calculates the correlation between all objects and variables in a player model and saves it as a csv to your working directory.
#' 
#' @param model Name of player model
#' @param y Name to save the file as
#'
#' @return The name of the csv filed saved to your computer.
#' @export
#'
#' @examples mlbCorrToCsv(model=mlbModel,y="MLB_date_correlated")
mlbCorrToCsv <- function(model=mlbModel,y="MLB_date_correlated"){
  makeCor <- cor(model)
  correlatedModelData <- export(makeCor,file = paste0(y,".csv"))
  return(correlatedModelData)
}