#' A function to extract specific players from your player model
#'
#' @param playerList includes the names of all the players you desire
#' @param model the model to pull data from
#'
#' @return subsetted model
#' @export
#'
#' @examples extractSpecific(playerList = playerNames, model = mlbModel)
 extractSpecific <- function(playerList, model = mlbModel){

  newModelLis <- t(cbind.data.frame(sapply(lapply(playerList,function(x)which(grepl(x, model$Properties.Player_Name,fixed = TRUE))),function(y)model[y,])))
  
  return(newModelLis)
 }
