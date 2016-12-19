

#' A filtering option for FantasyLab MLB models
#' 
#' Get specific stat for one player in a fantasylabs player model data frame.
#' 
#' @param Name Player's name
#' @param Model Name of data frame
#' @param Filter name of stat to find
#'
#' @return The player stat desired according to your model
#' @export
#'
#' @examples playerModelFilter("Yoenis Cespedes",model=mlbModel,filter="actual")
playerModelFilter <- function(name,model=h.7_23,filter="actual"){
  selectCol <- grep(filter, colnames(model), ignore.case = TRUE)
  selectRow <- which(grepl(name, model$Properties.Player_Name))
  filval <- model[selectRow,selectCol]
  return(filval)
}
  