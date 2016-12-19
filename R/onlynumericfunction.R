#' Prepare Fantasy Labs MLB model for more analysis
#' 
#' Takes a player model data frame and returns only the numeric data for analysis with librarys such as corrplot
#' 
#' @param model Name of player model
#'
#' @return none
#' @export 
#'
#' @examples mlbNumericOnly(model=mlbModel)
mlbNumericOnly <- function(model= mlbModel){
  nums <- sapply(model, is.numeric)
  datareturn <- model[,nums]
  return(datareturn)
}