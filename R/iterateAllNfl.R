#' NFL Iterate All Function
#'
#' @param weeksFor 
#' @param forPosition 
#'
#' @return AllModelforPosition
#' @export
#'
#' @examples iterateAllNFL(weeksFor =NULL,forPosition = "QB")
iterateAllNFL <- function(weeksFor =NULL,forPosition = "QB"){
  AllModelforPosition <- foreach(m = 1:46) %do% getNflLabModel(modelYear = "2016", modelWeek = weeksFor, model = m, PositionID = forPosition)
  return(AllModelforPosition)
}