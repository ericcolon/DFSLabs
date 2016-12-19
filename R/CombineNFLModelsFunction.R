#' Bind NFL Models
#'
#' @param x 
#'
#' @return binded dataframe of all weeks in season
#' @export
#'
#' @examples combineNflModels(allModels)
combineNflModels <- function(x){
  xBinded <- bind_rows(x)
  numsDF <- xBinded[,sapply(xBinded,is.numeric)]
  numsDFnoNA <- na.zero(numsDF)
  numsDFnoNANamesMutates <- numsDFnoNA %>% mutate(player_Name = xBinded$Properties.Player_Name) %>% dplyr::arrange(desc(ActualPoints))
  return(numsDFnoNANamesMutates)
}