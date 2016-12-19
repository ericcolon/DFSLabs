#' The full flow
#'
#' @param swishModel 
#' @param labsModel 
#'
#' @return all
#' @export
#'
#' @examples swisherlyLabModel(SwishModel = sModel, labsModel = lModel)
swisherlyLabModel<-function(swishModel=sModel,labsModel=lModel){
rankRows<-dim(swishModel)[1]
  sModelWRank <- swishModel %>% mutate(sRank1=c(1:rankRows))
  fullModel<- inner_join(sModelWRank,labsModel)
  projectionStart <- dim(fullModel)[1]*2/10
  sProjection <- projectionStart-(fullModel$sRank1 * 0.1)
  fullModel_sProjections <- fullModel %>% mutate(swishProjection=sProjection)
  sProjectionxThree <- fullModel_sProjections$swishProjection * 3
  swishFinalProjection <- sProjectionxThree
  lModelScore <- fullModel_sProjections$Score
  fullModelProjectionsAverage <- (swishFinalProjection + lModelScore)/2
  fullModelEverything <- fullModel_sProjections %>% mutate(finalProjection=fullModelProjectionsAverage)
  fullModelEverything %>% arrange(desc(finalProjection))
  subsettedFinalProjections <- fullModelEverything %>% dplyr::select(player_Name, Properties.ActualPoints, finalProjection)
  return(subsettedFinalProjections)
}
