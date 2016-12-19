#' Full running MLB tool on a loop
#'
#' @param Ddate month and year with day being looped 
#' @param modelDateFlabs 
#' @param idFlabs 301 for hitters 302 for pitchers
#' @param gDateSwisher month mm format
#' @param urlDateCashLine month (3 letter format)
#'
#' @return model with swish ratings(hitters)
#' @export
#'
#' @examples foreach(i=1:30) %do% MLB_Data_Scripted_Function_Loop(Ddate=paste0("6_",i,"_2016"), modelDateFlabs = "6_12_2016",idFlabs ="301", gDateSwisher = "06", urlDateCashLine = "jun")
MLB_Data_Scripted_Function_Loop <- function(Ddate=paste0("6_",i,"_2016"), modelDateFlabs = "06",idFlabs ="301", gDateSwisher = "2016", urlDateCashLine = "jun"){
  
  lModelDdate <- getMlbPlayerModelLoop(modelDate = Ddate ,id = idFlabs)
  
  if(i < 10) {
    sModelDdate <- swishHittersLoop(gDate = paste0("2016-",modelDateFlabs,"-0",i))
  } else {
    sModelDdate <-swishHittersLoop(gDate = paste0("2016-",modelDateFlabs,"-",i))
  }
  swisherlyDdate <- swisherlyLabModelLoop(swishModel = sModelDdate, labsModel = lModelDdate)
  
  
  sModelTeamDdate <- sModelByTeamRatings(sModel=sModelDdate)
  lModelTeamDdate <- lModelTeams(lModel = lModelDdate)
  
  if(i < 10) {
    cashLineDdate <- getCashLineLoop(urlDate = paste0(urlDateCashLine,"-0",i,"-",gDateSwisher))
  } else {
    cashLineDdate <- getCashLineLoop(urlDate = paste0(urlDateCashLine,"-",i,"-",gDateSwisher))
  }
  
  
  write.csv(cashLineDdate, file=paste0("/users/Ben/Desktop/MLB_R_Data/cashLine",Ddate,".csv"))
  lModelTeamDdatechar <- sapply(lModelTeamDdate,as.character)
  write.csv(lModelTeamDdatechar , file=paste0("/users/Ben/Desktop/MLB_R_Data/lModelTeam",Ddate,"char.csv"))
  write.csv(sModelDdate , file=paste0("/users/Ben/Desktop/MLB_R_Data/sModel",Ddate,"char.csv"))
  write.csv(sModelTeamDdate , file=paste0("/users/Ben/Desktop/MLB_R_Data/sModelTeam",Ddate,".csv"))
  write.csv(swisherlyDdate , file=paste0("/users/Ben/Desktop/MLB_R_Data/swisherly",Ddate,".csv"))
  
  returnedModel <- inner_join(lModelDdate,swisherlyDdate)
  sapplyReturnedModel <- sapply(returnedModel,as.character)
  write.csv(sapplyReturnedModel , file = paste0("/users/Ben/Desktop/MLB_R_Data/combinedModel",Ddate,".csv"))
  return(returnedModel)
}