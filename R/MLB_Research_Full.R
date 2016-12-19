#' Full Run of MLB Tools 
#'
#' @param Date as M_d_YYYY 
#'
#' @return several csv files saved to computer
#' @export
#'
#' @examples MLB_Data_Scripted_Function(Date = "7_20_2016")
MLB_Data_Scripted_Function <- function(Date="7_20_2016"){

lModelDate <- getMlbPlayerModel()
sModelDate <-swishHitters()

swisherlyDate <- swisherlyLabModel(swishModel = sModelDate, labsModel = lModelDate)

sModelTeamDate <- sModelByTeamRatings(sModel=sModelDate)
lModelTeamDate <- lModelTeams(lModel = lModelDate)

cashLineDate <- getCashLine()

write.csv(cashLineDate, file=paste0("/users/Ben/Desktop/MLB_R_Data/cashLine",Date,".csv"))
lModelTeamDatechar <- sapply(lModelTeamDate,as.character)
write.csv(lModelTeamDatechar , file=paste0("/users/Ben/Desktop/MLB_R_Data/lModelTeam",Date,"char.csv"))
write.csv(sModelDate , file=paste0("/users/Ben/Desktop/MLB_R_Data/sModel",Date,"char.csv"))
write.csv(sModelTeamDate , file=paste0("/users/Ben/Desktop/MLB_R_Data/sModelTeam",Date,".csv"))
write.csv(swisherlyDate , file=paste0("/users/Ben/Desktop/MLB_R_Data/swisherly",Date,".csv"))
}