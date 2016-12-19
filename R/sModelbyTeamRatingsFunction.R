#' SwisherModel groups
#'
#' @param sModel 
#'
#' @return model grouped
#' @export
#'
#' @examples sModelByTeamRatings(sModel = sModel)
sModelByTeamRatings <- function(sModel=sModel){
sModelRankAdd <- data.frame(sModel, stringsAsFactors = FALSE) %>% separate(stat.table.top.Opposing.Pitcher.Strength, into = c("firstInitial","lastName"),sep = ". ",remove = FALSE) %>% mutate(srank=1:nrow(sModel))
sModelbyTeam <- sModelRankAdd %>% group_by(lastName)
sModelTeamRanks <- sModelbyTeam %>% summarise(TeamRating = mean(srank)) %>% arrange(TeamRating)

return(sModelTeamRanks)
}
