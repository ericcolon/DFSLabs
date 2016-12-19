#' NBA Team Total Summary
#'
#' @param modelDate 
#'
#' @return data frame with relevant Team information for chosen game slate(s)
#' @export
#'
#' @examples nbaTeamSummary(modelDate = "11_30_2016")
nbaTeamSummary <- function(modelDate = "11_30_2016"){
nba_byTeam <- nbaFullRun(modelDate)
nba_byTeam <- nba_byTeam %>% group_by(Team)
nba_byTeam_Summary <- nba_byTeam %>% summarise(Active_Player_Exp = length(Team),total = sum(Projection),average = mean(Projection),actualTotal = sum(ActualPoints),actualAvg = mean(ActualPoints))
return(nba_byTeam_Summary)
}