#' labs Model Grouping
#'
#' @param lModel 
#'
#' @return grouped model
#' @export
#'
#' @examples lModelTeams(lModel=lModel)
lModelTeams <- function(lModel=lModel){
lModelOP <- lModel %>% separate(Properties.Opposing_Team, into=(c("pitcherTeam","dash","firstInitial","lastName","throws")),sep = " ",remove = TRUE)
lModelOPTeam <- lModelOP  %>% group_by(lastName)
return(lModelOPTeam)
}