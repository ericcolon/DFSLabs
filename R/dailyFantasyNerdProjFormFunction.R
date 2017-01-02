#' Daily FantasyNerd PRojections Format to csv for upload
#'
#' @param modelDate "1_1_2017"
#' @param projType name of Projections to be saved as...csv
#'
#' @return df of projections from desired model and day in DFNerd uplaid format
#' @export
#'
#' @examples dailyFantasyNerdProjForm(modelDate="12_1_2016",projType="Averaged")
dailyFantasyNerdProjForm <- function(modelDate="12_1_2016",projType = "Averaged"){
  projections <- read.csv(file=paste0("~/Desktop/NBA_Daily/",modelDate,projType,"_LinRegFitProjections.csv"))
  newForm <- data.frame(projections$Name,projections$Projection)
  names(newForm)<-c("Player","FP")
  write.csv(newForm,file=paste0("~/Desktop/NBA_Daily/DFNerdProjForm",modelDate,projType,".csv"))
  return(newForm)
}
