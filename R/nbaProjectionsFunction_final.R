#' NBA Projections(final)
#'
#' @param modelDate "3_26_2017"
#' @param modelSite "DraftKings"
#' @param lmFit lmer10
#' @param lmModel "lmer10"
#' @param errorHandleProp NULL
#' @param errorHandleValue NULL
#' @param cookie labsC
#'
#' @return Daily NBA Projections
#' @export
#'
#' @examples NbaProjections()
NbaProjections<- function(modelDate="3_31_2017",modelSite="Draftkings",lmFit=nba16_17AllLm3Step,lmModel = "nba16_17AllLm3Step",errorHandleProp=NULL,errorHandleValue=NULL,cookie=labsC){
nba1 <- suppressWarnings(nbaLinRegProjections(modelDate=modelDate,lmFit=lmFit,lmModel=lmModel,cookie=cookie))
return(nba1)
}
