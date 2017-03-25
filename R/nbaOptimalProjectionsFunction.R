#' Optimal NBA Projections(soFar)
#'
#' @param mMonth month #
#' @param mDay day #
#' @param mYear "2017"
#' @param labsFit Lab Regression
#' @param labs3k lab Regression for chumps
#' @param labsName name of normal regression
#' @param labsName3k name of chump regression
#' @param bbmFit bbm regression
#' @param bbm3k bbm regression for chumps
#' @param bbmName bbm regression name
#' @param combinedName nameofBoth
#' @param combined3k nameofBoth chump models
#' @param finalName name of entire shindig
#' @param labsCookie see above for labs
#'
#' @return df/csv of best NBA projections
#' @export
#'
#' @examples nbaOptimalProjections(mDay="29")
nbaOptimalProjections <- function(mMonth="1",mDay="27",mYear="2017",labsFit=nbaLM12,labs3k=all3000LM3FitStep,labsName="nbaFit12",labsName3k="all3000LM3FitStep",bbmFit=bbmLMNums1,bbm3k=bbmAll3000LM,bbmName="bbmLMNums1",combinedName="nbaLM12_AND_bbmLMNums1",combined3k="daily3000Projections",finalName="OptimalProjections",labsCookie=NULL){
modelDate<- paste0(mMonth,"_",mDay,"_",mYear)
nba <- nbaMonsterLabProjections(mMonth=mMonth,mDay=mDay,mYear=mYear,labsFit=labsFit,labsName=labsName,bbmFit=bbmFit,combinedName=combinedName,bbmCookie=bbmCookie)
nba3k <- nba3000Projections(mMonth=mMonth,mDay=mDay,mYear=mYear,labsModel=labs3k,labsName=labsName3k,BBMModel=bbm3k,combinedModel=combined3k)
nba <- nba %>% filter(Salary>4000)
nbaAll <- bind_rows(nba,nba3k)
write.csv(nbaAll,file=paste0("~/Desktop/NBA_Daily/",mMonth,"_",mDay,"_",mYear,finalName,".csv"))
return(nbaAll)
}
