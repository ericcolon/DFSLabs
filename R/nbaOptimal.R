#' NBA Optimal Projections Function
#'
#' @param mMonth 1
#' @param mDay 30
#' @param mYear 2017
#' @param labsFit m1
#' @param labs3k m2
#' @param labsName 1
#' @param labsName3k 2
#' @param bbmFit 3
#' @param bbm3k 4
#' @param bbmName 5
#' @param combinedName combo
#' @param combined3k combo2
#' @param finalName final
#' @param errorHandleProp fix errors
#' @param errorHandleValue fix errors
#' @param labsCookies use getLabsCookies();default=NULL
#' @param dockerName name of docker container ID to use for getting cookies functions
#'
#' @return names of created df/csv files of daily NBA Projections
#' @export
#'
#' @examples nbaOptimalProj(mMonth="2",mDay="11",mYear="2017",labsCookies=labsCook,labsFit=nbaLM13,labs3k=all3000LM3FitStep,labsName="nbaFit12",labsName3k="all3000LM3FitStep",bbmFit=bbmLMNums1,bbm3k=bbmAll3000LM,bbmName="bbmLMNums1",combinedName="nbaLM13_AND_bbmLMNums1",combined3k="daily3000Projections",finalName="OptimalProjections",errorHandleProp=NULL,errorHandleValue=NULL)
nbaOptimalProj <- function(mMonth="2",mDay="12",mYear="2017",labsCookies=NULL,dockerName="monsterLab",labsFit=allStarLmer,labs3k=all3000LM3FitStep,labsName="allStarLmer",labsName3k="all3000LM3FitStep",bbmFit=bbmLMNums1,bbm3k=bbmAll3000LM,bbmName="bbmLMNums1",combinedName="allStarLmer_AND_bbmLMNums1",combined3k="daily3000Projections",finalName="OptimalProjections",errorHandleProp=NULL,errorHandleValue=NULL){
  require(dplyr)

  if(is.null(labsCookies)){
    labsCookies<- getLabsCookies(dockerName=dockerName,sport="nba")
  }
  modelDate<- paste0(mMonth,"_",mDay,"_",mYear)
  suppressWarnings(nbaFullRun(modelDate=modelDate,step=labsFit,model2name=labsName,cookie=labsCookies))
  nba <- suppressWarnings(nbaMonsterLabProjections(mMonth=mMonth,mDay=mDay,mYear=mYear,labsFit=labsFit,labsName=labsName,bbmFit=bbmFit,combinedName=combinedName,errorHandleProp=errorHandleProp,errorHandleValue=errorHandleValue))
  #nba3k <- suppressWarnings(nbaMonsterLabProjections(mMonth=mMonth,mDay=mDay,mYear=mYear,labsFit=labs3k,labsName=labsName3k,bbmFit=bbm3k,combinedName=combined3k,errorHandleProp=errorHandleProp,errorHandleValue=errorHandleValue))
  nba <- nba %>% filter(Salary>4000)
  #nba3k <- nba3k %>% filter(Salary<4100)
  #nbaAll <- suppressWarnings(bind_rows(nba,nba3k))
  nbaAll <- suppressWarnings(bind_rows(nba))
  nbaAll <- na.zero(nbaAll)
  write.csv(nbaAll,file=paste0("~/Desktop/NBA_Daily/",mMonth,"_",mDay,"_",mYear,finalName,".csv"))
  zeroLabs <- suppressWarnings(labZeroProjections(modelDate=paste0(mMonth,"_",mDay,"_",mYear),zeroFit=labZero2Step,labsCookies=labsCookies))
  zeroLabs <- na.zero(zeroLabs)
  zeroFull <- suppressWarnings(full_join(nbaAll,zeroLabs))
  write.csv(zeroFull,file=paste0("~/Desktop/NBA_Daily/",mMonth,"_",mDay,"_",mYear,"zeroFull",".csv"))
  projectionNames <- list("labsCombined","OptimalProjections","zeroFull")
  return(projectionNames)
}
