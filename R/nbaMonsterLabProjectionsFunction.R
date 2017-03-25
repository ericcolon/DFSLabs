#' NBA Labs and BBMonster projections Function
#'
#' @param mMonth "1"
#' @param mDay "27"
#' @param mYear "2017"
#' @param labsFit nbaLM12
#' @param bbmFit bbmLMNums1
#' @param labsName "nbaLM12"
#' @param bbmName "bbmLMNums1"
#' @param combinedName "MonsterLab1"
#' @param bbmCookie "49390133.1485608109.1.1" the _utmz cookie frm your bbmonster session(update every 30 minutes...)
#'
#' @return df/csv of NBA projections using both models
#' @export
#'
#' @examples nbaMonsterLabProjections(mMonth="1",mDay="27",mYear="2017",labsFit=nbaLM12,bbmFit=bbmLMNums1,labsName="nbaFit12",bbmName="bbmLMNums1",combinedName="nbaLM12_AND_bbmLMNums1",bbmCookie="49390133.1485608109.1.1")
nbaMonsterLabProjections <- function(mMonth="1",mDay="27",mYear="2017",labsFit=nbaLM12,bbmFit=bbmLMNums1,labsName="nbaFit12",bbmName="bbmLMNums1",combinedName="nbaLM12_AND_bbmLMNums1",bbmCookie=NULL,errorHandleProp=NULL,errorHandleValue=NULL){
  modelDate<-paste0(mMonth,"_",mDay,"_",mYear)
  labs <- nbaLinRegProjections(modelDate=modelDate,lmFit=labsFit,lmModel=labsName,errorHandleProp=errorHandleProp,errorHandleValue=errorHandleValue)
  bbm0 <- na.zero(getBBMModel2())
  bbm <- data.frame(bbm0)
  bbm <- na.zero((bbm))
  bbmPred <- predict(bbmLMNums1,bbm)
  bbmValues <- data.frame(bbm$Name,bbmPred)
  names(bbmValues) <- c("Name","bbmProjection")
  nbaFull <- full_join(labs,bbmValues)
  bbmV <- nbaFull$Salary/nbaFull$bbmProjection
  GPPScore <- (nbaFull$Salary/1000)*5.5
  AvgProjection <- (nbaFull$bbmProjection+nbaFull$Projection)/2
  GPPLabs <- ((nbaFull$Projection/GPPScore)*100)
  GPPbbm <- ((nbaFull$bbmProjection/GPPScore)*100)
  GPPAvg <- ((AvgProjection/GPPScore)*100)
  FinalDolp <- nbaFull$Salary/AvgProjection
  pName <- nbaFull$Name
  nbaFull <- nbaFull %>% mutate(bbmValue=bbmV,GPP=GPPScore,Name2=pName,AvgProj=AvgProjection,GPPL=GPPLabs,GPPb=GPPbbm,GPPTotal=GPPAvg,FinalDolPer=FinalDolp)
  nbaFull <- nbaFull %>% filter((!is.na(Salary)))
  nbaFull <- nbaFull %>% filter(ActualPoints>=0)
  nbaFull <- nbaFull %>% filter((!is.na(AvgProj)))

  write.csv(nbaFull,file=paste0("~/Desktop/NBA_Daily/",mMonth,"_",mDay,"_",mYear,"NBA",combinedName,".csv"))
  return(nbaFull)
  }
