#' Labs Zero Ownership Projections
#'
#' @param modelDate date "1_29_2017"
#' @param zeroFit regression Model to use for projections
#' @param labsCookies labs cookies data for access to models
#'
#' @return df/csv for zero ownership projected players from labs
#' @export
#'
#' @examples labZeroProjections("1_29_2017",zeroFit=labZero2Step)
labZeroProjections<- function(modelDate="2_12_2017",zeroFit=labZero2Step,labsCookies=NULL){
  getNBAPlayerModel(modelDate=modelDate,cookie=labsCookies)

  labModel <- read.csv(file=paste0("~/Desktop/NBA_Daily/",modelDate,".csv"))
  labModel <- na.zero(labModel)
  labModel <- labModel %>% filter(Properties.p_own=="0-1")
  zeroProj <- predict(zeroFit,labModel)
  zeroDF <- data.frame(labModel$Team,labModel$Properties.Player_Name,zeroProj,labModel$Salary,labModel$Salary/zeroProj,(zeroProj/(labModel$Salary/1000)*5.5)*100)
  names(zeroDF)<- list("Team","Name","zeroProj","Salary","zero_$/Pt.","zero_GPP%")
  zeroDF <- zeroDF %>% filter(zeroProj>1)
  write.csv(zeroDF,file=paste0("~/Desktop/NBA_Daily/",modelDate,"zeroProjections.csv"))
  return(zeroDF)
}
