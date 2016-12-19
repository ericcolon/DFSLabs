#' NBA model Retrieve and projections export
#'
#' @param modelDate 
#'
#' @return FantasyLabs NBA Model projections
#' @export
#'
#' @examples getNbaModelandProject()
getNbaModelandProject <- function(modelDate = "10_30_2015"){
  getNBAPlayerModel(modelDate)
  readCSV <- readNBACSVs(modelDate)
  readCSV <- na.zero(readCSV)
  project1 <- readCSV$Properties.OppPlusMinus*0.2170312559
  project2 <- readCSV$Salary*0.0016437112
  project3 <- readCSV$Properties.Month_Salary_Change*-0.0008733492
  project4 <- readCSV$Properties.CeilingPct*0.1211694691
  project5 <- readCSV$Properties.FantasyPerMinute*2.5317331466
  project6 <- readCSV$Properties.Floor*0.4278688107
  Final <- project1+project2+project3+project4+project5+project6+1.6241031979
  finalDf <- data.frame(readCSV$Properties.Player_Name,Final,readCSV$ActualPoints)
  write.csv(finalDf, file = paste0("~/Desktop/NBA_Daily/",modelDate,"Projections.csv"))
  return(finalDf)
}