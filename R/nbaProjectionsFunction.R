#' NBA Projections
#'
#' @param modelDate
#'
#' @return NBA projections for desired Date
#' @export
#'
#' @examples projectNBA(modelDate,slate,exclude,cookie)
projectNBA <- function(modelDate = NULL, slate = NULL, exclude = NULL, cookie = "__cfduid=d46b64495b55215841566c89ce01d6fbd1480519747; LD_U=http%3A%2F%2Fwww.fantasylabs.com%2F; __distillery=bb3e53d_b333f0ea-b6ab-4d8b-bcbb-e8f78b8d5c3f-86a4ffd60-c4a9ff71f0fe-aef4; LD_S=1483050568679; LD_R=; _gat=1; .AspNet.Cookies=y5EwFMncNLomeFSafp9SIE7fCvJBgJX7e7NEzShGZwzkrAV2GpihUEdVaavtghwRaQAexBI_oHlDKO1hi3BXQUbyWlS9F9Vl6nduqcs0aJ2pjEhtahx_Lww_by4_RhWOSIoPc62aqDWdAJ7XXncYhb7PNk8kTE_T44WqWADL5Qc5LB3upqEk20amdTdHK8sWKFgaNs054FSxnwjki6pLxBQ_4IehZ2uo_8XqBaLYQSfWPiDKZZXY93CUIHRuTB6Vk6YqGdvTqX-lqDCu2MjdNwvwcnbRs8xMD3veXuvlUPADlzUv6UY4E1lQ942M0BJRK-n1Vs1rBcB7ItL0fogJ-mMC-ZQc_AYJo_uG3Pa5mWHWnPmmFHlS0goRDs9zUTa4SPiPtmCReo14fgRdS0YdQZWUWd0uy9qwSGEgS3cjK0xx3EB5VdM7U-YfYG7OCOPCEnz6mqVWJPxdji_ckRWZPxWVUuiOy8nhihJbqgzoWNY; flid=9FKN218a_UamhblGysQA7Q; __zlcmid=e6g4jX86CRpGap; _ga=GA1.2.351079370.1480519759; LD_T=d014f701-2492-4486-bcec-b421e46d54b2'"){
  getNBAPlayerModel(modelDate,slate,exclude,cookie)
  if (is.null(exclude)) {
    readCSV <- readNBACSVs(modelDate)
    readCSVFd <- readNBACSVs(paste0(modelDate,"_FD"))
    readCSVFd <- na.zero(readCSVFd)
    readCSV <- na.zero(readCSV)
    project1 <- readCSV$Properties.OppPlusMinus*0.2170312559
    project2 <- readCSV$Salary*0.0016437112
    project3 <- readCSV$Properties.Month_Salary_Change*-0.0008733492
    project4 <- readCSV$Properties.CeilingPct*0.1211694691
    project5 <- readCSV$Properties.FantasyPerMinute*2.5317331466
    project6 <- readCSV$Properties.Floor*0.4278688107
    Final <- project1+project2+project3+project4+project5+project6+1.6241031979
    projCostPerPoint <- readCSV$Salary/Final
    actualCostPerPoint <- readCSV$Salary/readCSV$ActualPoints
    finalDf <- data.frame(readCSV$Score,readCSV$Salary,readCSV$Properties.Player_Name,Final,projCostPerPoint,readCSV$ActualPoints,actualCostPerPoint,readCSV$Position)
    names(finalDf)<- list("Score","Salary","Name","Projection","ProjCostPerPt","ActualPoints","ActualCostPerPt","Position(s)")
    finalDf <- full_join(finalDf,readCSVFd, by = "Name")
    finalDf <- finalDf %>% select(-X) %>% arrange(desc(Projection))
    finalDf <- tidyr::separate(data = finalDf, col = Projected_Ownership, into = c("minOWN", "maxOWN"))
    finalDf <- na.zero(finalDf)
    finalDfChar <- sapply(finalDf,as.character)

    write.csv(finalDfChar, file = paste0("~/Desktop/NBA_Daily/",modelDate,"Projections.csv"))
    scoreQuantiles <- quantile(readCSV$Score)
    scoreThirdQuant <- scoreQuantiles[4]
    finalDf3rdQuartileScoreOnly <- finalDf %>% filter(Score>=scoreThirdQuant) %>% arrange(desc(Projection))
    finalDf3rdQuartileScoreOnlyChar <- sapply(finalDf3rdQuartileScoreOnly,as.character)

    write.csv(finalDf3rdQuartileScoreOnlyChar, file = paste0("~/Desktop/NBA_Daily/",modelDate,"Projections3rdQuantileOnly.csv"))
    projValueQuants <- quantile(finalDf[,5])
    projValue1stQuant <- projValueQuants[2]
    projValueFilteredDf <- finalDf %>% filter(ProjCostPerPt<=projValue1stQuant)
    projValueFilteredDf <- projValueFilteredDf %>% arrange(desc(Projection)) %>% arrange(desc(Projection))
    projValueFilteredDf <- sapply(projValueFilteredDf,as.character)
    write.csv(projValueFilteredDf, file = paste0("~/Desktop/NBA_Daily/",modelDate,"ProjectedValuePlays.csv"))
  }
  if (!(is.null(exclude))) {
    readCSV <- readNBACSVs(paste0(modelDate,slate))
    readCSVFd <- readNBACSVs(paste0(modelDate,slate,"_FD"))
    readCSVFd <- na.zero(readCSVFd)
    readCSV <- na.zero(readCSV)
    project1 <- readCSV$Properties.OppPlusMinus*0.2170312559
    project2 <- readCSV$Salary*0.0016437112
    project3 <- readCSV$Properties.Month_Salary_Change*-0.0008733492
    project4 <- readCSV$Properties.CeilingPct*0.1211694691
    project5 <- readCSV$Properties.FantasyPerMinute*2.5317331466
    project6 <- readCSV$Properties.Floor*0.4278688107
    Final <- project1+project2+project3+project4+project5+project6+1.6241031979
    projCostPerPoint <- readCSV$Salary/Final
    actualCostPerPoint <- readCSV$Salary/readCSV$ActualPoints
    finalDf <- data.frame(readCSV$Score,readCSV$Salary,readCSV$Properties.Player_Name,Final,projCostPerPoint,readCSV$ActualPoints,actualCostPerPoint,readCSV$Position)
    names(finalDf)<- list("Score","Salary","Name","Projection","ProjCostPerPt","ActualPoints","ActualCostPerPt","Position(s)")
    finalDf <- left_join(finalDf,readCSVFd, by = "Name")
    finalDf <- finalDf %>% select(-X) %>% arrange(desc(Projection))
    finalDf <- tidyr::separate(data = finalDf, col = Projected_Ownership, into = c("minOWN", "maxOWN"))
    finalDf <- na.zero(finalDf)
    finalDfChar <- sapply(finalDf,as.character)

    write.csv(finalDfChar, file = paste0("~/Desktop/NBA_Daily/",modelDate,slate,"Projections.csv"))
    scoreQuantiles <- quantile(readCSV$Score)
    scoreThirdQuant <- scoreQuantiles[4]
    finalDf3rdQuartileScoreOnly <- finalDf %>% filter(Score>=scoreThirdQuant) %>% arrange(desc(Projection))
    finalDf3rdQuartileScoreOnlyChar <- sapply(finalDf3rdQuartileScoreOnly,as.character)

    write.csv(finalDf3rdQuartileScoreOnlyChar, file = paste0("~/Desktop/NBA_Daily/",modelDate,slate,"Projections3rdQuantileOnly.csv"))
    projValueQuants <- quantile(finalDf[,5])
    projValue1stQuant <- projValueQuants[2]
    projValueFilteredDf <- finalDf %>% filter(ProjCostPerPt<=projValue1stQuant) %>% arrange(desc(Projection))
    projValueFilteredDf <- sapply(projValueFilteredDf,as.character)
    write.csv(projValueFilteredDf, file = paste0("~/Desktop/NBA_Daily/",modelDate,slate,"ProjectedValuePlays.csv"))
  }
  return(finalDf)
}
