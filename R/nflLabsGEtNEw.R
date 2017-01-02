#' NFL FantasyLabs Model Retrieve
#'
#' @param modelYear "2016"
#' @param modelWeek 17
#' @param model NULL
#' @param mod NULL
#' @param PositionID "QB"
#'
#' @return FantasyLabs NFL model
#' @export
#'
#' @examples getNflLabModel(modelYear = "2015",modelWeek = NULL,model = NULL,mod = "", PositionID = NULL)
getNflLabModel <- function(modelYear = "2015",modelWeek = NULL,model = NULL,mod = "", PositionID = NULL,cookie ="__cfduid=d46b64495b55215841566c89ce01d6fbd1480519747; LD_U=http%3A%2F%2Fwww.fantasylabs.com%2F; __distillery=bb3e53d_b333f0ea-b6ab-4d8b-bcbb-e8f78b8d5c3f-86a4ffd60-c4a9ff71f0fe-aef4; LD_S=1483050568679; LD_R=; _gat=1; .AspNet.Cookies=y5EwFMncNLomeFSafp9SIE7fCvJBgJX7e7NEzShGZwzkrAV2GpihUEdVaavtghwRaQAexBI_oHlDKO1hi3BXQUbyWlS9F9Vl6nduqcs0aJ2pjEhtahx_Lww_by4_RhWOSIoPc62aqDWdAJ7XXncYhb7PNk8kTE_T44WqWADL5Qc5LB3upqEk20amdTdHK8sWKFgaNs054FSxnwjki6pLxBQ_4IehZ2uo_8XqBaLYQSfWPiDKZZXY93CUIHRuTB6Vk6YqGdvTqX-lqDCu2MjdNwvwcnbRs8xMD3veXuvlUPADlzUv6UY4E1lQ942M0BJRK-n1Vs1rBcB7ItL0fogJ-mMC-ZQc_AYJo_uG3Pa5mWHWnPmmFHlS0goRDs9zUTa4SPiPtmCReo14fgRdS0YdQZWUWd0uy9qwSGEgS3cjK0xx3EB5VdM7U-YfYG7OCOPCEnz6mqVWJPxdji_ckRWZPxWVUuiOy8nhihJbqgzoWNY; flid=9FKN218a_UamhblGysQA7Q; __zlcmid=e6g4jX86CRpGap; _ga=GA1.2.351079370.1480519759; LD_T=d014f701-2492-4486-bcec-b421e46d54b2'"){
  modelIdList <- c("519959","519957","199070","519650","519649","76475","519987")

  modelWeekDates2015 <- c("9_9_2015","9_16_2015","9_23_2015","9_30_2015","10_7_2015","10_14_2015","10_21_2015","10_28_2015","11_4_2015","11_11_2015","11_18_2015","11_25_2015","11_2_2015","11_9_2015","11_16_2015","11_23_2015","11_30_2015")
  if(modelYear == "2015"){
    if(is.null(modelWeek)){
      modelWeek <- readline("2015, Week 1-17?: ")
    }
    modelDate = modelWeekDates2015[modelWeek]
  }
  modelWeekDates2016 <- c("9_7_2016","9_14_2016","9_21_2016","9_28_2016","10_5_2016","10_12_2016","10_19_2016","10_26_2016","11_2_2016","11_9_2016","11_16_2016","11_23_2016","11_30_2016","12_7_2016","12_14_2016","12_21_2016","12_28_2016")
  if(modelYear == "2016"){
    if(is.null(modelWeek)){
      modelWeek <- readline("2016, Week 1-17?: ")
    }
    modelDate = modelWeekDates2016[modelWeek]
  }
  modelId <- if(is.null(model)){
    modelId <- 1
  }
  if(mod=="Bales"){
    modelId = 2}
  if(mod=="Proj"){
    modelId = 3}
  if(mod=="Levitan"){
    modelId = 4}
  if(mod=="Cash"){
    modelId = 5}
  if(mod=="Optimized"){
    modelId = 6}
  if(mod=="Tournament"){
    modelId = 7}

  modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/1/",modelDate,"/?modelid=",modelIdList[modelId])

  y <- paste0("nfl",modelYear,"Week",modelWeek,PositionID,".json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models/' -H 'Cookie: ",cookie,"  -o ",y," -H 'Connection: keep-alive' --compressed")
  curlAddress <- paste0("curl ",modelURL," ",pmCurlHandles)
  system(curlAddress)

  nflModelAll <- jsonlite::fromJSON(y, flatten = TRUE)
  nflModelDK <- nflModelAll$PlayerModels[nflModelAll$PlayerModels$Properties.SourceId == 4,]
  if(is.null(PositionID)){
    nflModel <- dplyr::arrange(nflModelDK,desc(Properties.ActualPoints))}
  if(PositionID == "QB"){
    nflModel <- nflModelDK[nflModelDK$PositionId == "101",]
    nflModel <- dplyr::arrange(nflModel,desc(Properties.ActualPoints))}
  if(PositionID == "RB"){
    nflModel <- nflModelDK[nflModelDK$PositionId == "102",]
    nflModel <- dplyr::arrange(nflModel,desc(Properties.ActualPoints))}
  if(PositionID == "WR"){
    nflModel <- nflModelDK[nflModelDK$PositionId == "103",]
    nflModel <- dplyr::arrange(nflModel,desc(Properties.ActualPoints))}
  if(PositionID == "TE"){
    nflModel <- nflModelDK[nflModelDK$PositionId == "104",]
    nflModel <- dplyr::arrange(nflModel,desc(Properties.ActualPoints))}
  if(PositionID == "DST"){
    nflModel <- nflModelDK[nflModelDK$PositionId == "105",]
    nflModel <- dplyr::arrange(nflModel,desc(Properties.ActualPoints))}

  LabsModel <- data.frame(sapply(nflModel,as.character))
  LabsModel$Properties.p_own <- paste0("'",LabsModel$Properties.p_own)
  write.csv(LabsModel, file = paste0("~/Desktop/NFL_Daily/NFL_",modelYear,"_Week",modelWeek,"/NFL",modelYear,"Week",modelWeek,PositionID,mod,".csv"))
  return(LabsModel)
}
