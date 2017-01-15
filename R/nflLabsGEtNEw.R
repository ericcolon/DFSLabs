#' NFL FantasyLabs Model Retrieve
#'
#' @param modelYear "2016"
#' @param modelWeek 17
#' @param model NULL
#' @param mod NULL
#' @param PositionID "QB"
#' @param modelSite Draftkings or FanDuel
#' @param cookie FantasyLabs browser session cookie to enable access to models
#'
#' @return FantasyLabs NFL model
#' @export
#'
#' @examples getNflLabModel(modelYear = "2015",modelSite="Draftkings",modelWeek = NULL,model = NULL,mod = "", PositionID = NULL)
getNflLabModel <- function(modelYear = "2015",modelSite="Draftkings",modelWeek = NULL,model = NULL,mod = "", PositionID = NULL,cookie ="__cfduid=d8a77c25dbef815632fdbf4f45e81df8b1483248089; LD_U=http%3A%2F%2Fwww.fantasylabs.com%2F; flid=HQAMnfjSB0ytEUbUy1F_3w; __distillery=13f6068_5d3aa9eb-3e25-403f-be81-5a58b0d371e7-0ef1805cc-ee4b27f5fbc2-cf6a; LD_S=1484395001153; LD_R=; _gat=1; __zlcmid=eNg7LAWTPEIN0T; .AspNet.Cookies=HvlkmjkB2hOLIjpz6d0F83ga6mSltxFn8SlRcPAeSTYlVEK-KGeBLODL74KcuXggUxjSHMhaVKsj105PJl94rsAvzs6SiqsqB3GevubDHupyP26duElDEXAVnqdfW9hUGPmtq7JTr0gMncjlN-vvbQgmSnVT1pg_WxhtojndN5qmtBYK1ZGWOHsvqCapHd7AgFYGwdOfzez01eKVyAtLG0IUXSZnjfXUviFn1U2XeW1VypD_42uykPSBpVyBJ3h1cwSUiGVfCn22BYxejNKo-v--nqwob_7fj2LXNaK3FMBg0qBerQ911pp25SyrhshdxElQf3b4b83Q8SquwW-uuxWhyLj-HL7zk7DblWtK9L-QYk7DEe7w3UoDtrkAG9TiKEbEnBJxv-aI2jmIC9xghrIaBHsDXvq_tnrASbMb4cF0nUmrgZQV3ZEncc0qpHx4FMW-alc8izQJxWQuNy4tNc5hvYgON6oQeOkrSigFjTY; _ga=GA1.2.2034235139.1483248092; LD_T=3afe007d-48de-4954-9c64-a6dd5f392768'"){
  modelIdList <- c("519959","519957","199070","519650","519649","76475","519987","832809")

  modelWeekDates2015 <- c("9_9_2015","9_16_2015","9_23_2015","9_30_2015","10_7_2015","10_14_2015","10_21_2015","10_28_2015","11_4_2015","11_11_2015","11_18_2015","11_25_2015","12_2_2015","12_9_2015","12_16_2015","12_23_2015","12_30_2015","1_6_2016","1_13_2016","1_20_2016","2_3_2016")
  if(modelYear == "2015"){
    if(is.null(modelWeek)){
      modelWeek <- readline("2015, Week 1-17?: ")
    }
    modelDate = modelWeekDates2015[modelWeek]
  }
  modelWeekDates2016 <- c("9_7_2016","9_14_2016","9_21_2016","9_28_2016","10_5_2016","10_12_2016","10_19_2016","10_26_2016","11_2_2016","11_9_2016","11_16_2016","11_23_2016","11_30_2016","12_7_2016","12_14_2016","12_21_2016","12_28_2016","1_4_2017","1_11_2017","1_18_2017")
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
  if(mod=="Playoffs"){
    modelId = 8}

  modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/1/",modelDate,"/?modelid=",modelIdList[modelId])

  y <- paste0("nfl",modelYear,"Week",modelWeek,PositionID,".json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models/' -H 'Cookie: ",cookie,"  -o ",y," -H 'Connection: keep-alive' --compressed")
  curlAddress <- paste0("curl ",modelURL," ",pmCurlHandles)
  system(curlAddress)

  nflModelAll <- jsonlite::fromJSON(y, flatten = TRUE)
  if(modelSite=="Draftkings"){
  nflModelDK <- nflModelAll$PlayerModels[nflModelAll$PlayerModels$Properties.SourceId == 4,]
  }
  if(modelSite=="FanDuel"){
  nflModelDK <- nflModelAll$PlayerModels[nflModelAll$PlayerModels$Properties.SourceId == 3,]
  }
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
  if(modelSite=="Draftkings"){
    write.csv(LabsModel, file = paste0("~/Desktop/NFL_Daily/NFL_",modelYear,"_Week",modelWeek,"/NFL",modelYear,"Week",modelWeek,PositionID,mod,".csv"))
  }
  if(modelSite=="FanDuel"){
    write.csv(LabsModel, file = paste0("~/Desktop/NFL_Daily/NFL_",modelYear,"_Week",modelWeek,"/NFL",modelYear,"Week",modelWeek,PositionID,mod,"FanDuel.csv"))
  }

    return(LabsModel)
}
