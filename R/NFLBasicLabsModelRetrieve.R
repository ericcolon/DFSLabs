#'NFL FantasyLabs Model Retrieve
#'
#' @param modelYear (2015,2016)
#' @param modelWeek  (1-17)
#' @param model (1...42)
#' @param showModelList default FALSE(takes thru interactive modeling if TRUE)
#' @param PositionID (QB.RB.WR,TE,DST)
#'
#' @param cookie cookies from browser needed to get into fantasylabs account
#'
#' @return FantasyLabs NFL model
#' @export
#'
#' @examples get.NflLabModel_old(modelYear = "2015",modelWeek = NULL,model = NULL,mod = "", PositionID = NULL)
get.NflLabModel_old <- function(modelYear = "2016",modelWeek = NULL,model = NULL,showModelList = FALSE, PositionID = NULL, cookie = "__cfduid=d46b64495b55215841566c89ce01d6fbd1480519747; LD_U=http%3A%2F%2Fwww.fantasylabs.com%2F; __distillery=bb3e53d_b333f0ea-b6ab-4d8b-bcbb-e8f78b8d5c3f-86a4ffd60-c4a9ff71f0fe-aef4; _gat=1; LD_S=1481767246798; LD_R=; .AspNet.Cookies=EVGJH0IuWLy8RuLAWqA3JVB3L8lvyXCaqIDX7mPnHvFo-X76ra1UZRbqo5URKoMo2lrt_BKDiQXclEtWa_hgP7lB6nJsVKo-4dCzLhGJnKZUhq7vWHYc0dHsl2_3m8Wjy-PgYHci5gghAw5y8Lp-rKbi1hRYrHaLQH1_d58PZIZ5q9spxBoCCbZ2ooD-HG0xYfHcPot8_DNrfbeW0XMnByaFVebeNnfem-Mb11nh-wHR3De5peqOHz0-BaRMpG3CnjW8N_pa3OO6BgVMdQ-kr16OSk82gNYDIAAMAvWaqfghUMUjoouY8bJPvqMLcEYX7E2-4K0VWR6usBYAWeZhYOPWPmK9oKhn38Y7RnTawoCNOnqhh6mQyCblU5YwhP7U2xztRCzaNk1N0hvKMvHfQbS35ExGTqeVin7bQIMynh2nYqkjFxhjIT9USeCxQj1zu0UFVZ937dHIARI47-A_QBmier5CJ0Q29n4Isdcu8Bw; flid=-t582kVLjUizx14ZPB2BXQ; __zlcmid=e6g4jX86CRpGap; _ga=GA1.2.351079370.1480519759; LD_T=d014f701-2492-4486-bcec-b421e46d54b2'"){
  #modelIdList <- c("554405","519957","199070","519650","519649","76475","519987")
  nflSystems <- read.csv(file = paste0("~/Desktop/NFL_Daily/nflLabModelIds",".csv"),stringsAsFactors = FALSE)
  nflSystems <- nflSystems %>% select(-X)
  modelWeekDates2015 <- c("9_9_2015","9_16_2015","9_23_2015","9_30_2015","10_7_2015","10_14_2015","10_21_2015","10_28_2015","11_4_2015","11_11_2015","11_18_2015","11_25_2015","11_2_2015","11_9_2015","11_16_2015","11_23_2015","11_30_2015")
  modelWeekDates2016 <- c("9_7_2016","9_14_2016","9_21_2016","9_28_2016","10_5_2016","10_12_2016","10_19_2016","10_26_2016","11_2_2016","11_9_2016","11_16_2016","11_23_2016","11_30_2016","12_7_2016","12_14_2016","12_21_2016","12_28_2016")
  modNumb <- nrow(nflSystems)
  if(modelYear == "2015"){
    if(is.null(modelWeek)){
      modelWeek <- readline("2015, Week 1-17?: ")
    }
    modelDate = modelWeekDates2015[modelWeek]
  }
  if(modelYear == "2016"){
    if(is.null(modelWeek)){
      modelWeek <- readline("2016, Week 1-17?: ")
    }
    modelDate = modelWeekDates2016[modelWeek]
  }


    modelId <- nflSystems[35,2]
    modelName <- nflSystems[35,1]
    if (!is.null(model)) {
      modelId = nflSystems[model,2]
      modelName = nflSystems[model,1]
    }
  modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/1/",modelDate,"/?modelid=",modelId)

  y <- paste0("nfl",modelYear,"Week",modelWeek,PositionID,"model_",modelId,".json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models/' -H 'Cookie: ",cookie,"  -o ",y," -H 'Connection: keep-alive' --compressed")
  curlAddress <- paste0("curl ",modelURL," ",pmCurlHandles)
  system(curlAddress)

  nflModelAll <- jsonlite::fromJSON(y, flatten = TRUE)
  nflModelDK <- nflModelAll$PlayerModels[nflModelAll$PlayerModels$Properties.SourceId == 4,]
  nflModel <- dplyr::arrange(nflModelDK,desc(Properties.ActualPoints))

  nflModelQB <- nflModel %>% filter(Properties.PositionId == "101")
  nflModelRB <- nflModel %>% filter(Properties.PositionId == "102")
  nflModelWR <- nflModel %>% filter(Properties.PositionId == "103")
  nflModelTE <- nflModel %>% filter(Properties.PositionId == "104")
  nflModelDST <- nflModel %>% filter(Properties.PositionId == "105")


  LabsModel <- sapply(nflModel,as.character)
  LabsModelQB <- sapply(nflModelQB,as.character)
  write.csv(LabsModel, file = paste0("~/Desktop/NFL_Daily/NFL_",modelYear,"_Week",modelWeek,"/NFL",modelYear,"Week",modelWeek,"All",modelId,".csv"))
  write.csv(LabsModelQB, file = paste0("~/Desktop/NFL_Daily/NFL_",modelYear,"_Week",modelWeek,"/NFL",modelYear,"Week",modelWeek,"QB",modelId,".csv"))
  LabsModelQB <- sapply(nflModelQB,as.character)
  LabsModelRB <- sapply(nflModelRB,as.character)
  LabsModelWR <- sapply(nflModelWR,as.character)
  LabsModelTE <- sapply(nflModelTE,as.character)
  LabsModelDST <- sapply(nflModelDST,as.character)
  write.csv(LabsModelRB, file = paste0("~/Desktop/NFL_Daily/NFL_",modelYear,"_Week",modelWeek,"/NFL",modelYear,"Week",modelWeek,"RB",modelId,".csv"))
  write.csv(LabsModelWR, file = paste0("~/Desktop/NFL_Daily/NFL_",modelYear,"_Week",modelWeek,"/NFL",modelYear,"Week",modelWeek,"WR",modelId,".csv"))
  write.csv(LabsModelTE, file = paste0("~/Desktop/NFL_Daily/NFL_",modelYear,"_Week",modelWeek,"/NFL",modelYear,"Week",modelWeek,"TE",modelId,".csv"))
  write.csv(LabsModelDST, file = paste0("~/Desktop/NFL_Daily/NFL_",modelYear,"_Week",modelWeek,"/NFL",modelYear,"Week",modelWeek,"DST",modelId,".csv"))

}
