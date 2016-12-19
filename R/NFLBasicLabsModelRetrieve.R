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
#' @examples getNflLabModel(modelYear = "2015",modelWeek = NULL,model = NULL,mod = "", PositionID = NULL)
getNflLabModel <- function(modelYear = "2016",modelWeek = NULL,model = NULL,showModelList = FALSE, PositionID = NULL, cookie = "__cfduid=d19da73f21ae9852e137438d8ffd398371475796569; LD_U=http%3A%2F%2Fwww.fantasylabs.com%2F; __distillery=a88774e_344d8f47-a8f7-4094-81ba-d3336d4c2d4d-ba08c6bef-19ba082fafba-d101; LD_R=http%3A%2F%2Fwww.fantasylabs.com%2Fnfl%2Fplayer-models%2F; .AspNet.Cookies=I3uyNArip8P2mwm4iZUdp1aswKJvJUJsgXxxhq2yU-UayOPc51mQDfzLkb2HihonPOWkz3ZPkcBeFOk1SeaGwDn5-Oh61Xu1cUsarqZkutXgbTBwc2V6ITx_u4lMXjuodLaUjd3x0DcSg2Y7YFMQwjduY9bLFWkLyDqr5rutPNS5bxAxO80joxoa7tfhKOC0vLGmO3ZCDMTWGNXAMqiGpfG5p7eiS7drDO5q9AEwYYux_fLWATa04cZXGRkHc3eae_7B_eaGUaFPNqom5dZ2ewqZRHktUEGo4JavxHfv4TMxS4t9URXN3YxK6YGh7edIh1pFHuZ1RQpVolIHQzbuedjxPb_dkSwZ1iVPcplBM10TV3g3NRDZXZVQ0LYWbkFUdqEvbMeiBMos-kNeurxSrlKdwbQTro0vhq_ckdsRFtduXzQ-zxsbcwWHiUrNNOshYTZMau5j2tZzPsPyCOLGrPXJPJd-ZaKSJNIL_gY5pz8; flid=yijf7wz4JEegj3XR4O5vLw; _gat=1; LD_S=1478649158829; __zlcmid=cyfuDnxjy8XwIK; LD_T=6a763f78-d230-4827-f05d-37ca01630a5d; _ga=GA1.2.407536382.1475796573'"){
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
