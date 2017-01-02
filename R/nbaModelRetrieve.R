#' NBA Model Retrieve
#'
#' @param modelDate
#' @param cookie
#'
#' @return Fantasylabs NBA model
#' @export
#'
#' @examples getNBAPlayerModel()
#'


getNBAPlayerModel <- function(modelDate = NULL, slate = NULL, exclude = NULL, cookie = "__cfduid=d46b64495b55215841566c89ce01d6fbd1480519747; LD_U=http%3A%2F%2Fwww.fantasylabs.com%2F; __distillery=bb3e53d_b333f0ea-b6ab-4d8b-bcbb-e8f78b8d5c3f-86a4ffd60-c4a9ff71f0fe-aef4; LD_S=1483050568679; LD_R=; _gat=1; .AspNet.Cookies=y5EwFMncNLomeFSafp9SIE7fCvJBgJX7e7NEzShGZwzkrAV2GpihUEdVaavtghwRaQAexBI_oHlDKO1hi3BXQUbyWlS9F9Vl6nduqcs0aJ2pjEhtahx_Lww_by4_RhWOSIoPc62aqDWdAJ7XXncYhb7PNk8kTE_T44WqWADL5Qc5LB3upqEk20amdTdHK8sWKFgaNs054FSxnwjki6pLxBQ_4IehZ2uo_8XqBaLYQSfWPiDKZZXY93CUIHRuTB6Vk6YqGdvTqX-lqDCu2MjdNwvwcnbRs8xMD3veXuvlUPADlzUv6UY4E1lQ942M0BJRK-n1Vs1rBcB7ItL0fogJ-mMC-ZQc_AYJo_uG3Pa5mWHWnPmmFHlS0goRDs9zUTa4SPiPtmCReo14fgRdS0YdQZWUWd0uy9qwSGEgS3cjK0xx3EB5VdM7U-YfYG7OCOPCEnz6mqVWJPxdji_ckRWZPxWVUuiOy8nhihJbqgzoWNY; flid=9FKN218a_UamhblGysQA7Q; __zlcmid=e6g4jX86CRpGap; _ga=GA1.2.351079370.1480519759; LD_T=d014f701-2492-4486-bcec-b421e46d54b2'"){
 if (is.null(exclude)) {
      modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/2/",modelDate,"/?modelid=709033")
      y <- paste0("NBA",modelDate,".json")
      pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nba/player-models/' -H 'Cookie: ",cookie,"  -o ",y," -H 'Connection: keep-alive' --compressed")
      curlAddress <- paste0("curl ",modelURL," ",pmCurlHandles)
      system(curlAddress)
      nbaModelAll <- jsonlite::fromJSON(y, flatten = TRUE)
      nbaModelDK <- nbaModelAll$PlayerModels[nbaModelAll$PlayerModels$Properties.SourceId == 4,]
      #nbaModel <- nbaModelDK[nbaModelDK$Position == id,]
      nbaModelFD <- nbaModelAll$PlayerModels[nbaModelAll$PlayerModels$Properties.SourceId == 4,]
      nbaFDOwn <- data.frame(nbaModelFD$Properties.Player_Name,nbaModelFD$Properties.p_own)
      names(nbaFDOwn)<- c("Name","Projected_Ownership")
      nbaFDOwn <- na.zero(nbaFDOwn)



      LabsFixedNames1 <- sapply(nbaModelDK,as.character)
      LabsFixedNames2 <- sapply(nbaFDOwn,as.character)
      LabsFixedNames2 <- na.zero(LabsFixedNames2)
      write.csv(LabsFixedNames1, file = paste0("~/Desktop/NBA_Daily/",modelDate,".csv"))
      write.csv(LabsFixedNames2, file = paste0("~/Desktop/NBA_Daily/",modelDate,"_FD.csv"))
 }
  if (!(is.null(exclude))) {
    modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/2/",modelDate,"/?modelid=705700")
    y <- paste0("NBA",modelDate,".json")
    pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nba/player-models/' -H 'Cookie: ",cookie,"  -o ",y," -H 'Connection: keep-alive' --compressed")
    curlAddress <- paste0("curl ",modelURL," ",pmCurlHandles)
    system(curlAddress)
    nbaModelAll <- jsonlite::fromJSON(y, flatten = TRUE)
    nbaModelDK <- nbaModelAll$PlayerModels[nbaModelAll$PlayerModels$Properties.SourceId == 4,]
    #nbaModel <- nbaModelDK[nbaModelDK$Position == id,]
    nbaModelFD <- nbaModelAll$PlayerModels[nbaModelAll$PlayerModels$Properties.SourceId == 4,]
    nbaFDOwn <- data.frame(nbaModelFD$Properties.Player_Name,nbaModelFD$Properties.p_own)
    names(nbaFDOwn)<- list("Name","Projected_Ownership")
    nbaFDOwn <- na.zero(nbaFDOwn)
    excLength <- length(exclude)

    laterNbaSlatesDkDf <- nbaModelDK[!(nbaModelDK$TeamName %in% exclude),]




    LabsFixedNames1 <- sapply(laterNbaSlatesDkDf,as.character)
    LabsFixedNames2 <- sapply(nbaFDOwn,as.character)
    LabsFixedNames2 <- na.zero(LabsFixedNames2)
    write.csv(LabsFixedNames1, file = paste0("~/Desktop/NBA_Daily/",modelDate,slate,".csv"))
    write.csv(LabsFixedNames2, file = paste0("~/Desktop/NBA_Daily/",modelDate,slate,"_FD.csv"))
  }

  return(LabsFixedNames2)

}
