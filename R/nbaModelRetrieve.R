#' NBA Model Retrieve
#'
#' @param modelDate "1_1_2017"
#' @param cookie for cURL from FLabs (Chrome Browser)..
#' @param modelSite Draftkings or FanDuel
#' @param slate Main,turbo,late,etc
#' @param exclude c("ATL","NYK")
#'
#' @return Fantasylabs NBA model
#' @export
#'
#' @examples getNBAPlayerModel()
#'


getNBAPlayerModel <- function(modelDate = NULL, modelSite = "Draftkings", slate = NULL, exclude = NULL, cookie = "__cfduid=d3754702e05d38930dec5c0d04d6ebdad1485624789; LD_U=http%3A%2F%2Fwww.fantasylabs.com%2F; .AspNet.Cookies=ktnc3djb1JUCI9JKWmXzxZ-F2MHoT9qImiixxGVPd2a2L8JwaiC2Amf5FijhLC1uY8-xsUP3uoRS81HjIG63W2uA0T_FRD5UaovpXg8LyiAOV9Ku60VOC3l6sFCTt1SNFH7hZ_MYgoqUgoucY5lsL9Nm7bmEX79WpB3J9KeCPEXkapD4Ap379jqi1qitYuX1-HQ-r-4PnLy6sJmZSCs7t512GEDlZXYFcvKggIh6Azi2g_DmFJ2y1qzJY2won_-1z4TRdnA84CaJRVGDoYVqa_5oiH_cRSoFVFaWh-PspbXV-WS8TZBjrRMOcw-pjSyad0zP8tVzBFUsI6gpjvWBHaAtDRr2LvDkZamSu_dnRjTj8z7dtPs0N3R7CAdxxe7r-M5jnB6ydRMwGjfJn1yFOypiYPxqeytGrsBAVrFsHR_-vn9M80ZwJv9hUNRPFnBsR7szJuXzpUFJ_Q-xMOnyNPWIfR6gSkqkyMC4kE5Wzd0; flid=5ANbAhdxrkqOWJnJYGArDA; __distillery=13f6068_5d3aa9eb-3e25-403f-be81-5a58b0d371e7-0ef1805cc-ee4b27f5fbc2-cf6a; LD_S=1485637044840; LD_R=; _gat=1; __zlcmid=eogBWbcILnbRCJ; LD_T=b63fdc9b-f2f3-4ff9-cfda-426bb863d4b9; _ga=GA1.2.1049198149.1485624801'"){
if(modelSite=="Draftkings"){
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
}
if(modelSite=="FanDuel"){
  if (is.null(exclude)) {
    modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/2/",modelDate,"/?modelid=709033")
    y <- paste0("NBA",modelDate,".json")
    pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nba/player-models/' -H 'Cookie: ",cookie,"  -o ",y," -H 'Connection: keep-alive' --compressed")
    curlAddress <- paste0("curl ",modelURL," ",pmCurlHandles)
    system(curlAddress)
    nbaModelAll <- jsonlite::fromJSON(y, flatten = TRUE)
    nbaModelFD <- nbaModelAll$PlayerModels[nbaModelAll$PlayerModels$Properties.SourceId == 3,]
    #nbaModel <- nbaModelFD[nbaModelFD$Position == id,]
    nbaModelFD <- nbaModelAll$PlayerModels[nbaModelAll$PlayerModels$Properties.SourceId == 3,]
    nbaFDOwn <- data.frame(nbaModelFD$Properties.Player_Name,nbaModelFD$Properties.p_own)
    names(nbaFDOwn)<- c("Name","Projected_Ownership")
    nbaFDOwn <- na.zero(nbaFDOwn)



    LabsFixedNames1 <- sapply(nbaModelFD,as.character)
    LabsFixedNames2 <- sapply(nbaFDOwn,as.character)
    LabsFixedNames2 <- na.zero(LabsFixedNames2)
    write.csv(LabsFixedNames1, file = paste0("~/Desktop/NBA_Daily/",modelDate,"FanDuel.csv"))
    write.csv(LabsFixedNames2, file = paste0("~/Desktop/NBA_Daily/",modelDate,"_FDOwn.csv"))
  }
  if (!(is.null(exclude))) {
    modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/2/",modelDate,"/?modelid=705700")
    y <- paste0("NBA",modelDate,".json")
    pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nba/player-models/' -H 'Cookie: ",cookie,"  -o ",y," -H 'Connection: keep-alive' --compressed")
    curlAddress <- paste0("curl ",modelURL," ",pmCurlHandles)
    system(curlAddress)
    nbaModelAll <- jsonlite::fromJSON(y, flatten = TRUE)
    nbaModelFD <- nbaModelAll$PlayerModels[nbaModelAll$PlayerModels$Properties.SourceId == 3,]
    #nbaModel <- nbaModelFD[nbaModelFD$Position == id,]
    nbaModelFD <- nbaModelAll$PlayerModels[nbaModelAll$PlayerModels$Properties.SourceId == 3,]
    nbaFDOwn <- data.frame(nbaModelFD$Properties.Player_Name,nbaModelFD$Properties.p_own)
    names(nbaFDOwn)<- list("Name","Projected_Ownership")
    nbaFDOwn <- na.zero(nbaFDOwn)
    excLength <- length(exclude)

    laterNbaSlatesFDDf <- nbaModelFD[!(nbaModelFD$TeamName %in% exclude),]




    LabsFixedNames1 <- sapply(laterNbaSlatesFDDf,as.character)
    LabsFixedNames2 <- sapply(nbaFDOwn,as.character)
    LabsFixedNames2 <- na.zero(LabsFixedNames2)
    write.csv(LabsFixedNames1, file = paste0("~/Desktop/NBA_Daily/",modelDate,slate,"FanDuel.csv"))
    write.csv(LabsFixedNames2, file = paste0("~/Desktop/NBA_Daily/",modelDate,slate,"_FDOwn.csv"))
  }
}

return(LabsFixedNames2)

}
