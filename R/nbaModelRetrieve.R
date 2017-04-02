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


getNBAPlayerModel <- function(modelDate = NULL, modelSite = "Draftkings", slate = NULL, exclude = NULL, cookie = labsC){
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
