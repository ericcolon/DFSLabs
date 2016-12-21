#' NBA Model Retrieve
#'
#' @param modelDate
#' @param cookie
#' @param slate
#' @param exclude
#' @param switch
#' @param returnFull
#'
#' @return Fantasylabs NBA model
#' @export
#'
#' @examples getNBAPlayerModel()
#'


getNBAPlayerModel <- function(modelDate = NULL, slate = NULL, exclude = NULL, switch=NULL,returnFull=NULL, cookie = "__cfduid=d46b64495b55215841566c89ce01d6fbd1480519747; LD_U=http%3A%2F%2Fwww.fantasylabs.com%2F; __distillery=bb3e53d_b333f0ea-b6ab-4d8b-bcbb-e8f78b8d5c3f-86a4ffd60-c4a9ff71f0fe-aef4; .AspNet.Cookies=EVGJH0IuWLy8RuLAWqA3JVB3L8lvyXCaqIDX7mPnHvFo-X76ra1UZRbqo5URKoMo2lrt_BKDiQXclEtWa_hgP7lB6nJsVKo-4dCzLhGJnKZUhq7vWHYc0dHsl2_3m8Wjy-PgYHci5gghAw5y8Lp-rKbi1hRYrHaLQH1_d58PZIZ5q9spxBoCCbZ2ooD-HG0xYfHcPot8_DNrfbeW0XMnByaFVebeNnfem-Mb11nh-wHR3De5peqOHz0-BaRMpG3CnjW8N_pa3OO6BgVMdQ-kr16OSk82gNYDIAAMAvWaqfghUMUjoouY8bJPvqMLcEYX7E2-4K0VWR6usBYAWeZhYOPWPmK9oKhn38Y7RnTawoCNOnqhh6mQyCblU5YwhP7U2xztRCzaNk1N0hvKMvHfQbS35ExGTqeVin7bQIMynh2nYqkjFxhjIT9USeCxQj1zu0UFVZ937dHIARI47-A_QBmier5CJ0Q29n4Isdcu8Bw; flid=-t582kVLjUizx14ZPB2BXQ; __zlcmid=e6g4jX86CRpGap; LD_S=1482140145260; LD_R=; _gat=1; _ga=GA1.2.351079370.1480519759; LD_T=d014f701-2492-4486-bcec-b421e46d54b2'"){
 if (is.null(exclude)) {
   if (is.null(switch)) {
   modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/2/",modelDate,"/?modelid=709033")
    } else {
        modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/2/",modelDate,"/?modelid=",switch)
    }
   if (is.null(switch)){
      y <- paste0("NBA",modelDate,".json")
   } else {
      y <- paste0("NBA",modelDate,switch,".json")
   }
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



      LabsFixedNames1 <- sapply(nbaModelDK,as.character)
      LabsFixedNames2 <- sapply(nbaFDOwn,as.character)
      LabsFixedNames2 <- na.zero(LabsFixedNames2)
      if(is.null(switch)){
      write.csv(LabsFixedNames1, file = paste0("~/Desktop/NBA_Daily/",modelDate,".csv"))
      write.csv(LabsFixedNames2, file = paste0("~/Desktop/NBA_Daily/",modelDate,"_FD.csv"))
      } else {
        write.csv(LabsFixedNames1, file = paste0("~/Desktop/NBA_Daily/",modelDate,switch,".csv"))
        write.csv(LabsFixedNames2, file = paste0("~/Desktop/NBA_Daily/",modelDate,switch,"_FD.csv"))
      }
 } else {
   if (is.null(switch)) {
     modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/2/",modelDate,"/?modelid=709033")
   } else {
     modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/2/",modelDate,"/?modelid=",switch)
   }
   if (is.null(switch)){
    y <- paste0("~/Documents/NBA",modelDate,".json")
   } else {
    y <- paste0("~/Documents/NBA",modelDate,switch,".json")
   }
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
    if (is.null(switch)){
    write.csv(LabsFixedNames1, file = paste0("~/Desktop/NBA_Daily/",modelDate,slate,".csv"))
    write.csv(LabsFixedNames2, file = paste0("~/Desktop/NBA_Daily/",modelDate,slate,"_FD.csv"))
    } else {
      write.csv(LabsFixedNames1, file = paste0("~/Desktop/NBA_Daily/",modelDate,slate,switch,".csv"))
      write.csv(LabsFixedNames2, file = paste0("~/Desktop/NBA_Daily/",modelDate,slate,switch,"_FD.csv"))
    }
 }

  if(returnFull=="yes"){
    return(LabsFixedNames1)
  } else {
    return(LabsFixedNames2)
  }
}
