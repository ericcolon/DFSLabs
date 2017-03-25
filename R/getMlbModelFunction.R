#' MLB FantasyLabs Model Retrieve
#'
#' @param modelDate "4_2_2017"
#' @param cookie labCookies
#' @param View H or P
#'
#' @return hitter, pitcher, or ownership percentage models
#' @export
#'
#' @examples getMlbModel("4_2_2017",labCookies,"H")
getMlbModel <- function(modelDate=NULL, cookie=labCookies, View=NULL){

    modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/3/",modelDate,"/?modelid=346142")
    #modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/2/",modelDate,"/?modelid=813769")
    y <- paste0("MLB",modelDate,".json")
    pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/mlb/player-models/' -H 'Cookie: ",cookie,"  -o ",y," -H 'Connection: keep-alive' --compressed")
    curlAddress <- paste0("curl ",modelURL," ",pmCurlHandles)
    system(curlAddress)
    MLBModelAll <- jsonlite::fromJSON(y, flatten = TRUE)
    MLBModelDK <- MLBModelAll$PlayerModels[MLBModelAll$PlayerModels$Properties.SourceId == 4,]
    MLBModelH <- MLBModelDK[MLBModelDK$PositionId == "301",]
    MLBModelP <- MLBModelDK[MLBModelDK$PositionId == "302",]

    MLBDKOwn <- data.frame(MLBModelDK$Properties.Player_Name,MLBModelDK$Properties.p_own)
    names(MLBDKOwn)<- c("Name","Projected_Ownership")
    MLBDKOwn <- na.zero(MLBDKOwn)
    MLBDKOwn <- MLBDKOwn %>% arrange(desc(Projected_Ownership))



    LabsFixedNames1 <- sapply(MLBModelH,as.character)
    LabsFixedNames2 <- sapply(MLBModelP,as.character)
    LabsFixedNames3 <- sapply(MLBDKOwn,as.character)
    LabsFixedNames3 <- na.zero(LabsFixedNames3)
    write.csv(LabsFixedNames1, file = paste0("~/Desktop/MLB_Daily/","MLB_",modelDate,"_DK_Hitters.csv"))
    write.csv(LabsFixedNames2, file = paste0("~/Desktop/MLB_Daily/","MLB_",modelDate,"_DK_Pitchers.csv"))
    write.csv(LabsFixedNames3, file = paste0("~/Desktop/MLB_Daily/","MLB_",modelDate,"_DKOwn.csv"))



  if(is.null(View)){
    return(LabsFixedNames3)
  }

  if(View == "H"){
    return(LabsFixedNames1)
  }

  if(View == "P"){
    return(LabsFixedNames2)
  }


}
