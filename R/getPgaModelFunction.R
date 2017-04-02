#' PGA FantasyLabs Model Retrieve
#'
#' @param modelDate "4_2_2017"
#' @param cookie labCookies
#'
#' @return Fantasylabs PGA player Model
#' @export
#'
#' @examples getpgaModel("4_2_2017",labCookies)
getPgaModel <- function(modelDate=NULL, cookie=labCookies){

  modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/5/",modelDate,"/?modelid=607147")
  y <- paste0("PGA",modelDate,".json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/pga/player-models/' -H 'Cookie: ",cookie,"  -o ",y," -H 'Connection: keep-alive' --compressed")
  curlAddress <- paste0("curl ",modelURL," ",pmCurlHandles)
  system(curlAddress)
  pgaModelAll <- jsonlite::fromJSON(y, flatten = TRUE)
  pgaModelDK <- pgaModelAll$PlayerModels[pgaModelAll$PlayerModels$Properties.SourceId == 4,]

  pgaDKOwn <- data.frame(pgaModelDK$Properties.Player_Name,pgaModelDK$Properties.p_own)
  names(pgaDKOwn)<- c("Name","Projected_Ownership")
  pgaDKOwn <- na.zero(pgaDKOwn)
  pgaDKOwn <- pgaDKOwn %>% arrange(desc(Projected_Ownership))



  LabsFixedNames1 <- sapply(pgaModelDK,as.character)
  write.csv(LabsFixedNames1, file = paste0("~/Desktop/pga_Daily/","pga",modelDate,".csv"))

  return(LabsFixedNames1)


}
