library(jsonlite)
library(RCurl)

#' Fantasy Labs MLB model importing function
#' 
#' Import FantasyLabs MLB player model as a data frame; interactively input the date and a selection of hitters or pitchers.
#' 
#' @return The model as a data frame
#' @export
#'
#' @examples getMlbPlayerModel()
getMlbPlayerModel <- function(){
  modelDate <- readline("Enter the model date ie. 7_5_2016:")
  id <- readline("Enter 301 for hitters or 302 for pitchers:")
  modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/3/",modelDate,"/?modelid=501654")
  y <- paste0("mlb",modelDate,".json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/mlb/player-models/' -H 'Cookie: __distillery=a9f5185_f6f2f8ff-c6e2-4195-8692-9c8d16187164-b489a30ed-a7e7fa52b2a5-42c7; __cfduid=db9b1b49181a8ec11f989911199d472171470029244; LD_U=http%3A%2F%2Fwww.fantasylabs.com%2F; LD_R=http%3A%2F%2Fwww.fantasylabs.com%2FMLB%2Ftrends%2F473658%2F; flid=EUrsOK-wd0WjwXvdf65RNg; __zlcmid=bufk4GxGcd42S2; _gat=1; LD_S=1474072842176; .AspNet.Cookies=-s1cP17cRQBj2pFPQRj7OaX6Hx0WNo3dOuK5DH4pr3iWk37zC835wF_P9ImFrzfcpFVzBssxjA6-myN2-oXK-a4vmd_xab9Nvvb9EZp0V38tX4zAh21xXdZXTci89_B74DhfH6GwIr8CL5PlmWRulKtmK9Yk17BotuVINMB9TccS3rTbLaJXltGn7wVH7LbFUSHSG4ZzL7MwDQCB8FMxP8Rrpg78X_Vrk71ybHiRVatT0i4bGQns2qcVDZo1l5Hh-piObcXnt_Ee958z4eWtHrzQXK6vRhIBNEeCXKWkvNEI7YN5U09ZiLEh-oUNxa_bEHDwygfdjXSBuf0VMKkZsnWoB06M2RNJ3wVMkwYLj-DM2DMCLHghf5aqfyJD5sR-QbxW66CcAtPxQhUeQRRvBolLxuM73zoYBd9Ya5tChgWlTC79tFrHO71DL6d4_koAE1O-3vVowF_GWMcKlLbfaXPnkwypRgKz4HeuvehK_Hc; LD_T=a722ef7d-b6ba-4451-c097-4133db680240; _ga=GA1.2.940766083.1470029245' -o ",y," -H  'Connection: keep-alive' --compressed")
  curlAddress <- paste0("curl ",modelURL," ",pmCurlHandles)
  system(curlAddress)
  mlbModelAll <- jsonlite::fromJSON(y, flatten = TRUE)
  mlbModelDK <- mlbModelAll$PlayerModels[mlbModelAll$PlayerModels$Properties.SourceId == 4,]
  mlbModel <- mlbModelDK[mlbModelDK$PositionId == id,]
  
  LabsFixedNames <- (mlbModel 
                      %>% separate(Properties.Player_Name,into=c("player_Name","getRidOF"),sep="\\(",remove = TRUE)
                      %>% dplyr::select(-getRidOF))
  LabsFixedNames1 <- sapply(LabsFixedNames,as.character)
  write.csv(LabsFixedNames1, file = paste0(id,modelDate,".csv"))
  return(LabsFixedNames)
}