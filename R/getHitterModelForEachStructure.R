#' FANTASY labs model retrieve loop 
#'
#' @param modelDate as 4_5_2016 
#' @param id 301/302 hitter/pitcher
#'
#' @return FantasyLabs data frame
#' @export
#'
#' @examples getMlbPlayerModelLoop(modelDate = "8_1_2016", id ="301")
getMlbPlayerModelLoop <- function(modelDate = "8_1_2016", id = "301"){
  
  
  modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/3/",modelDate,"/?modelid=501654")
  y <- paste0("mlb",modelDate,".json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/mlb/player-models/' -H 'Cookie: __distillery=a9f5185_f6f2f8ff-c6e2-4195-8692-9c8d16187164-b489a30ed-a7e7fa52b2a5-42c7; __cfduid=db9b1b49181a8ec11f989911199d472171470029244; LD_U=http%3A%2F%2Fwww.fantasylabs.com%2F; flid=SoXzi2RpgU620nAP_reHpg; LD_R=http%3A%2F%2Fwww.fantasylabs.com%2FMLB%2Ftrends%2F473658%2F; .AspNet.Cookies=pafmAGl0Pf6Ja1IieNRj524a-8-tYWIqu8gfoz5CHHXEVwef4zdO7CnpyomHXIGCmKoTjuKdilrje1e-VMQpR24NEuDtQYzJsX9ORGMX_Fwpe2l77Hmm3gFIMbR2nw8eBlmKTEAHBroa2Sqn0Kv5xbY-Min068OV3oS_Cl_QqLTsttlgC2ZgBVLne0qYMunlvu923C9ZnpN3wfOnq31vfLheLDb0a54mVHaTCueDbjott17QPVYDfKihmKZJ_ku_R0ASpES0r18Pwe--0ONwVnkdaihnCLW0ao4zNMh1jXKrhJcrcSREkG6nBPjPJLqAaXwyJ2LZfCnTsXZqVE5-Bb2wtBqdt9UK_3d9TtuF0HWFH66qbBT6xQBdpzTUKGB68TQEw3Ff52fGOKr9FMM6yCOsxF8PuaiZ_3TXh9eT7yvQ2KgVIOhRtBK4waxV3kKo93y8_maZttiH6ytu5ZvO0tR0wkf58gPoZjM2uL-mpiE; _gat=1; LD_S=1471025812328; __zlcmid=bufk4GxGcd42S2; _ga=GA1.2.940766083.1470029245; LD_T=a722ef7d-b6ba-4451-c097-4133db680240'  -o ",y,"  -H  'Connection: keep-alive' --compressed")
  curlAddress <- paste0("curl ",modelURL," ",pmCurlHandles)
  system(curlAddress)
  mlbModelAll <- jsonlite::fromJSON(y, flatten = TRUE)
  mlbModelDK <- mlbModelAll[mlbModelAll$Properties.SourceId == 4,]
  mlbModel <- mlbModelDK[mlbModelDK$PositionId == id,]
  mlbModel <- arrange(mlbModel,desc(Properties.ActualPoints))
  LabsFixedNames <- (mlbModel 
                     %>% separate(Properties.Player_Name,into=c("player_Name","getRidOF"),sep="\\(",remove = TRUE) 
                     %>% select(-getRidOF))
  LabsFixedNames1 <- sapply(LabsFixedNames,as.character)
  write.csv(LabsFixedNames1, file = paste0(id,modelDate,".csv"))
  return(LabsFixedNames)
}