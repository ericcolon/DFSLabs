#' PGA model Retrieve
#'
#' @param modelDate 
#' @param cookie 
#'
#' @return Fantasylabs PGA model
#' @export
#'
#' @examples getPGAPlayerModeL()
getPGAPlayerModel <- function(modelYear = "2015", modelWeek = "2015Frys", singleDate = "10_18_2015",cookie = "__cfduid=d19da73f21ae9852e137438d8ffd398371475796569; LD_U=http%3A%2F%2Fwww.fantasylabs.com%2F; .AspNet.Cookies=4OqHDcdGDsIxvmTJlDZP4l5WDNavz_EqYieZYNQ0jwz5D4hdcpnW81hhl10guRKYUJ4FUaeWfGILkrlHYAc5iAfkM78TCuId4C3Msk1UXmSOrCRGtWWf8LD2PvJVzmQDYuhLPJxChRIyLrl1J_wmMfFVG1GmepgwOUhW6njdFz7iNy7KAp-QjTnS7IQs20QabkcogIBaqqNQDyuNHD-7oeljqkv1ZDJq6CuGlLxe_8VmH8IX9kEtk8WM2GWATZaahPx09bCoYiNuWcydMTVG2vEOxBjy_obw7EgcJaQ3L8BjTRtfezcSo3_zRaSbSd8hlzFdC3gXQjm5x3H48o68HzJ6Moj3FjwIiAE7cv2KOoZ8GqJBgdD-aSfYangaMtwkgSlO4wYb2eOiLkjZojaBf-nVARxa89E293Fan6i4dZXyCTetik94qijmISHokbYG01lOQUoEPWjm07GsoqWvEp6AVplqN5DVHnqiWC23e54; flid=EriUJhQuj0yimw3ajUgOew; __distillery=a88774e_344d8f47-a8f7-4094-81ba-d3336d4c2d4d-ba08c6bef-19ba082fafba-d101; LD_R=http%3A%2F%2Fwww.fantasylabs.com%2Fnfl%2Fplayer-models%2F; __zlcmid=cyfuDnxjy8XwIK; LD_S=1476204941088; _gat=1; LD_T=6a763f78-d230-4827-f05d-37ca01630a5d; _ga=GA1.2.407536382.1475796573'"){
  week2016 <- as.POSIXct("2015-10-25 23:12:13", tz = "Etc/GMT+8")
  allWeeks2016 <- week2016 + weeks(0:45)
  dates2016 <- format(allWeeks2016, format="%m_%d_%Y")
  modelWeekDates2015 <- c(dates2016)
  modelDate <- singleDate
    if(is.null(singleDate)){
    if(modelYear == "2015"){
    if(is.null(modelWeek)){
      modelWeek <- readline("2015, Week 1-17?: ")
    }
    modelDate = modelWeekDates2015[modelWeek]
    }}
  
  

  modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/5/",modelDate,"/?modelid=466780")
  y <- paste0("PGA",modelDate,".json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/pga/player-models/' -H 'Cookie: ",cookie,"  -o ",y," -H 'Connection: keep-alive' --compressed")
  curlAddress <- paste0("curl ",modelURL," ",pmCurlHandles)
  system(curlAddress)
  pgaModelAll <- jsonlite::fromJSON(y, flatten = TRUE)
  pgaModelDK <- pgaModelAll$PlayerModels[pgaModelAll$PlayerModels$Properties.SourceId == 4,]
  #pgaModel <- pgaModelDK[pgaModelDK$Position == id,]
  
  
  LabsFixedNames1 <- sapply(pgaModelDK,as.character)
  write.csv(LabsFixedNames1,file = paste0("~/Desktop/PGA_Daily/PGA_Week",modelWeek,".csv"))
  return(pgaModelDK)
}