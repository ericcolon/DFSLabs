#' nhl Model Retrieve
#'
#' @param modelDate
#' @param cookie
#'
#' @return Fantasylabs nhl model
#' @export
#'
#' @examples getNhlPlayerModel()
#'


getNhlPlayerModel <- function(modelDate = "12_23_2016", cookie = "__cfduid=d46b64495b55215841566c89ce01d6fbd1480519747; LD_U=http%3A%2F%2Fwww.fantasylabs.com%2F; __distillery=bb3e53d_b333f0ea-b6ab-4d8b-bcbb-e8f78b8d5c3f-86a4ffd60-c4a9ff71f0fe-aef4; LD_S=1483050568679; LD_R=; _gat=1; .AspNet.Cookies=y5EwFMncNLomeFSafp9SIE7fCvJBgJX7e7NEzShGZwzkrAV2GpihUEdVaavtghwRaQAexBI_oHlDKO1hi3BXQUbyWlS9F9Vl6nduqcs0aJ2pjEhtahx_Lww_by4_RhWOSIoPc62aqDWdAJ7XXncYhb7PNk8kTE_T44WqWADL5Qc5LB3upqEk20amdTdHK8sWKFgaNs054FSxnwjki6pLxBQ_4IehZ2uo_8XqBaLYQSfWPiDKZZXY93CUIHRuTB6Vk6YqGdvTqX-lqDCu2MjdNwvwcnbRs8xMD3veXuvlUPADlzUv6UY4E1lQ942M0BJRK-n1Vs1rBcB7ItL0fogJ-mMC-ZQc_AYJo_uG3Pa5mWHWnPmmFHlS0goRDs9zUTa4SPiPtmCReo14fgRdS0YdQZWUWd0uy9qwSGEgS3cjK0xx3EB5VdM7U-YfYG7OCOPCEnz6mqVWJPxdji_ckRWZPxWVUuiOy8nhihJbqgzoWNY; flid=9FKN218a_UamhblGysQA7Q; __zlcmid=e6g4jX86CRpGap; _ga=GA1.2.351079370.1480519759; LD_T=d014f701-2492-4486-bcec-b421e46d54b2'"){
    modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/4/",modelDate,"/?modelid=794374")
    y <- paste0("nhl",modelDate,".json")
    pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nhl/player-models/' -H 'Cookie: ",cookie,"  -o ",y," -H 'Connection: keep-alive' --compressed")
    curlAddress <- paste0("curl ",modelURL," ",pmCurlHandles)
    system(curlAddress)
    nhlModelAll <- jsonlite::fromJSON(y, flatten = TRUE)
    nhlModelDK <- nhlModelAll$PlayerModels[nhlModelAll$PlayerModels$Properties.SourceId == 4,]
    #nhlModel <- nhlModelDK[nhlModelDK$Position == id,]
    nhlModelG <- nhlModelDK[nhlModelDK$Properties.PositionId == 402,]
    nhlModelC <- nhlModelDK[nhlModelDK$Properties.Position == "C",]
    nhlModelW <- nhlModelDK[nhlModelDK$Properties.Position == "w",]
    nhlModelD <- nhlModelDK[nhlModelDK$Properties.Position == "D",]
    nhlModelS <- nhlModelDK[nhlModelDK$Properties.PositionId == 401,]



    LabsFixedNames1 <- sapply(nhlModelDK,as.character)
    LabsFixedNames2 <- sapply(nhlModelG,as.character)
    LabsFixedNames3 <- sapply(nhlModelC,as.character)
    LabsFixedNames4 <- sapply(nhlModelW,as.character)
    LabsFixedNames5 <- sapply(nhlModelD,as.character)
    LabsFixedNames6 <- sapply(nhlModelS,as.character)

    write.csv(LabsFixedNames1, file = paste0("~/Desktop/nhl_Daily/",modelDate,"All","NHL",".csv"))
    write.csv(LabsFixedNames2, file = paste0("~/Desktop/nhl_Daily/",modelDate,"G","NHL",".csv"))
    write.csv(LabsFixedNames3, file = paste0("~/Desktop/nhl_Daily/",modelDate,"C","NHL",".csv"))
    write.csv(LabsFixedNames4, file = paste0("~/Desktop/nhl_Daily/",modelDate,"W","NHL",".csv"))
    write.csv(LabsFixedNames5, file = paste0("~/Desktop/nhl_Daily/",modelDate,"D","NHL",".csv"))
    write.csv(LabsFixedNames6, file = paste0("~/Desktop/nhl_Daily/",modelDate,"S","NHL",".csv"))


    return(LabsFixedNames1)

}
