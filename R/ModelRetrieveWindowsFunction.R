#' NFL model for Windows
#'
#' @param userName 
#' @param passWord 
#' @param modelYear 
#' @param modelWeek 
#' @param modelPosition 
#'
#' @return NFLplayerModel
#' @export 
#'
#' @examples NFLQB2016Wk1 <- windowsNFLModel(userName = "Benjaminryanshopping@icloud.com",passWord = "W3ytjk589682",modelYear = "2016",modelWeek = 1,modelPosition = "QB")
windowsNFLModel <- function(userName = "Benjaminryanshopping@icloud.com",passWord = "W3ytjk589682",modelYear = "2016",modelWeek = 1,modelPosition = "QB"){
pJS <- phantom()
week2016 <- as.POSIXct("2016-09-07 23:12:13", tz = "Etc/GMT+8")
allWeeks2016 <- week2016 + weeks(0:45)
dates2016 <- format(allWeeks2016, format="%m_%d_%Y")
week2015 <- as.POSIXct("2015-09-09 23:12:13", tz = "Etc/GMT+8")
allWeeks2015 <- week2015 + weeks(0:17)
dates2015 <- format(allWeeks2015, format="%m_%d_%Y")
ModelYear <- modelYear
modelDate <- dates2016[modelWeek]
  if(ModelYear == "2015"){
  modelDate <- dates2015[modelWeek]
}
#Start webDriver
web <- remoteDriver(browserName = 'phantomjs')
web$open()
Sys.sleep(3)

#Goto login menu
web$navigate(url = "http://www.fantasylabs.com/account/login/")
Sys.sleep(3)

#Enter login info
nameBox <- web$findElement("name","input")
pwBox <- web$findElement("name","password")
nameBox$sendKeysToElement(list(userName))
pwBox$sendKeysToElement(list(passWord))
submitButton <- web$findElement("xpath","/html/body/div[3]/form[1]/div[4]/button")
#Submit Login
submitButton$clickElement()
Sys.sleep(5)

#Goto playerModel
web$navigate(paste0("http://www.fantasylabs.com/api/playermodel/1/",modelDate,"/?modelId=519987"))
Sys.sleep(5)
modelElem <- web$findElement("tag name","body")

#Convert JSON to dataFrame
jsonToConvert <- modelElem$getElementText()
fullPlayerModel <- jsonlite::fromJSON(jsonToConvert[[1]], flatten = TRUE)
model.df <- data.frame(lapply(fullPlayerModel$PlayerModels, as.character), stringsAsFactors=FALSE)

#Get only DraftKings data from playerModel
model.df.dkOnly <- model.df %>% filter(Properties.SourceId == 4)

#Get only DraftKings position data
model.df.dkOnly.modelPosition <- model.df.dkOnly %>% filter(Properties.Position == modelPosition)
#Save as csv
write.csv(model.df.dkOnly.modelPosition, file = paste0("NFLPlayerModelDK",modelPosition,modelYear,"Week",modelWeek,".csv"))

web$quit()
pJS$stop()

return(model.df.dkOnly.modelPosition)
}