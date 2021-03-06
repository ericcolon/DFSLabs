#' getLabsCookies
#'
#' @param dockerName docker container name(can be anything)
#' @param sport mlb,nba,nfl,nhl,pga
#'
#' @return all FantasyLab Cookies
#' @export
#'
#' @examples getLabsCookies("labCookies","mlb")
getLabsCookies<-function(dockerName="labCookies",sport="mlb"){

  require(RSelenium)
  require(wdman)
  system(command=paste0("docker run -d --name ","'",dockerName,"'"," -p 4445:4444 selenium/standalone-firefox"))
  #rd<-rsDriver(browser="firefox",verbose=FALSE)
  #web <- rd[["client"]]
  waitFor(3)
  web<-remoteDriver(port=4445L)
  web$open()
  web$navigate(url="https://fantasylabs.com/account/login")
  usernameBox <- web$findElement(using='xpath',"/html/body/div[3]/form[1]/div[2]/div/input")
  passwordBox <- web$findElement(using='xpath',"/html/body/div[3]/form[1]/div[3]/div/input")
  usernameBox$sendKeysToElement(list("Benjaminryanshopping@icloud.com"))
  passwordBox$sendKeysToElement(list("W3ytjk589682"))
  web$findElement("xpath", "/html/body/div[3]/form[1]/div[4]/button")$clickElement()
  waitFor(5)
  web$navigate(url=paste0("http://www.fantasylabs.com/",sport,"/player-models/"))
  waitFor(8)
  LabsCookies <- web$getAllCookies()
  LabsCookies <- data.frame(LabsCookies)
  cookie0 <- LabsCookies$value
  cookie1 <- LabsCookies$value.1
  cookie2 <- LabsCookies$value.2
  cookie3 <- LabsCookies$value.3
  cookie4 <- LabsCookies$value.4
  cookie5 <- LabsCookies$value.5
  cookie6 <- LabsCookies$value.6
  cookie7 <- LabsCookies$value.7
  cookie8 <- LabsCookies$value.8
  cookie9 <- LabsCookies$value.9
  cookie10 <- LabsCookies$value.10

  name0 <- LabsCookies$name
  name1 <- LabsCookies$name.1
  name2 <- LabsCookies$name.2
  name3 <- LabsCookies$name.3
  name4 <- LabsCookies$name.4
  name5 <- LabsCookies$name.5
  name6 <- LabsCookies$name.6
  name7 <- LabsCookies$name.7
  name8 <- LabsCookies$name.8
  name9 <- LabsCookies$name.9
  name10 <- LabsCookies$name.10
  labsCookie <- paste0("__cfduid","=",cookie1,"; ",
                      "LD_U=",cookie7,"; ",
                      "LD_R=",cookie5,"; ",
                      "_gat=",cookie4,"; ",
                      ".AspNet.Cookies=",cookie3,"; ",
                      "flid=",cookie0,"; ",
                      "__zlcmid=",cookie10,"; ",
                      "__distillery=",cookie9,"; ",
                      "_ga=",cookie6,"; ",
                      "LD_T=",cookie8,"'")
  #labsCookie <- paste0("'",name0,"=",cookie0,"; ",
                       #name1,"=",cookie1,"; ",
                       #name2,"=",cookie2,"; ",
                       #name4,"=",cookie4,"; ",
                       #name5,"=",cookie5,"; ",
                       #name6,"=",cookie6,"; ",
                       #name7,"=",cookie7,"; ",
                       #name8,"=",cookie8,"; ",
                       #name9,"=",cookie9,"; ",
                       #name10,"=",cookie10,"'")

  web$close()
  #rd[["server"]]$stop()
  #rm(rd)
  system(command=paste0("docker stop ",dockerName))
  system(command=paste0("docker rm ",dockerName))

  return(labsCookie)
}
