#' getBBMCookies
#'
#' @return all BBM Cookies
#' @export
#'
#' @examples getBBMCookies()
getBBMCookies<-function(dockerName="monsterLab"){

  require(RSelenium)
  require(wdman)
    system(command=paste0("docker run -d --name ","'",dockerName,"'"," -p 4445:4444 selenium/standalone-firefox"))
  #rd<-rsDriver(browser="firefox",verbose=FALSE)
  #web <- rd[["client"]]
  waitFor(4)
  web<-remoteDriver(port=4445L)
  web$open()
  web$navigate(url="https://basketballmonster.com/login.aspx")
  usernameBox <- web$findElement(using='css selector',"#ContentPlaceHolder1_UsernameTextBox")
  passwordBox <- web$findElement(using='css selector',"#ContentPlaceHolder1_PasswordTextBox")
  usernameBox$sendKeysToElement(list("Benjaminryanclarke"))
  passwordBox$sendKeysToElement(list("tjk589682"))
  web$findElement("css selector", "#ContentPlaceHolder1_LoginButton")$clickElement()
  waitFor(7)
  web$navigate(url="https://basketballmonster.com/daily.aspx")
  bbmCookies <- web$getAllCookies()
  bbmCookies <- data.frame(bbmCookies)
  cookie1 <- bbmCookies$value.3
  cookie2 <- bbmCookies$value
  cookie3 <- bbmCookies$value.2
  cookie4 <- bbmCookies$value.6
  cookie5 <- bbmCookies$value.4
  cookie6 <- bbmCookies$value.5
  cookie7 <- bbmCookies$value.1
  bbmCookie <- paste0("'","Cookie: RotoMonsterUserId=",cookie1,"; ",
                      "ASP.Net_SessionId=",cookie2,"; ",
                      "__utmt=",cookie3,"; ",
                      "__utma=",cookie4,"; ",
                      "__utmb=",cookie5,"; ",
                      "__utmc=",cookie6,"; ",
                      "__utmz=",cookie7,"'")
  web$close()
  #rd[["server"]]$stop()
  #rm(rd)
  system(command=paste0("docker stop ",dockerName))
  system(command=paste0("docker rm ",dockerName))

  return(bbmCookie)
}
