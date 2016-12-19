#' NFL CashLine Contest Data
#'
#' @param i: week for data
#'
#' @return Dk NFL cash line and winning scores; saves as .csv for later use as well
#' @export
#'
#' @examples nflWeek2Cashlines <- dfsGoldNflRecap(2)
dfsGoldNflRecap <- function(i,year = "2016"){
tableget <- read_html(paste0("http://www.dfsgold.com/nfl/",year,"-week-",i,"-NFL-fantasy-recap-draftkings-6822"))
topLineup <- html_table(tableget)[[1]]
cashLines <- html_table(tableget)[[2]]
playerData <- html_table(tableget)[[3]]
write.csv(topLineup,file=paste0("~/Desktop/NFL_Daily/NFL_",year,"_Week",i,"/",year,"Week",i,"topLineup.csv"))
write.csv(cashLines,file=paste0("~/Desktop/NFL_Daily/NFL_",year,"_Week",i,"/",year,"Week",i,"cashLines.csv"))
write.csv(playerData,file=paste0("~/Desktop/NFL_Daily/NFL_",year,"_Week",i,"/",year,"Weel",i,"playerData.csv"))
return(list(topLineup,cashLines,playerData))
}