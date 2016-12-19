#' Simple Swish rating for a loop
#'
#' @param gDate as yyyy-mm-dd
#'
#' @return a simplified swish rating for hitters
#' @export
#'
#' @examples swishHitterLoop(gDate = "2016-08-12")
swishHittersLoop <- function(gDate = "2016-08-12"){

  remDr <- remoteDriver(browserName = "phantomjs")
  remDr$open()
  remDr$navigate(url = paste0("https://swishanalytics.com/optimus/mlb/batter-comparison?date=",gDate))
  elem <- remDr$findElement(using = "id", value = "stat-table-top")
  elemtxt <- elem$getElementAttribute("outerHTML")[[1]]
  elemxml <- htmlTreeParse(elemtxt, useInternalNodes = T)
  swish <- readHTMLTable(elemxml, stringsAsFactors = FALSE)
  swish.df <- data.frame(swish)
  swishFixedNames <- (swish.df 
                      %>% separate(stat.table.top.Player,into=c("player_Name","getRidOF"),sep="\\(",remove = TRUE) 
                      %>% dplyr::select(-getRidOF))
  remDr$quit()
  return(swishFixedNames)
  
}