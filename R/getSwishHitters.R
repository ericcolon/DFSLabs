#' Retreive the batter comparison table from Swish Analytics
#'
#' Runs as an interactive function asking for a Date and returns the Swish table as a data frame.
#'
#' @return swish data frame
#' @export
#'
#' @examples swishHitters()
swishHitters <- function(){
  gDate <- readline("Enter the date as YYYY-MM-DD:")
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
  %>% select(-getRidOF))
  
  return(swishFixedNames)
  }
