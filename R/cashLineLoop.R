#' CashLine function for multiple iterations
#'
#' @param urlDate 
#'
#' @return cashLines for all slates
#' @export
#'
#' @examples getCashLineLoop(urlDate = "aug-i-2016")
getCashLineLoop <- function(urlDate = "aug-12-2016") {
  
  goldGet <- httpGET(paste0("http://www.dfsgold.com/mlb/daily-fantasy-recap-draftkings-",urlDate))
  gold <- readHTMLTable(goldGet , stingsAsFactors = FALSE)
  goldTable <- data.frame(gold$MainContent_GridView3, stringsAsFactors = FALSE)
  
  goldView <- goldTable %>% arrange(Time) %>% dplyr::select(-Name,-Entries,-Prize.Pool,-Winner,-Total.Entries) 
  return(goldView)
}