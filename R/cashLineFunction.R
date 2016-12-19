#' Function to lookup past cash lines(DK)
#'
#' @return all slate cash lines for given date
#' @export
#'
#' @examples getCashLine()
getCashLine <- function() {
urlDate <- readline("Enter date as mon(first3letters)-dd-yyyy : ")
goldGet <- httpGET(paste0("http://www.dfsgold.com/mlb/daily-fantasy-recap-draftkings-",urlDate))
gold <- readHTMLTable(goldGet , stingsAsFactors = FALSE)
goldTable <- data.frame(gold$MainContent_GridView3, stringsAsFactors = FALSE)

goldView <- goldTable %>% arrange(Time) %>% dplyr::select(-Name,-Entries,-Prize.Pool,-Winner,-Total.Entries) 
return(goldView)
}