#' read NBA csv saved Models
#'
#' @param modelDate 
#'
#' @return pulls model into r
#' @export
#'
#' @examples nbaModel <- readNBACSVs(modelDate = "2015")
readNBACSVs <- function(modelDate){
  csvFile <- read.csv(file = paste0("~/Desktop/NBA_Daily/",modelDate,".csv"),stringsAsFactors = FALSE)
  return(csvFile)
}