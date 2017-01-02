#' Read NHL csv saved Models
#'
#' @param modelDate "1_1_2017"
#' @param modelPosition "ALL","S","G"... Both,Skaters Only,Goalies Only..
#'
#' @return pulls model into r
#' @export
#'
#' @examples nhl12_23_2016 <- readNHLCSVs(modelDate = "12_23_2016",modelPosition="All")
readNHLCSVs <- function(modelDate = "12_23_2016",modelPosition="All"){
  csvFile <- read.csv(file = paste0("~/Desktop/nhl_Daily/",modelDate,modelPosition,"NHL",".csv"))
  return(csvFile)
}
