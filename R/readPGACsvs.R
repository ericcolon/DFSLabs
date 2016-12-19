readPGACSVs <- function(modelWeek = NULL){
  csvFile <- read.csv(file = paste0("~/Desktop/PGA_Daily/PGA_Week",modelWeek,".csv"),stringsAsFactors = FALSE)
  return(csvFile)
}