#' Full NFL models from CSV function
#'
#' @param week 
#'
#' @return nfl data frame labs model(FULL)
#' @export
#'
#' @examples fullNFLModelFromCSV(week)
fullNFLModelFromCSV <- function(week){
  
qb <- read.csv(file = paste0("~/Desktop/NFL_Daily/NFL_2016_Week",week,"/NFL2016Week",week,"QB.csv"),stringsAsFactors = FALSE)
rb <- read.csv(file = paste0("~/Desktop/NFL_Daily/NFL_2016_Week",week,"/NFL2016Week",week,"RB.csv"),stringsAsFactors = FALSE)
wr <- read.csv(file = paste0("~/Desktop/NFL_Daily/NFL_2016_Week",week,"/NFL2016Week",week,"WR.csv"),stringsAsFactors = FALSE)
te <- read.csv(file = paste0("~/Desktop/NFL_Daily/NFL_2016_Week",week,"/NFL2016Week",week,"TE.csv"),stringsAsFactors = FALSE)
dst <- read.csv(file = paste0("~/Desktop/NFL_Daily/NFL_2016_Week",week,"/NFL2016Week",week,"DST.csv"),stringsAsFactors = FALSE)

all <- bind_rows(qb,rb,wr,te,dst)
write.csv(all, file = paste0("~/Desktop/NFL_Daily/NFL_2016_Week",week,"/NFL2016Week",week,"AllPositions.csv"))
return(all)

}