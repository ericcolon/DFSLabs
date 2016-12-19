#' Simple function obtains All Factors Rated modeling of Pitchers for desired date
#'
#' @param Date M_D_YYYY 
#'
#' @return saved csv file of the model
#' @export
#'
#' @examples AllFactorsRatedModelPitchers(Date="8_9_2016")
AllFactorsRatedModelPitchers <- function(Date ="8_9_2016"){
  PitchersAllFactorsRatedDate <- getMlbPlayerModel()
  
  write.csv(PitchersAllFactorsRatedDate, file=paste0("/users/Ben/Desktop/MLB_R_Data/PitchersAllFactorsRated",Date,".csv"))
}