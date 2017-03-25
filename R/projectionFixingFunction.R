#' Projections formatting for dataFrames from labs for NBA Predictions
#'
#' @param df dataframe
#'
#' @return formatted for use with predict()
#' @export
#'
#' @examples projectionFixing(labsAll)
projectionFixing <- function(df = labsAll){
  indx <- sapply(df, is.factor)
  df[indx] <- lapply(df[indx], function(x) as.numeric(as.character(x)))
  return(df)
}
