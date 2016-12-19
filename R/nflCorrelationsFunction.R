#' NFL Correlations 
#'
#' @param x 
#'
#' @return correlation DF
#' @export
#'
#' @examples ....
NFLCorrelations <- function(x){
  x.cor <- cor(x[,sapply(x,is.numeric)])
  return(x.cor)
}