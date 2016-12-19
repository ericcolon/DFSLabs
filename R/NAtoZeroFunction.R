#' NA to Zero
#'
#' @param x 
#'
#' @return no NA Dataframe x
#' @export
#'
#' @examples na.zero(x)
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}