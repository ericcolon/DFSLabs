#' Get BBM Data easier
#'
#' @return bbm df/csv
#' @export
#'
#' @examples getBBMModel2()
getBBMModel2 <- function(){
  fn = file.choose()
  bbmTable <- gdata::read.xls(fn)
  return(bbmTable)
}
