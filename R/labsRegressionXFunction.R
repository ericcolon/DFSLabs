#' Labs Regression X
#'
#' @param labModel labs Model to fit data (X)
#'
#' @return labsModel prepared for linear regression fitting
#' @export
#'
#' @examples labsRegressionX(nbaAll)
labsRegressionX <- function(labModel=NULL,fromReg=NULL){
  require(dplyr)
  if(is.null(fromReg)){
  labX <- labModel %>% select(-X,-Properties.ActualPoints,-ActualPoints)
  }
  if(!is.null(fromReg)){
      labX <- labModel[,-1]
  }
return(labX)
}
