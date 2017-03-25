#' Labs Regression Y
#'
#' @param labModel model to get Y values from for linear regression fitting
#'
#' @return regression Y vector for fitting labModels
#' @export
#'
#' @examples labsRegressionY(nbaAll)
labsRegressionY <- function(labModel=NULL,fromReg=NULL){
  if(is.null(fromReg)){
  labY <- labModel$ActualPoints
  }
  if(!is.null(fromReg)){
    labY <- labModel[,1]
  }
  return(labY)
}
