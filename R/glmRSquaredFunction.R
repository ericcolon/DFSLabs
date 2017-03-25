#' GLM regression correlation r^2 value
#'
#' @param fitModel glm Model Name i.e:nYearGLM3
#'
#' @return r^2 correlation of regression model
#' @export
#'
#' @examples glmRSquared(fitModel=nYearGLM3)
glmRSquared <- function(fitModel=NULL){
  r_squared <- 1 - fitModel$deviance/fitModel$null.deviance
  return(r_squared)
}
