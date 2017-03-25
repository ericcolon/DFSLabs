#' kill running Docker
#'
#' @return docker Info
#' @export
#'
#' @examples dockerID()
killDocker <- function(container=NULL){
  dID <- system(command=paste0("docker kill ",container))
  return(dID)
}
