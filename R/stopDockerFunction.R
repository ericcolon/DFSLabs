#' stop Docker
#'
#' @param dID name of running Docker session to stop "distracted_snyder"
#'
#' @return NULL
#' @export
#'
#' @examples stopDocker("distracted_snyder")
stopDocker <- function(dID=NULL){
  system(command=paste0("docker stop ",dID))
  }
