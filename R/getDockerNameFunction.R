#' get Docker Name
#'
#' @return docker Info
#' @export
#'
#' @examples dockerID()
dockerID <- function(){
  dID <- system(command="docker ps -a")
  return(dID)
}
