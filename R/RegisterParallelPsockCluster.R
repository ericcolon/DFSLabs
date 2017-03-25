#' REGISTER PARALLEL PROCESSOR(PSOCKS)
#'
#' @param cores # OF CORES "2"
#'
#' @return NAME OF REGEISTERED PARALLEL PROCESSOR
#' @export
#'
#' @examples parallelPSock(2)
parallelPSock <- function(cores=2){
pSockCluster <- parallel::makePSOCKcluster(cores)
doParallel::registerDoParallel(pSockCluster)
doPName<- foreach::getDoParName()
return(doPName)
}
