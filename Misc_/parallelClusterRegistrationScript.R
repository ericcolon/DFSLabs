#register foreach parallel backend
library(doParallel)
myCluster = makeCluster(6)
registerDoParallel(myCluster)
clusterCall(myCluster, function() library(magrittr,dplyr))

#unregister parallel backend
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
