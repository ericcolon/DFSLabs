bbmOct <- foreach(i=25:30) %dopar% readxl::read_excel(path=paste0("~/Desktop/NBA_Daily/BBMonster10_",i,"_2016.xls"))
bbmNov <- foreach(i=1:23) %dopar% readxl::read_excel(path=paste0("~/Desktop/NBA_Daily/BBMonster11_",i,"_2016.xls"))
bbmNov2 <- foreach(i=25:30) %dopar% readxl::read_excel(path=paste0("~/Desktop/NBA_Daily/BBMonster11_",i,"_2016.xls"))
bbmDec <- foreach(i=1:23) %dopar% readxl::read_excel(path=paste0("~/Desktop/NBA_Daily/BBMonster12_",i,"_2016.xls"))
bbmDec2 <- foreach(i=25:30) %dopar% readxl::read_excel(path=paste0("~/Desktop/NBA_Daily/BBMonster12_",i,"_2016.xls"))
bbmJan <- foreach(i=1:27) %dopar% readxl::read_excel(path=paste0("~/Desktop/NBA_Daily/BBMonster1_",i,"_2017.xls"))
bbmAll <- bind_rows(bbmOct,bbmNov,bbmNov2,bbmDec,bbmDec2,bbmJan)
labsOct <- foreach(i=25:31) %dopar% read.csv(file=paste0("~/Desktop/NBA_Daily/10_",i,"_2016.csv"))
labsNov <- foreach(i=1:23) %dopar% read.csv(file=paste0("~/Desktop/NBA_Daily/11_",i,"_2016.csv"))
labsNov2 <- foreach(i=25:30) %dopar% read.csv(file=paste0("~/Desktop/NBA_Daily/11_",i,"_2016.csv"))
labsDec <- foreach(i=1:23) %dopar% read.csv(file=paste0("~/Desktop/NBA_Daily/12_",i,"_2016.csv"))
labsDec2 <- foreach(i=25:31) %dopar% read.csv(file=paste0("~/Desktop/NBA_Daily/12_",i,"_2016.csv"))
labsJan <- foreach(i=1:31) %dopar% read.csv(file=paste0("~/Desktop/NBA_Daily/1_",i,"_2017.csv"))
labsFeb <- foreach(i=1:10) %dopar% read.csv(file=paste0("~/Desktop/NBA_Daily/2_",i,"_2017.csv"))
labsAll<- bind_rows(labsOct,labsNov,labsNov2,labsDec,labsDec2,labsJan,labsFeb)
#convert all factor columns to numeric for regression analysis
indx <- sapply(labsAll, is.factor)
labsAll[indx] <- lapply(labsAll[indx], function(x) as.numeric(as.character(x)))
labsAll <- na.zero(labsAll)
#get columns of certain types(character/numeric)subsetting....
allCols <- names(labsAll)
numCols <- labsAll[sapply(labsAll,is.numeric)] #<-----all numeric columbs dataframe
num.Cols <- c(names(numCols))
char.cols <- allCols[!allCols %in% num.Cols]

allStarX <- numCols %>% select(-ActualPoints,-X,-Properties.ActualPoints,-Properties.MyTrends.custom)
allstarY <- numCols$ActualPoints

