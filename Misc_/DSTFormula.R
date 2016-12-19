j <- 10
AllDST <- foreach(i=1:17) %dopar% getNflLabModel("2015",i,PositionID = "DST")
readDST <- foreach(i=1:17) %dopar% readNFLCSVs("2015",i,PositionID = "DST")
AllDST16<- foreach(i=1:j) %dopar% getNflLabModel("2016",i,PositionID = "DST")
readDST16 <- foreach(i=1:j) %dopar% readNFLCSVs("2016",i,PositionID = "DST")
DST <- bind_rows(readDST16)#,readDST)

#week <- 2

DSTModel <- getNflLabModel("2016",week,PositionID = "DST")
DSTModel <- na.zero(readNFLCSVs("2016",week,PositionID = "DST"))
DSTnums <- na.zero(DST[,sapply(DST,is.numeric)])
DSTnums <- DSTnums %>% select(-X,-Properties.ActualPoints)
yDST <- DSTnums$ActualPoints
xDST <- DSTnums %>% select(-ActualPoints)
                                                     

DSTFit <- lm(yDST~.,data = xDST)
summary(DSTFit)
DSTFit <- step(DSTFit)
summary(DSTFit)
#addToPred <- min(DSTFit$residuals)
DSTP <- predict(DSTFit,DSTModel)
#DSTP <- DSTP-addToPred
DSTFile <- data.frame(cbind(DSTModel$Properties.Player_Name,DSTP))
DSTFile <- DSTFile %>% dplyr::arrange(V1) 
V1 <- c("Arizona Defense","Atlanta Defense","Baltimore Defense","Buffalo Defense","Carolina Defense","Chicago Defense","Cincinnati Defense","Cleveland Defense","Dallas Defense","Denver Defense","Detroit Defense","Green Bay Defense","Houston Defense","Jacksonville Defense","Kansas City Defense","Indianapolis Defense","LA Rams Defense","Miami Defense","Minnesota Defense","New England Defense","New Orleans Defense","NY Giants Defense","NY Jets Defense","Oakland Defense","Philadelphia Defense","Pittsburgh Defense","San Diego Defense","San Francisco Defense","Seatle Defense","Tampa Bay Defense","Tennessee Defense","Washington Defense") 
cruncherName1 <- c('Cardinals','Falcons','Ravens','Bills','Panthers','Bears','Bengals','Browns','Cowboys','Broncos','Lions','Packers','Texans','Jaguars','Chiefs','Colts','Rams','Dolphins','Vikings','Patriots','Saints','Giants','Jets','Raiders','Eagles','Steelers','Chargers','49ers','Seahawks','Buccaneers','Titans','Redskins')
dstNames <- data.frame(V1,cruncherName1,stringsAsFactors = FALSE)
DSTFile <- inner_join(DSTFile,dstNames,by="V1")
DSTFile <- DSTFile %>% select(cruncherName1,DSTP)
names(DSTFile)=list("V1","PP")
write.csv(DSTFile, file = paste0("DSTP",week,".csv"))
DSTFile

#CombineAllProjections
QBFile<- data.frame(QBFile)
RBFile<- data.frame(RBFile)
WRFile<- data.frame(WRFile)
TEFile<- data.frame(TEFile)
DSTFile<- data.frame(DSTFile)
names(QBFile)<-list("V1","PP")
names(RBFile)<-list("V1","PP")
names(WRFile)<-list("V1","PP")
names(TEFile)<-list("V1","PP")
names(DSTFile)<-list("V1","PP")

AllProj <- bind_rows(QBFile,RBFile,WRFile,TEFile,DSTFile)
write.csv(AllProj, file = paste0("~/Desktop/NFL_Daily/NFL_2016_Week",j,"/AllProjections",week,".csv"))
