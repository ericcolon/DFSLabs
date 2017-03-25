#Setup Parameters____________________
week <- 20
labsModel <- ""
site <- "Draftkings"
#Register Parallel Processor
pSockCluster <- parallel::makePSOCKcluster(2)
doParallel::registerDoParallel(pSockCluster)
foreach::getDoParName()
#____________________________________QB(FanDuel)
foreach(i=1:17) %dopar% DFSLabs::getNflLabModel("2015",modelSite=site,modelWeek=i,mod=labsModel,PositionID="QB")
foreach(i=1:19) %dopar% DFSLabs::getNflLabModel("2016",modelSite=site,modelWeek=i,mod=labsModel,PositionID="QB")

readQB16 <- foreach(i=1:19) %dopar% DFSLabs::readNFLCSVs("2016",modelSite=site,modelWeek=i,modelId=labsModel,PositionID = "QB")
readQB15 <- foreach(i=1:17) %dopar% DFSLabs::readNFLCSVs("2015",modelSite=site,modelWeek=i,modelId=labsModel,PositionID = "QB")
QB <- bind_rows(readQB15,readQB16)
#QB <- bind_rows(readQB16)


QBModel <- DFSLabs::getNflLabModel("2016",modelSite=site,modelWeek=week,PositionID = "QB")
QBModel <- na.zero(DFSLabs::readNFLCSVs("2016",modelSite=site,modelWeek=week,PositionID = "QB"))
QBModel <- na.zero(QBModel)
nfl_QBOwnP <- data.frame(c(QBModel$Properties.OwnRank))
QBX <- QB %>% select(-Properties.Watch,-Properties.Wind_Direction,-X,-Properties.ActualPoints,-Properties.Player_Name,-ActualPoints,-FirstPosition,-Position,-Positions,-Properties.InjuryStatus,-Properties.Position)
QBX <- QB %>% select(-Properties.p_own,-Properties.Watch,-Properties.Wind_Direction,-X,-Properties.ActualPoints,-Properties.Player_Name,-ActualPoints,-FirstPosition,-Position,-Positions,-Properties.InjuryStatus,-Properties.Position)

QBY <- QB$ActualPoints
QBX <- na.zero(QBX)
QBglm <- glm(QBY~.,data=QBX)
QBR2 <- 1-QBglm$deviance/QBglm$null.deviance
QBR2
QBP <- predict(QBglm,QBModel)
naQB <- QBModel$Score
QBFile <- data.frame(QBModel$Properties.Player_Name,QBP,QBModel$Properties.Position,QBModel$Properties.Salary,QBModel$Properties.Salary/QBP,QBModel$ActualPoints,QBModel$Properties.Salary/QBModel$ActualPoints,nfl_QBOwnP,naQB,QBModel$Properties.ProjPlusMinus,QBModel$Properties.OppPlusMinus,QBModel$TeamName)
write.csv(QBFile, file = paste0("QBP",week,site,".csv"))
QBFile

#______________________________RB
foreach(i=1:17) %dopar% DFSLabs::getNflLabModel("2015",modelSite=site,modelWeek=i,mod=labsModel,PositionID="RB")
foreach(i=1:19) %dopar% DFSLabs::getNflLabModel("2016",modelSite=site,modelWeek=i,mod=labsModel,PositionID="RB")

readRB16 <- foreach(i=1:19) %dopar% DFSLabs::readNFLCSVs("2016",modelSite=site,modelWeek=i,modelId=labsModel,PositionID = "RB")
readRB15 <- foreach(i=1:17) %dopar% DFSLabs::readNFLCSVs("2015",modelSite=site,modelWeek=i,modelId=labsModel,PositionID = "RB")
RB <- bind_rows(readRB15,readRB16)
#RB <- bind_rows(readRB16)

RBModel <- DFSLabs::getNflLabModel("2016",modelSite=site,modelWeek=week,PositionID = "RB")
RBModel <- na.zero(DFSLabs::readNFLCSVs("2016",modelSite=site,modelWeek=week,PositionID = "RB"))
nfl_RBOwnP <- data.frame(c(RBModel$Properties.OwnRank))

RBX <- RB %>% select(-Properties.Watch,-Properties.Wind_Direction,-X,-Properties.ActualPoints,-Properties.Player_Name,-ActualPoints,-FirstPosition,-Position,-Positions,-Properties.InjuryStatus,-Properties.Position,-Properties.p_own)
#RBX <- RB %>% select(-Properties.p_own,-Properties.Watch,-ExposureProbability,-IsExposureLocked,-Properties.Wind_Direction,-X,-Properties.ActualPoints,-Properties.Player_Name,-ActualPoints,-FirstPosition,-Position,-Positions,-Properties.InjuryStatus,-Properties.Position)

RBY <- RB$ActualPoints
RBX <- na.zero(RBX)
RBglm <- glm(RBY~.,data=RBX)
RBR2 <- 1-RBglm$deviance/RBglm$null.deviance
RBR2
RBP <- predict(RBglm,RBModel)
naRB <- RBModel$Score

RBFile <- data.frame(RBModel$Properties.Player_Name,RBP,RBModel$Properties.Position,RBModel$Properties.Salary,RBModel$Properties.Salary/RBP,RBModel$ActualPoints,RBModel$Properties.Salary/RBModel$ActualPoints,nfl_RBOwnP,naRB,RBModel$Properties.ProjPlusMinus,RBModel$Properties.OppPlusMinus,RBModel$TeamName)
write.csv(RBFile, file = paste0("RBP",week,site,".csv"))
RBFile

#_________________________________WR
foreach(i=1:17) %dopar% DFSLabs::getNflLabModel("2015",modelSite=site,modelWeek=i,mod=labsModel,PositionID="WR")
foreach(i=1:19) %dopar% DFSLabs::getNflLabModel("2016",modelSite=site,modelWeek=i,mod=labsModel,PositionID="WR")

readWR16 <- foreach(i=1:19) %dopar% DFSLabs::readNFLCSVs("2016",modelSite=site,modelWeek=i,modelId=labsModel,PositionID = "WR")
readWR15 <- foreach(i=1:17) %dopar% DFSLabs::readNFLCSVs("2015",modelSite=site,modelWeek=i,modelId=labsModel,PositionID = "WR")
WR <- bind_rows(readWR15,readWR16)
#WR <- bind_rows(readWR16)

WRModel <- DFSLabs::getNflLabModel("2016",modelSite=site,modelWeek=week,PositionID = "WR")
WRModel <- na.zero(DFSLabs::readNFLCSVs("2016",modelSite=site,modelWeek=week,PositionID = "WR"))
nfl_WROwnP <- data.frame(c(WRModel$Properties.OwnRank))

WRX <- WR %>% select(-Properties.Watch,-Properties.Wind_Direction,-X,-Properties.ActualPoints,-Properties.Player_Name,-ActualPoints,-FirstPosition,-Position,-Positions,-Properties.InjuryStatus,-Properties.Position)

#WRX <- WR %>% select(-Properties.Watch,-ExposureProbability,-IsExposureLocked,-Properties.Wind_Direction,-X,-Properties.ActualPoints,-Properties.Player_Name,-ActualPoints,-FirstPosition,-Position,-Positions,-Properties.InjuryStatus,-Properties.Position,-Properties.p_own)
WRY <- WR$ActualPoints
WRX <- na.zero(WRX)
WRglm <- glm(WRY~.,data=WRX)
WRR2 <- 1-WRglm$deviance/WRglm$null.deviance
WRR2

WRP <- predict(WRglm,WRModel)
naWR <- WRModel$Score

WRFile <- data.frame(WRModel$Properties.Player_Name,WRP,WRModel$Properties.Position,WRModel$Properties.Salary,WRModel$Properties.Salary/WRP,WRModel$ActualPoints,WRModel$Properties.Salary/WRModel$ActualPoints,nfl_WROwnP,naWR,WRModel$Properties.ProjPlusMinus,WRModel$Properties.OppPlusMinus,WRModel$TeamName)
write.csv(WRFile, file = paste0("WRP",week,site,".csv"))
WRFile

#_____________________________TE
foreach(i=1:17) %dopar% DFSLabs::getNflLabModel("2015",modelSite=site,modelWeek=i,mod="",PositionID="TE")
foreach(i=1:19) %dopar% DFSLabs::getNflLabModel("2016",modelSite=site,modelWeek=i,mod="",PositionID="TE")

readTE16 <- foreach(i=1:19) %dopar% DFSLabs::readNFLCSVs("2016",modelSite=site,modelWeek=i,modelId="",PositionID = "TE")
readTE15 <- foreach(i=1:17) %dopar% DFSLabs::readNFLCSVs("2015",modelSite=site,modelWeek=i,modelId="",PositionID = "TE")
TE <- bind_rows(readTE15,readTE16)
#TE <- bind_rows(readTE16)

TEModel <- DFSLabs::getNflLabModel("2016",modelSite=site,modelWeek=week,PositionID = "TE")
TEModel <- na.zero(DFSLabs::readNFLCSVs("2016",modelSite=site,modelWeek=week,PositionID = "TE"))
nfl_TEOwnP <- data.frame(c(TEModel$Properties.OwnRank))

TEX <- TE %>% select(-Properties.p_own,-Properties.Watch,-Properties.Wind_Direction,-X,-Properties.ActualPoints,-Properties.Player_Name,-ActualPoints,-FirstPosition,-Position,-Positions,-Properties.InjuryStatus,-Properties.Position)

#TEX <- TE %>% select(-Properties.p_own,-Properties.Watch,-ExposureProbability,-IsExposureLocked,-Properties.Wind_Direction,-X,-Properties.ActualPoints,-Properties.Player_Name,-ActualPoints,-FirstPosition,-Position,-Positions,-Properties.InjuryStatus,-Properties.Position)
TEY <- TE$ActualPoints
TEX <- na.zero(TEX)
TEglm <- glm(TEY~.,data=TEX)
TER2 <- 1-TEglm$deviance/TEglm$null.deviance
TER2
TEP <- predict(TEglm,TEModel)
naTE <- TEModel$Score

TEFile <- data.frame(TEModel$Properties.Player_Name,TEP,TEModel$Properties.Position,TEModel$Properties.Salary,TEModel$Properties.Salary/TEP,TEModel$ActualPoints,TEModel$Properties.Salary/TEModel$ActualPoints,nfl_TEOwnP,naTE,TEModel$Properties.ProjPlusMinus,TEModel$Properties.OppPlusMinus,TEModel$TeamName)
write.csv(TEFile, file = paste0("TEP",week,site,".csv"))
TEFile

#_________________________________________DST
foreach(i=1:17) %dopar% DFSLabs::getNflLabModel("2015",modelSite=site,modelWeek=i,mod=labsModel,PositionID = "DST")
foreach(i=1:19) %dopar% DFSLabs::getNflLabModel("2016",modelSite=site,modelWeek=i,mod=labsModel,PositionID = "DST")

readDST16 <- foreach(i=1:19) %dopar% DFSLabs::readNFLCSVs("2016",modelSite=site,modelWeek=i,modelId=labsModel,PositionID = "DST")
readDST15 <- foreach(i=1:17) %dopar% DFSLabs::readNFLCSVs("2015",modelSite=site,modelWeek=i,modelId=labsModel,PositionID = "DST")
DST <- bind_rows(readDST15,readDST16)
#DST <- bind_rows(readDST16)

DSTModel <- DFSLabs::getNflLabModel("2016",modelSite=site,modelWeek=week,PositionID = "DST")
DSTModel <- na.zero(DFSLabs::readNFLCSVs("2016",modelSite=site,modelWeek=week,PositionID = "DST"))
DSTX <- DST %>% select(-Properties.Watch,-Properties.Wind_Direction,-X,-Properties.ActualPoints,-ActualPoints,-FirstPosition,-Position,-Positions,-Properties.InjuryStatus,-Properties.Position)
#DSTX <- DST %>% select(-Properties.p_own,-Properties.Watch,-ExposureProbability,-IsExposureLocked,-Properties.Wind_Direction,-X,-Properties.ActualPoints,-Properties.Player_Name,-ActualPoints,-FirstPosition,-Position,-Positions,-Properties.InjuryStatus,-Properties.Position)

DSTY <- DST$ActualPoints
DSTX <- na.zero(DSTX)
DSTglm <- glm(DSTY~.,data=DSTX)
DSTR2 <- 1-DSTglm$deviance/DSTglm$null.deviance
DSTR2
DSTP <- predict(DSTglm,DSTModel)
naDST <- DSTModel$Score

#DSTFile <- data.frame(DSTModel$Properties.Player_Name,DSTP)
#write.csv(DSTFile, file = paste0("DSTP",week,site,".csv"))
#DSTFile


#afterDSTONLY
nfl_DSTOwnP <- data.frame(c(DSTModel$Properties.OwnRank))
DSTFile <- data.frame(DSTModel$Properties.Player_Name,DSTP,DSTModel$Properties.Position,DSTModel$Properties.Salary,DSTModel$Properties.Salary/DSTP,DSTModel$ActualPoints,DSTModel$Properties.Salary/DSTModel$ActualPoints,nfl_DSTOwnP,naDST,DSTModel$Properties.ProjPlusMinus,DSTModel$Properties.OppPlusMinus,DSTModel$TeamName)
DSTFile <- DSTFile %>% dplyr::arrange(DSTModel.Properties.Player_Name)


names(DSTFile)<-c("V1","DSTP","Position","Salary","Proj$/Pt.","ActualPoints","Actual$/Pt.","OwnershipPred","NA","ProjPlusMinus","OppPlusMinus","Team")
V1 <- c("Arizona Defense","Atlanta Defense","Baltimore Defense","Buffalo Defense","Carolina Defense","Chicago Defense","Cincinnati Defense","Cleveland Defense","Dallas Defense","Denver Defense","Detroit Defense","Green Bay Defense","Houston Defense","Jacksonville Defense","Kansas City Defense","Indianapolis Defense","LA Rams Defense","Miami Defense","Minnesota Defense","New England Defense","New Orleans Defense","NY Giants Defense","NY Jets Defense","Oakland Defense","Philadelphia Defense","Pittsburgh Defense","San Diego Defense","San Francisco Defense","Seatle Defense","Tampa Bay Defense","Tennessee Defense","Washington Defense")
cruncherName1 <- c('Cardinals','Falcons','Ravens','Bills','Panthers','Bears','Bengals','Browns','Cowboys','Broncos','Lions','Packers','Texans','Jaguars','Chiefs','Colts','Rams','Dolphins','Vikings','Patriots','Saints','Giants','Jets','Raiders','Eagles','Steelers','Chargers','49ers','Seahawks','Buccaneers','Titans','Redskins')
dstNames <- data.frame(V1,cruncherName1,stringsAsFactors = FALSE)
DSTFile <- inner_join(DSTFile,dstNames,by="V1")
DSTFile <- DSTFile %>% select(cruncherName1,DSTP,Position,Salary,`Proj$/Pt.`,ActualPoints,`Actual$/Pt.`,OwnershipPred,`NA`,ProjPlusMinus,OppPlusMinus,Team)
names(DSTFile)=list("V1","PP","Position","Salary","Proj$/Pt.","ActualPoints","Actual$/Pt.","OwnershipPred","NA","ProjPlusMinus","OppPlusMinus","Team")
write.csv(DSTFile, file = paste0("DSTP",week,site,".csv"))
DSTFile

#CombineAllProjections
QBFile<- data.frame(QBFile)
RBFile<- data.frame(RBFile)
WRFile<- data.frame(WRFile)
TEFile<- data.frame(TEFile)
DSTFile<- data.frame(DSTFile)
names(QBFile)<-list("V1","PP","Position","Salary","Proj$/Pt.","ActualPoints","Actual$/Pt.","OwnershipPred","NA","ProjPlusMinus","OppPlusMinus","Team")
names(RBFile)<-list("V1","PP","Position","Salary","Proj$/Pt.","ActualPoints","Actual$/Pt.","OwnershipPred","NA","ProjPlusMinus","OppPlusMinus","Team")
names(WRFile)<-list("V1","PP","Position","Salary","Proj$/Pt.","ActualPoints","Actual$/Pt.","OwnershipPred","NA","ProjPlusMinus","OppPlusMinus","Team")
names(TEFile)<-list("V1","PP","Position","Salary","Proj$/Pt.","ActualPoints","Actual$/Pt.","OwnershipPred","NA","ProjPlusMinus","OppPlusMinus","Team")
names(DSTFile)<-list("V1","PP","Position","Salary","Proj$/Pt.","ActualPoints","Actual$/Pt.","OwnershipPred","NA","ProjPlusMinus","OppPlusMinus","Team")

AllProj <- bind_rows(QBFile,RBFile,WRFile,TEFile,DSTFile)
AllProj <- AllProj %>% filter(`Proj$/Pt.`!="NA")
AllProj <- AllProj %>% filter(PP!="NA")
AllProj <- AllProj %>% filter(PP>0)
AllProj$`Actual$/Pt.` <- as.character(AllProj$`Actual$/Pt.`)

AllProj$`Actual$/Pt.`[AllProj$`Actual$/Pt.` == "Inf"] <- "10000"
AllProj$`Actual$/Pt.`[AllProj$`Actual$/Pt.` < 0] <- "10000"


write.csv(AllProj, file = paste0("~/Desktop/NFL_Daily/NFL_2016_Week",week,"/AllProjections",week,site,".csv"))

