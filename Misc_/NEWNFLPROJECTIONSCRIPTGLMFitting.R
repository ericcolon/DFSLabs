AllDST16<- foreach(i=1:15) %dopar% getNflLabModel("2016",i,PositionID = "DST")
readDST16 <- foreach(i=1:14) %dopar% readNFLCSVs("2016",i,PositionID = "DST")
DST <- bind_rows(readDST16)
week <- 17
DSTModel <- getNflLabModel("2016",week,PositionID = "DST")
DSTModel <- na.zero(readNFLCSVs("2016",week,PositionID = "DST"))
DSTX <- DST %>% select(-X,-Properties.ActualPoints,-ActualPoints,-FirstPosition,-Position,-Positions,-Properties.InjuryStatus,-Properties.Position)
DSTY <- DST$ActualPoints
DSTX <- na.zero(DSTX)
DSTglm <- glm(DSTY~.,data=DSTX)
DSTR2 <- 1-DSTglm$deviance/DSTglm$null.deviance
DSTR2
DSTP <- predict(DSTglm,DSTModel)
naDST <- DSTModel$Score

#DSTFile <- data.frame(DSTModel$Properties.Player_Name,DSTP)
#write.csv(DSTFile, file = paste0("DSTP",week,".csv"))
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
write.csv(DSTFile, file = paste0("DSTP",week,".csv"))
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


write.csv(AllProj, file = paste0("~/Desktop/NFL_Daily/NFL_2016_Week",week,"/AllProjections",week,".csv"))

