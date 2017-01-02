foreach(i=1:16) %dopar% getNflLabModel("2016",i,PositionID="QB")
readQB16 <- foreach(i=1:14) %dopar% readNFLCSVs("2016",i,PositionID = "QB")
QB <- bind_rows(readQB16)
week <- 17

QBModel <- getNflLabModel("2016",week,PositionID = "QB")
QBModel <- na.zero(readNFLCSVs("2016",week,PositionID = "QB"))
QBModel <- na.zero(QBModel)
nfl_QBOwnP <- data.frame(c(QBModel$Properties.OwnRank))
QBX <- QB %>% select(-X,-Properties.ActualPoints,-Properties.Player_Name,-ActualPoints,-FirstPosition,-Position,-Positions,-Properties.InjuryStatus,-Properties.Position)
QBY <- QB$ActualPoints
QBX <- na.zero(QBX)
QBglm <- glm(QBY~.,data=QBX)
QBR2 <- 1-QBglm$deviance/QBglm$null.deviance
QBR2
QBP <- predict(QBglm,QBModel)
naQB <- QBModel$Score
QBFile <- data.frame(QBModel$Properties.Player_Name,QBP,QBModel$Properties.Position,QBModel$Properties.Salary,QBModel$Properties.Salary/QBP,QBModel$ActualPoints,QBModel$Properties.Salary/QBModel$ActualPoints,nfl_QBOwnP,naQB,QBModel$Properties.ProjPlusMinus,QBModel$Properties.OppPlusMinus,QBModel$TeamName)
write.csv(QBFile, file = paste0("QBP",week,".csv"))
QBFile

