foreach(i=1:16) %dopar% getNflLabModel("2016",i,PositionID="RB")
readRB16 <- foreach(i=1:14) %dopar% readNFLCSVs("2016",i,PositionID = "RB")
RB <- bind_rows(readRB16)
week <- 17
RBModel <- getNflLabModel("2016",week,PositionID = "RB")
RBModel <- na.zero(readNFLCSVs("2016",week,PositionID = "RB"))
nfl_RBOwnP <- data.frame(c(RBModel$Properties.OwnRank))

RBX <- RB %>% select(-X,-Properties.ActualPoints,-Properties.Player_Name,-ActualPoints,-FirstPosition,-Position,-Positions,-Properties.InjuryStatus,-Properties.Position)
RBY <- RB$ActualPoints
RBX <- na.zero(RBX)
RBglm <- glm(RBY~.,data=RBX)
RBR2 <- 1-RBglm$deviance/RBglm$null.deviance
RBR2
RBP <- predict(RBglm,RBModel)
naRB <- RBModel$Score

RBFile <- data.frame(RBModel$Properties.Player_Name,RBP,RBModel$Properties.Position,RBModel$Properties.Salary,RBModel$Properties.Salary/RBP,RBModel$ActualPoints,RBModel$Properties.Salary/RBModel$ActualPoints,nfl_RBOwnP,naRB,RBModel$Properties.ProjPlusMinus,RBModel$Properties.OppPlusMinus,RBModel$TeamName)
write.csv(RBFile, file = paste0("RBP",week,".csv"))
RBFile

