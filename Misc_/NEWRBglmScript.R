foreach(i=8:17) %dopar% getNflLabModel("2015",i,PositionID="RB")
readRB16 <- foreach(i=1:17) %dopar% readNFLCSVs("2016",i,PositionID = "RB")
readRB15 <- foreach(i=1:17) %dopar% readNFLCSVs("2015",i,PositionID = "RB")
RB <- bind_rows(readRB15,readRB16)
#RB <- bind_rows(readRB16)
week <- 19
RBModel <- getNflLabModel("2016",week,PositionID = "RB")
RBModel <- na.zero(readNFLCSVs("2016",week,PositionID = "RB"))
nfl_RBOwnP <- data.frame(c(RBModel$Properties.OwnRank))

RBX <- RB %>% select(-Properties.Watch,-ExposureProbability,-IsExposureLocked,-Properties.Wind_Direction,-X,-Properties.ActualPoints,-Properties.Player_Name,-ActualPoints,-FirstPosition,-Position,-Positions,-Properties.InjuryStatus,-Properties.Position,-Properties.p_own)
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

