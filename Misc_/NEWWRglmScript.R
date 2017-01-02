foreach(i=1:16) %dopar% getNflLabModel("2016",i,PositionID="WR")
readWR16 <- foreach(i=1:14) %dopar% readNFLCSVs("2016",i,PositionID = "WR")
WR <- bind_rows(readWR16)
week <- 17
WRModel <- getNflLabModel("2016",week,PositionID = "WR")
WRModel <- na.zero(readNFLCSVs("2016",week,PositionID = "WR"))
nfl_WROwnP <- data.frame(c(WRModel$Properties.OwnRank))

WRX <- WR %>% select(-X,-Properties.ActualPoints,-Properties.Player_Name,-ActualPoints,-FirstPosition,-Position,-Positions,-Properties.InjuryStatus,-Properties.Position)
WRY <- WR$ActualPoints
WRX <- na.zero(WRX)
WRglm <- glm(WRY~.,data=WRX)
WRR2 <- 1-WRglm$deviance/WRglm$null.deviance
WRR2

WRP <- predict(WRglm,WRModel)
naWR <- WRModel$Score

WRFile <- data.frame(WRModel$Properties.Player_Name,WRP,WRModel$Properties.Position,WRModel$Properties.Salary,WRModel$Properties.Salary/WRP,WRModel$ActualPoints,WRModel$Properties.Salary/WRModel$ActualPoints,nfl_WROwnP,naWR,WRModel$Properties.ProjPlusMinus,WRModel$Properties.OppPlusMinus,WRModel$TeamName)
write.csv(WRFile, file = paste0("WRP",week,".csv"))
WRFile

